use clap::{App, Arg, ArgMatches};

use crate::error::Error;
use crate::stack::Stack;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("show", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("show")
        .about("Show patch commits")
        // .override_usage("xxx")
        .long_about(
            "Show the commit log and diff corresponding to the given patches. \
             The topmost patch is shown by default, or HEAD if no patches are \
             applied.\n\
             The output is similar to 'git show'.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Show the applied patches"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Show the unapplied patches"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Show the hidden patches"),
        )
        .arg(
            Arg::new("stat")
                .long("stat")
                .short('s')
                .help("Show a diffstat summary instead of the full diff"),
        )
        .arg(&*crate::argset::DIFF_OPTS_ARG)
        .arg(
            Arg::new("patch_revs")
                .help("Patch or revision to show")
                .long_help(
                    "Patch or revisions to show.\n\
                     \n\
                     A patch name, patch range of the form \
                     '[begin-patch]..[end-patch]', or any valid Git revision \
                     may be specified.",
                )
                .value_name("patch-rev")
                .multiple_values(true)
                .conflicts_with_all(&["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("path_limits")
                .help("Limit diff to files matching path")
                .value_name("path")
                .last(true)
                .multiple_values(true)
                .allow_invalid_utf8(true),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    let opt_stat = matches.is_present("stat");
    let opt_applied = matches.is_present("applied");
    let opt_unapplied = matches.is_present("unapplied");
    let opt_hidden = matches.is_present("hidden");
    let opt_patch_revs = matches.values_of("patch_revs");

    let mut oids: Vec<git2::Oid> = Vec::new();

    if opt_applied {
        for patchname in &stack.state.applied {
            oids.push(stack.state.patches[patchname].commit.id());
        }
    }
    if opt_unapplied {
        for patchname in &stack.state.unapplied {
            oids.push(stack.state.patches[patchname].commit.id());
        }
    }
    if opt_hidden {
        for patchname in &stack.state.hidden {
            oids.push(stack.state.patches[patchname].commit.id());
        }
    }
    if let Some(patch_rev) = opt_patch_revs {
        for patch_rev in patch_rev {
            match crate::patchrange::parse_patch_ranges(
                [patch_rev],
                stack.all_patches(),
                stack.all_patches(),
            ) {
                Ok(patchnames) => {
                    for patchname in &patchnames {
                        oids.push(stack.state.patches[patchname].commit.id())
                    }
                }
                Err(crate::patchrange::Error::PatchNotKnown { patchname: _ }) => {
                    let oid =
                        crate::revspec::parse_stgit_revision(&repo, Some(patch_rev), opt_branch)
                            .map_err(|rev_err| match rev_err {
                                Error::InvalidRevision(spec) => {
                                    Error::Generic(format!("invalid revision spec `{}`", spec))
                                }
                                Error::RevisionNotFound(spec) => Error::Generic(format!(
                                    "patch or revision `{}` not found",
                                    spec
                                )),
                                _ => rev_err,
                            })?;
                    oids.push(oid);
                }
                Err(crate::patchrange::Error::PatchName(_)) => {
                    let oid =
                        crate::revspec::parse_stgit_revision(&repo, Some(patch_rev), opt_branch)
                            .map_err(|rev_err| match rev_err {
                                Error::InvalidRevision(spec) => Error::Generic(format!(
                                    "invalid patch or revision spec `{}`",
                                    spec
                                )),
                                Error::RevisionNotFound(spec) => Error::Generic(format!(
                                    "invalid patch or revision `{}` not found",
                                    spec
                                )),
                                _ => rev_err,
                            })?;
                    oids.push(oid);
                }
                Err(e) => {
                    return Err(e.into());
                }
            }
        }
    } else if !opt_applied && !opt_unapplied && !opt_hidden {
        oids.push(stack.head.id());
    }

    let mut command = std::process::Command::new("git");
    command.arg("show");
    if opt_stat {
        command.arg("--stat").arg("--summary");
    } else {
        command.arg("--patch");
    }

    if let Some(diff_opts) = matches.value_of("diff-opts") {
        for opt in diff_opts.split_ascii_whitespace() {
            command.arg(opt);
        }
    }

    for oid in &oids {
        command.arg(&oid.to_string());
    }

    command.arg("--");

    if let Some(path_limits) = matches.values_of_os("path_limits") {
        command.args(path_limits);
    }

    let status = command.status()?;

    if !status.success() {
        Err(Error::Generic("subordinate `git show` failed".to_string()))
    } else {
        Ok(())
    }
}
