//! `stg pick` implementation.

use std::{path::PathBuf, str::FromStr};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::Arg;

use crate::{
    color::get_color_stdout,
    commit::RepositoryCommitExtended,
    patchname::PatchName,
    patchrange,
    repo::RepositoryExtended,
    revspec::{parse_branch_and_spec, parse_stgit_revision},
    signature::SignatureExtended,
    stack::{Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("pick", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("pick")
        .about("Import a patch from another branch or a commit object")
        .long_about(
            "Import one or more patches from another branch or commit object into the \
             current series.\n\
             \n\
             By default, the imported patch's name is reused, but may be overridden \
             with the --name option. A commit object can be reverted with the --revert \
             option.\n\
             \n\
             When using the --expose option, the format of the commit message is \
             determinded by the 'stgit.pick.expose-format' configuration option. This \
             option is a format string as may be supplied to the --pretty option of \
             'git show'. The default is \"format:%B%n(imported from commit %H)\", \
             which appends the commit hash of the picked commit to the patch's commit \
             message.",
        )
        .override_usage(
            "stg pick [OPTIONS] <source>...\n    \
             stg pick [OPTIONS] [--name NAME] [--parent COMMITTISH] <source>\n    \
             stg pick [OPTIONS] --fold [--file PATH]... <source>...\n    \
             stg pick [OPTIONS] --update <source>...",
        )
        .arg(
            Arg::new("source")
                .help("Patch name or committish to import")
                .required(true)
                .multiple_values(true)
                .forbid_empty_values(true),
        )
        .arg(
            Arg::new("ref-branch")
                .long("ref-branch")
                .short('B')
                .help("Pick patches from BRANCH")
                .value_name("BRANCH"),
        )
        .arg(
            Arg::new("revert")
                .long("revert")
                .short('r')
                .help("Revert the given commit object")
                .conflicts_with("expose"),
        )
        .arg(
            Arg::new("expose")
                .long("expose")
                .short('x')
                .help("Append the imported commit id to the patch log")
                .conflicts_with_all(&["fold", "update"]),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Keep the imported patch unapplied")
                .conflicts_with_all(&["fold", "update"]),
        )
        .arg(
            Arg::new("name")
                .long("name")
                .short('n')
                .help("Use NAME for the patch name")
                .value_name("NAME")
                .validator(PatchName::from_str)
                .forbid_empty_values(true)
                .conflicts_with_all(&["fold", "update"]),
        )
        .arg(
            Arg::new("parent")
                .long("parent")
                .short('p')
                .help("Use COMMITTISH as parent")
                .value_name("COMMITTISH")
                .forbid_empty_values(true)
                .conflicts_with_all(&["fold", "update"]),
        )
        .arg(
            Arg::new("fold")
                .long("fold")
                .help("Fold the commit object into the current patch"),
        )
        .arg(
            Arg::new("update")
                .long("update")
                .help("Like fold but only update the current patch's files")
                .conflicts_with("fold"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("Only fold the given file (may be used multiple times)")
                .forbid_empty_values(true)
                .multiple_occurrences(true)
                .value_name("PATH")
                .requires("fold"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let ref_branchname = matches.value_of("ref-branch");
    let ref_stack = Stack::from_branch(&repo, ref_branchname)?;
    let fold = matches.is_present("fold");
    let update = matches.is_present("update");

    if update && stack.applied().is_empty() {
        return Err(crate::stack::Error::NoAppliedPatches.into());
    }

    if !matches.is_present("noapply") {
        repo.check_index_and_worktree_clean()?;
        stack.check_head_top_mismatch()?;
    }

    let sources: Vec<_> = matches
        .values_of("source")
        .expect("required argument")
        .collect();

    let picks: Vec<(Option<PatchName>, git2::Commit)> = match patchrange::parse(
        sources.iter().copied(),
        &ref_stack,
        patchrange::Allow::VisibleWithAppliedBoundary,
    ) {
        Ok(patchnames) => patchnames
            .iter()
            .map(|pn| (Some(pn.clone()), ref_stack.get_patch_commit(pn).clone()))
            .collect(),
        Err(_) => {
            let mut picks = Vec::new();
            for source in sources {
                let (branchname, patchname) = parse_branch_and_spec(None, Some(source));
                if let Some(branchname) = branchname {
                    if let Some(patchname) = patchname {
                        let ref_stack = Stack::from_branch(&repo, Some(branchname))?;
                        let patchnames = patchrange::parse(
                            [patchname],
                            &ref_stack,
                            patchrange::Allow::VisibleWithAppliedBoundary,
                        )?;
                        for pn in &patchnames {
                            picks.push((Some(pn.clone()), ref_stack.get_patch_commit(pn).clone()));
                        }
                    } else {
                        return Err(
                            crate::revspec::Error::InvalidRevision(source.to_string()).into()
                        );
                    }
                } else {
                    let commit = parse_stgit_revision(&repo, Some(source), ref_branchname)?
                        .peel_to_commit()?;
                    picks.push((None, commit));
                }
            }
            picks
        }
    };

    if fold || update {
        // Fold into current patch
        fold_picks(&stack, matches, &picks)
    } else {
        // Pick new patches from sources
        if picks.len() > 1 {
            if matches.is_present("name") {
                return Err(anyhow!("--name can only be specified with one patch"));
            }
            if matches.is_present("parent") {
                return Err(anyhow!("--parent can only be specified with one patch"));
            }
        }
        let opt_parent = if let Some(parent_committish) = matches.value_of("parent") {
            let commit = parse_stgit_revision(
                stack.repo,
                Some(parent_committish),
                Some(&ref_stack.branch_name),
            )?
            .peel_to_commit()?;
            Some(commit)
        } else {
            None
        };
        pick_picks(stack, matches, opt_parent, &picks)
    }
}

fn fold_picks(
    stack: &Stack,
    matches: &clap::ArgMatches,
    picks: &[(Option<PatchName>, git2::Commit)],
) -> Result<()> {
    let stupid = stack.repo.stupid();
    for (patchname, commit) in picks {
        let parent = commit.parent(0)?;
        let (top, bottom) = if matches.is_present("revert") {
            (&parent, commit)
        } else {
            (commit, &parent)
        };

        let pathspecs: Option<Vec<PathBuf>> = if matches.is_present("fold") {
            matches
                .values_of("file")
                .map(|values| values.map(PathBuf::from).collect())
        } else {
            assert!(matches.is_present("update"));
            Some(stack.repo.diff_tree_files(&stack.branch_head)?)
        };

        let conflicts = !stupid
            .apply_treediff_to_worktree_and_index(bottom.tree_id(), top.tree_id(), pathspecs)
            .with_context(|| {
                if let Some(patchname) = patchname {
                    format!("folding `{patchname}`")
                } else {
                    format!("folding `{}`", commit.id())
                }
            })?;

        if conflicts {
            return Err(
                crate::stack::Error::CausedConflicts(if let Some(patchname) = patchname {
                    format!("`{patchname}` does not apply cleanly")
                } else {
                    format!("`{}` does not apply cleanly", commit.id())
                })
                .into(),
            );
        }
    }
    Ok(())
}

fn pick_picks(
    stack: Stack,
    matches: &clap::ArgMatches,
    opt_parent: Option<git2::Commit>,
    picks: &[(Option<PatchName>, git2::Commit)],
) -> Result<()> {
    let stupid = stack.repo.stupid();
    let config = stack.repo.config()?;
    let patchname_len_limit = PatchName::get_length_limit(&config);
    let mut new_patches: Vec<(PatchName, git2::Oid)> = Vec::with_capacity(picks.len());

    for (patchname, commit) in picks {
        let disallow: Vec<&PatchName> = stack.all_patches().collect();

        let patchname = if let Some(name) = matches.value_of("name") {
            PatchName::from_str(name)?
        } else if let Some(patchname) = patchname {
            if matches.is_present("revert") {
                PatchName::from_str(&format!("revert-{patchname}"))?
            } else {
                patchname.clone()
            }
        } else {
            PatchName::make(
                &commit.message_raw_bytes().to_str_lossy(),
                false,
                patchname_len_limit,
            )
        }
        .uniquify(&[], &disallow);

        let commit_id_string = commit.id().to_string();
        let message = if matches.is_present("revert") {
            let message = commit.message();
            let (subject, body) = if let Some(message) = message {
                message.split_once('\n').unwrap_or((message, ""))
            } else {
                (commit_id_string.as_str(), "")
            };
            format!(
                "Revert \"{subject}\"\n\
                 \n\
                 This reverts commit {commit_id_string}.\n\
                 \n\
                 {body}"
            )
        } else if matches.is_present("expose") {
            let expose_format = config
                .get_string("stgit.pick.expose-format")
                .unwrap_or_else(|_| "format:%B%n(imported from commit %H)%n".to_string());
            stupid
                .show_pretty(commit.id(), &expose_format)?
                .to_str_lossy()
                .to_string()
        } else {
            commit.message_raw_bytes().to_str_lossy().to_string()
        };
        let message = &crate::commit::CommitMessage::String(message);
        let author = commit.author();
        let committer = git2::Signature::default_committer(Some(&config))?;
        let parent = if let Some(parent) = opt_parent.as_ref() {
            parent.clone()
        } else {
            commit.parent(0)?
        };

        let (top, bottom) = if matches.is_present("revert") {
            (&parent, commit)
        } else {
            (commit, &parent)
        };
        let new_commit_id =
            stack
                .repo
                .commit_ex(&author, &committer, message, top.tree_id(), [bottom.id()])?;
        new_patches.push((patchname, new_commit_id));
    }

    stack
        .setup_transaction()
        .with_output_stream(get_color_stdout(matches))
        .use_index_and_worktree(true)
        .transact(|trans| {
            let mut to_push = Vec::new();
            for (i, (patchname, commit_id)) in new_patches.iter().enumerate() {
                trans.new_unapplied(patchname, *commit_id, i)?;
                to_push.push(patchname);
            }
            if !matches.is_present("noapply") {
                trans.push_patches(&to_push, false)?;
            }
            Ok(())
        })
        .execute("pick")?;
    Ok(())
}
