// SPDX-License-Identifier: GPL-2.0-only

//! `stg pick` implementation.

use std::{
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::Arg;

use crate::{
    argset,
    branchloc::BranchLocator,
    color::get_color_stdout,
    ext::{CommitExtended, RepositoryExtended},
    patch::{
        revspec, PatchName, RangeConstraint, RangeRevisionSpec, SingleRevisionSpec, StGitRevision,
    },
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "pick",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Import a patch from another branch or a commit object")
        .long_about(
            "Import one or more patches from another branch or commit object into the \
             current series.\n\
             \n\
             By default, the imported patch's name is reused, but may be overridden \
             with the '--name' option. A commit object can be reverted with the \
             '--revert' option.\n\
             \n\
             When using the '--expose' option, the format of the commit message is \
             determined by the 'stgit.pick.expose-format' configuration option. This \
             option is a format string as may be supplied to the '--pretty' option of \
             'git show'. The default is \"format:%B%n(imported from commit %H)\", \
             which appends the commit hash of the picked commit to the patch's commit \
             message.",
        )
        .override_usage(
            "stg pick [OPTIONS] <source>...\n       \
             stg pick [OPTIONS] [--name NAME] [--parent COMMITTISH] <source>\n       \
             stg pick [OPTIONS] --fold [--file PATH]... <source>...\n       \
             stg pick [OPTIONS] --update <source>...",
        )
        .arg(
            Arg::new("stgit-revision")
                .help("Patch name or committish to import")
                .value_name("source")
                .required(true)
                .num_args(1..)
                .value_parser(clap::value_parser!(RangeRevisionSpec)),
        )
        .arg(
            Arg::new("ref-branch")
                .long("ref-branch")
                .short('B')
                .help("Pick patches from <branch>")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
        .arg(
            Arg::new("revert")
                .long("revert")
                .short('r')
                .help("Revert the given commit object")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("expose"),
        )
        .arg(
            Arg::new("expose")
                .long("expose")
                .short('x')
                .help("Append the imported commit id to the patch log")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["fold", "update"]),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Keep the imported patch unapplied")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["fold", "update"]),
        )
        .arg(
            Arg::new("name")
                .long("name")
                .short('n')
                .help("Use <name> for the patch name")
                .value_name("name")
                .value_parser(clap::value_parser!(PatchName))
                .conflicts_with_all(["fold", "update"]),
        )
        .arg(
            Arg::new("parent")
                .long("parent")
                .short('p')
                .help("Use <committish> as parent")
                .value_name("committish")
                .value_parser(clap::value_parser!(SingleRevisionSpec))
                .conflicts_with_all(["fold", "update"]),
        )
        .arg(argset::committer_date_is_author_date_arg())
        .arg(
            Arg::new("fold")
                .long("fold")
                .help("Fold the commit object into the current patch")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("update")
                .long("update")
                .help("Like fold but only update the current patch's files")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("fold"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("Only fold the given file (may be used multiple times)")
                .value_parser(clap::value_parser!(PathBuf))
                .action(clap::ArgAction::Append)
                .value_name("path")
                .requires("fold"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AutoInitialize)?;
    let ref_stack = Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("ref-branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    if matches.get_flag("update") && stack.applied().is_empty() {
        return Err(super::Error::NoAppliedPatches.into());
    }

    if !matches.get_flag("noapply") {
        repo.stupid()
            .statuses(None)?
            .check_index_and_worktree_clean()?;
        stack.check_head_top_mismatch()?;
    }

    let picks = revspec::resolve(
        &repo,
        Some(&ref_stack),
        matches
            .get_many::<RangeRevisionSpec>("stgit-revision")
            .expect("required argument"),
        RangeConstraint::VisibleWithAppliedBoundary,
    )?;

    if matches.get_flag("fold") || matches.get_flag("update") {
        // Fold into current patch
        fold_picks(&stack, matches, &picks)
    } else {
        // Pick new patches from sources
        if picks.len() > 1 {
            if matches.contains_id("name") {
                return Err(anyhow!("--name can only be specified with one patch"));
            }
            if matches.contains_id("parent") {
                return Err(anyhow!("--parent can only be specified with one patch"));
            }
        }
        let opt_parent = matches
            .get_one::<SingleRevisionSpec>("parent")
            .map(|parent_spec| {
                parent_spec
                    .resolve(&repo, Some(&ref_stack))
                    .map(|rev| rev.commit)
            })
            .transpose()?;
        pick_picks(stack, matches, opt_parent, &picks)
    }
}

fn fold_picks(stack: &Stack, matches: &clap::ArgMatches, picks: &[StGitRevision]) -> Result<()> {
    let stupid = stack.repo.stupid();
    for StGitRevision { patchname, commit } in picks {
        let parent = commit.get_parent_commit()?.into();
        let (top, bottom) = if matches.get_flag("revert") {
            (&parent, commit)
        } else {
            (commit, &parent)
        };

        let diff_files;

        let pathspecs: Option<Vec<&Path>> = if matches.get_flag("fold") {
            matches
                .get_many::<PathBuf>("file")
                .map(|pathbufs| pathbufs.map(PathBuf::as_path).collect())
        } else {
            assert!(matches.get_flag("update"));
            let branch_head = stack.get_branch_head();
            diff_files = stupid.diff_tree_files(
                branch_head.get_parent_commit()?.tree_id()?.detach(),
                branch_head.tree_id()?.detach(),
            )?;
            Some(diff_files.iter().collect())
        };

        let conflicts = !stupid
            .apply_treediff_to_worktree_and_index(
                bottom.tree_id()?.detach(),
                top.tree_id()?.detach(),
                pathspecs,
            )
            .with_context(|| {
                if let Some(patchname) = patchname {
                    format!("folding `{patchname}`")
                } else {
                    format!("folding `{}`", commit.id)
                }
            })?;

        if conflicts {
            return Err(
                super::Error::CausedConflicts(if let Some(patchname) = patchname {
                    format!("`{patchname}` does not apply cleanly")
                } else {
                    format!("`{}` does not apply cleanly", commit.id)
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
    opt_parent: Option<Rc<gix::Commit>>,
    picks: &[StGitRevision],
) -> Result<()> {
    let stupid = stack.repo.stupid();
    let config = stack.repo.config_snapshot();
    let patchname_len_limit = PatchName::get_length_limit(&config);
    let mut new_patches: Vec<(PatchName, gix::ObjectId)> = Vec::with_capacity(picks.len());

    for StGitRevision { patchname, commit } in picks {
        let commit_ref = commit.decode()?;
        let mut disallow: Vec<&PatchName> = stack.all_patches().collect();

        let patchname = if let Some(name) = matches.get_one::<PatchName>("name") {
            name.clone()
        } else if let Some(patchname) = patchname {
            if matches.get_flag("revert") {
                PatchName::from_str(&format!("revert-{patchname}"))?
            } else {
                patchname.clone()
            }
        } else {
            PatchName::make(
                &commit_ref.message.to_str_lossy(),
                false,
                patchname_len_limit,
            )
        }
        .uniquify(&[], &disallow);

        let commit_id_string = commit.id.to_string();
        let message = if matches.get_flag("revert") {
            let message = commit_ref.message.to_str().ok();
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
        } else if matches.get_flag("expose") {
            let expose_format =
                config
                    .plumbing()
                    .string("stgit", Some("pick".into()), "expose-format");
            let expose_format = expose_format
                .as_ref()
                .map(|bs| bs.to_str().ok())
                .unwrap_or(None)
                .unwrap_or("format:%B%n(imported from commit %H)%n");
            stupid
                .show_pretty(commit.id, expose_format)?
                .to_str_lossy()
                .to_string()
        } else {
            commit_ref.message.to_str_lossy().to_string()
        };
        let message = &crate::wrap::Message::String(message);
        let author = commit.author_strict()?;
        let default_committer = stack.repo.get_committer()?;
        let committer = if matches.get_flag("committer-date-is-author-date") {
            let mut committer = default_committer.to_owned();
            committer.time = author.time;
            committer
        } else {
            default_committer.to_owned()
        };
        let parent = if let Some(parent) = opt_parent.as_ref() {
            parent.clone()
        } else {
            Rc::new(commit.get_parent_commit()?)
        };

        let (top, bottom) = if matches.get_flag("revert") {
            (parent, commit.clone())
        } else {
            (commit.clone(), parent)
        };
        let new_commit_id = stack.repo.commit_ex(
            &author,
            &committer,
            message,
            top.tree_id()?.detach(),
            [bottom.id],
        )?;
        new_patches.push((patchname, new_commit_id));
        disallow.push(&new_patches[new_patches.len() - 1].0);
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
            if !matches.get_flag("noapply") {
                trans.push_patches(&to_push, false)?;
            }
            Ok(())
        })
        .execute("pick")?;
    Ok(())
}
