// SPDX-License-Identifier: GPL-2.0-only

//! `stg spill` implementation.

use std::path::PathBuf;

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::{CommitExtended, RepositoryExtended},
    stack::{Error, InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "spill",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Spill changes from the topmost patch")
        .long_about(
            "Spill changes from the topmost patch. Changes are removed from the patch, \
             but remain in the index and worktree.\n\
             \n\
             Spilling a patch may be useful for reselecting the files/hunks to be \
             included in the patch.",
        )
        .arg(
            Arg::new("annotate")
                .long("annotate")
                .short('a')
                .help("Annotate the patch log entry with note")
                .num_args(1..)
                .value_name("note"),
        )
        .arg(
            Arg::new("reset")
                .long("reset")
                .short('r')
                .help("Also reset the index")
                .long_help(
                    "Also reset the index such that the patch's changes only remain \
                     in the worktree. Without this option, the patch's changes will \
                     be in both the index and worktree.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::committer_date_is_author_date_arg())
        .arg(
            Arg::new("pathspecs")
                .help("Only spill files matching path")
                .value_name("path")
                .num_args(1..)
                .value_parser(clap::value_parser!(PathBuf)),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    repo.check_repository_state()?;
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;
    statuses.check_index_clean()?;
    stack.check_head_top_mismatch()?;

    let patchname = stack
        .applied()
        .last()
        .ok_or(Error::NoAppliedPatches)?
        .clone();
    let patch_commit = stack.get_patch_commit(&patchname);
    let patch_commit_ref = patch_commit.decode()?;
    let parent = patch_commit.get_parent_commit()?;
    let parent_commit_ref = parent.decode()?;

    let tree_id = if let Some(pathspecs) = matches.get_many::<PathBuf>("pathspecs") {
        stupid.with_temp_index(|stupid_temp| {
            stupid_temp.read_tree(patch_commit_ref.tree())?;
            stupid_temp.apply_pathlimited_treediff_to_index(
                patch_commit_ref.tree(),
                parent_commit_ref.tree(),
                true,
                pathspecs,
            )?;
            stupid_temp.write_tree()
        })?
    } else {
        parent_commit_ref.tree()
    };

    let author = patch_commit.author_strict()?;
    let default_committer = repo.committer_or_default();
    let committer = if matches.get_flag("committer-date-is-author-date") {
        let mut committer = default_committer.to_owned();
        committer.time = author.time;
        committer
    } else {
        default_committer.to_owned()
    };

    let commit_id = repo.commit_ex(
        &author,
        &committer,
        &patch_commit.message_ex(),
        tree_id,
        patch_commit_ref.parents(),
    )?;

    drop(patch_commit_ref);

    let reflog_msg = if let Some(annotation) = matches.get_one::<String>("annotate") {
        format!("spill {patchname}\n\n{annotation}")
    } else {
        format!("spill {patchname}")
    };

    stack
        .setup_transaction()
        .use_index_and_worktree(false)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.update_patch(&patchname, commit_id))
        .execute(&reflog_msg)?;

    if matches.get_flag("reset") {
        stupid.read_tree(tree_id)?;
    }

    Ok(())
}
