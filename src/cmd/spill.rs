// SPDX-License-Identifier: GPL-2.0-only

//! `stg spill` implementation.

use std::path::PathBuf;

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    commit::{CommitExtended, RepositoryCommitExtended},
    repo::RepositoryExtended,
    stack::{Error, Stack, StackStateAccess},
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
        .arg(
            Arg::new("pathspecs")
                .help("Only spill files matching path")
                .value_name("path")
                .num_args(1..)
                .value_parser(clap::value_parser!(PathBuf)),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
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
    let parent = patch_commit.parent(0)?;

    let tree_id = if let Some(pathspecs) = matches.get_many::<PathBuf>("pathspecs") {
        stupid.with_temp_index(|stupid_temp| {
            stupid_temp.read_tree(patch_commit.tree_id())?;
            stupid_temp.apply_pathlimited_treediff_to_index(
                patch_commit.tree_id(),
                parent.tree_id(),
                pathspecs,
            )?;
            stupid_temp.write_tree()
        })?
    } else {
        parent.tree_id()
    };

    let commit_id = repo.commit_ex(
        &patch_commit.author_strict()?,
        &patch_commit.committer_strict()?,
        &patch_commit.message_ex(),
        tree_id,
        patch_commit.parent_ids(),
    )?;

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
