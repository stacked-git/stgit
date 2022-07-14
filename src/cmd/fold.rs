// SPDX-License-Identifier: GPL-2.0-only

//! `stg fold` implementation.

use std::io::Read;

use anyhow::Result;
use clap::{Arg, ArgGroup};

use crate::repo::RepositoryExtended;
use crate::revspec::parse_stgit_revision;
use crate::stack::{Error, Stack, StackStateAccess};
use crate::stupid::Stupid;

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    (
        "fold",
        StGitCommand {
            make,
            run,
            category: super::CommandCategory::PatchManipulation,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("fold")
        .about("Fold diff file into the current patch")
        .long_about(
            "Fold diff file into the current patch. The given GNU diff file (or standard input) \
             is applied onto the current patch.\n\
             \n\
             With the --threeway option, the diff is applied onto the bottom of the current patch \
             and a three-way merge is performed with the current top. With the --base option, the \
             diff is applied onto the specified base and a three-way merge is performed with the \
             current top.",
        )
        .arg(
            Arg::new("file")
                .help("GNU diff file")
                .allow_invalid_utf8(true),
        )
        .arg(
            Arg::new("three-way")
                .long("threeway")
                .short('t')
                .help("Perform a three-way merge with the current patch"),
        )
        .arg(
            Arg::new("base")
                .long("base")
                .short('b')
                .help("Use BASE instead of HEAD when applying the patch")
                .value_name("BASE"),
        )
        .group(ArgGroup::new("merge-style").args(&["three-way", "base"]))
        .arg(
            Arg::new("strip")
                .long("strip")
                .short('p')
                .help("Remove N leading components from diff paths (default 1)")
                .value_name("N")
                .validator(|s| {
                    s.parse::<usize>()
                        .map_err(|_| format!("'{s}' is not an unsigned integer"))
                }),
        )
        .arg(
            Arg::new("context-lines")
                .short('C')
                .help("Ensure N lines of matching context for each change")
                .value_name("N")
                .validator(|s| {
                    s.parse::<usize>()
                        .map_err(|_| format!("'{s}' is not an unsigned integer"))
                }),
        )
        .arg(
            Arg::new("reject")
                .long("reject")
                .help("Leave rejected hunks in \".rej\" files"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    repo.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;

    if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let diff = if let Some(filename) = matches.value_of_os("file") {
        std::fs::read(filename)?
    } else {
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let mut diff = Vec::new();
        stdin.read_to_end(&mut diff)?;
        diff
    };

    let reject = matches.is_present("reject");
    let strip_level = matches
        .value_of("strip")
        .map(|s| s.parse::<usize>().expect("clap already validated"));

    let context_lines = matches
        .value_of("context-lines")
        .map(|s| s.parse::<usize>().unwrap());

    let stupid = repo.stupid();

    stupid.update_index_refresh()?;

    let base_commit = if matches.is_present("three-way") {
        Some(stack.top().parent(0)?)
    } else if let Some(base_spec) = matches.value_of("base") {
        Some(parse_stgit_revision(&repo, Some(base_spec), None)?.peel_to_commit()?)
    } else {
        None
    };

    if let Some(base_commit) = base_commit {
        let orig_head_tree_id = stack.branch_head.tree_id();
        let base_tree_id = base_commit.tree_id();
        stupid.read_tree_checkout(orig_head_tree_id, base_tree_id)?;
        if let Err(e) =
            stupid.apply_to_worktree_and_index(&diff, reject, strip_level, context_lines)
        {
            stupid.read_tree_checkout_hard(orig_head_tree_id)?;
            return Err(e);
        }
        let applied_tree_id = stupid.write_tree()?;
        stupid.read_tree_checkout(applied_tree_id, orig_head_tree_id)?;
        if stupid.merge_recursive(base_tree_id, orig_head_tree_id, applied_tree_id)? {
            Ok(())
        } else {
            Err(crate::stack::Error::CausedConflicts("Merge conflicts".to_string()).into())
        }
    } else {
        stupid.apply_to_worktree_and_index(&diff, reject, strip_level, context_lines)
    }
}
