// SPDX-License-Identifier: GPL-2.0-only

//! `stg fold` implementation.

use std::{io::Read, path::PathBuf, rc::Rc};

use anyhow::Result;
use clap::{Arg, ArgGroup};

use crate::{
    ext::{CommitExtended, RepositoryExtended},
    patch::SingleRevisionSpec,
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "fold",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Fold diff file into the current patch")
        .long_about(
            "Fold diff file into the current patch. The given GNU diff file (or \
             standard input) is applied onto the current patch.\n\
             \n\
             With the '--threeway' option, the diff is applied onto the bottom of the \
             current patch and a three-way merge is performed with the current top. \
             With the '--base' option, the diff is applied onto the specified base and \
             a three-way merge is performed with the current top.",
        )
        .arg(
            Arg::new("file")
                .help("GNU diff file")
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(clap::ValueHint::FilePath),
        )
        .arg(
            Arg::new("three-way")
                .long("threeway")
                .short('t')
                .help("Perform a three-way merge with the current patch")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("base")
                .long("base")
                .short('b')
                .help("Use <committish> instead of HEAD when applying the patch")
                .value_name("committish")
                .value_parser(clap::value_parser!(SingleRevisionSpec)),
        )
        .group(ArgGroup::new("merge-style").args(["three-way", "base"]))
        .arg(
            Arg::new("strip")
                .long("strip")
                .short('p')
                .help("Remove <n> leading components from diff paths (default 1)")
                .value_name("n")
                .value_parser(crate::argset::parse_usize),
        )
        .arg(
            Arg::new("context-lines")
                .short('C')
                .help("Ensure <n> lines of matching context for each change")
                .value_name("n")
                .value_parser(crate::argset::parse_usize),
        )
        .arg(
            Arg::new("reject")
                .long("reject")
                .help("Leave rejected hunks in \".rej\" files")
                .action(clap::ArgAction::SetTrue),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    let statuses = stupid.statuses(None)?;
    statuses.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;

    if stack.applied().is_empty() {
        return Err(super::Error::NoAppliedPatches.into());
    }

    let diff = if let Some(filename) = matches.get_one::<PathBuf>("file") {
        std::fs::read(filename)?
    } else {
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let mut diff = Vec::new();
        stdin.read_to_end(&mut diff)?;
        diff
    };

    let reject_flag = matches.get_flag("reject");
    let strip_level = matches.get_one::<usize>("strip").copied();
    let context_lines = matches.get_one::<usize>("context-lines").copied();

    let stupid = repo.stupid();

    stupid.update_index_refresh()?;

    let base_commit = if matches.get_flag("three-way") {
        Some(Rc::new(stack.top().get_parent_commit()?))
    } else if let Some(base_spec) = matches.get_one::<SingleRevisionSpec>("base") {
        let rev = base_spec.resolve(&repo, Some(&stack))?;
        Some(rev.commit)
    } else {
        None
    };

    if let Some(base_commit) = base_commit {
        let orig_head_tree_id = stack.get_branch_head().tree_id()?.detach();
        let base_tree_id = base_commit.tree_id()?.detach();
        stupid.read_tree_checkout(orig_head_tree_id, base_tree_id)?;
        if let Err(e) = stupid.apply_to_worktree_and_index(
            &diff,
            reject_flag,
            false,
            strip_level,
            None,
            context_lines,
        ) {
            stupid.read_tree_checkout_hard(orig_head_tree_id)?;
            return Err(e);
        }
        let applied_tree_id = stupid.write_tree()?;
        stupid.read_tree_checkout(applied_tree_id, orig_head_tree_id)?;
        if stupid.merge_recursive(base_tree_id, orig_head_tree_id, applied_tree_id)? {
            Ok(())
        } else {
            Err(super::Error::CausedConflicts("merge conflicts".to_string()).into())
        }
    } else {
        stupid.apply_to_worktree_and_index(
            &diff,
            reject_flag,
            false,
            strip_level,
            None,
            context_lines,
        )
    }
}
