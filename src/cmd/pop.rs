// SPDX-License-Identifier: GPL-2.0-only

//! `stg pop` implementation.

use std::iter::FromIterator;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patchname::PatchName,
    patchrange,
    stack::{Error, InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "pop",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Pop (unapply) one or more applied patches")
        .long_about(
            "Pop (unapply) one or more applied patches.\n\
             \n\
             By default, the topmost applied patch is popped.\n\
             \n\
             If ranges of patches are specified, pop and push operations are \
             performed such that only the patches specified on the command line \
             are unapplied at the end of the operation. It is possible for some \
             of these intermediate push operations to fail due to conflicts if \
             patches are popped out of last-pushed first-popped order.",
        )
        .override_usage(
            "stg pop [OPTIONS] [patch]...\n       \
             stg pop [OPTIONS] --all\n       \
             stg pop [OPTIONS] -n <number>",
        )
        .arg(
            Arg::new("patchranges-applied")
                .help("Patches to pop")
                .value_name("patch")
                .num_args(1..)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with_all(["all", "number"]),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Pop all applied patches")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("number"),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Pop specified <number> of patches")
                .long_help(
                    "Pop the specified <number> of patches.\n\
                     \n\
                     A negative number indicates to pop all but that number \
                     of patches",
                )
                .num_args(1)
                .allow_hyphen_values(true) // i.e. for negative ints
                .value_name("number")
                .value_parser(clap::value_parser!(isize)),
        )
        .arg(
            Arg::new("spill")
                .long("spill")
                .short('s')
                .help("Keep patches' modifications in working tree after popping")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::keep_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;

    let opt_number = matches.get_one::<isize>("number").copied();

    if Some(0) == opt_number {
        return Ok(());
    }

    if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let mut patches: indexmap::IndexSet<PatchName> = if matches.get_flag("all") {
        stack.applied().iter().cloned().collect()
    } else if let Some(number) = opt_number {
        let num_applied = stack.applied().len();
        let num_to_take: usize = {
            if number >= 0 {
                std::cmp::min(number as usize, num_applied)
            } else if number.unsigned_abs() < num_applied {
                num_applied - number.unsigned_abs()
            } else {
                // User asked to retain more patches than are applied, so nothing to do.
                return Ok(());
            }
        };
        stack
            .applied()
            .iter()
            .rev()
            .take(num_to_take)
            .cloned()
            .collect()
    } else if let Some(range_specs) =
        matches.get_many::<patchrange::Specification>("patchranges-applied")
    {
        indexmap::IndexSet::from_iter(
            patchrange::patches_from_specs(range_specs, &stack, patchrange::Allow::Applied)
                .map_err(|e| match e {
                    patchrange::Error::BoundaryNotAllowed { patchname, range }
                        if stack.is_unapplied(&patchname) =>
                    {
                        anyhow!("Patch `{patchname}` from `{range}` is already unapplied")
                    }
                    patchrange::Error::PatchNotAllowed { patchname, .. }
                        if stack.is_unapplied(&patchname) =>
                    {
                        anyhow!("Patch `{patchname}` is already unapplied")
                    }
                    _ => e.into(),
                })?,
        )
    } else {
        stack.applied().iter().rev().take(1).cloned().collect()
    };

    assert!(!patches.is_empty());

    let keep_flag = matches.get_flag("keep");
    let spill_flag = matches.get_flag("spill");
    repo.check_repository_state()?;

    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;

    statuses.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !keep_flag && !spill_flag {
        statuses.check_index_and_worktree_clean()?;
    }

    let mut new_unapplied: Vec<PatchName> = vec![];
    let mut new_applied: Vec<PatchName> = vec![];

    for pn in stack.applied() {
        if let Some(patchname) = patches.swap_take(pn) {
            new_unapplied.push(patchname);
        } else {
            new_applied.push(pn.clone());
        }
    }

    if spill_flag {
        let topmost_applied: Vec<PatchName> = stack
            .applied()
            .iter()
            .rev()
            .take(new_unapplied.len())
            .rev()
            .cloned()
            .collect();
        if new_unapplied != topmost_applied {
            return Err(anyhow!("Only topmost patches may be spilled"));
        }
    }

    new_unapplied.reserve(stack.unapplied().len());
    stack
        .unapplied()
        .iter()
        .for_each(|pn| new_unapplied.push(pn.clone()));

    stack
        .setup_transaction()
        .use_index_and_worktree(!spill_flag)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            trans.reorder_patches(Some(&new_applied), Some(&new_unapplied), None)?;
            Ok(())
        })
        .execute("pop")?;

    Ok(())
}
