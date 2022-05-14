//! `stg pop` implementation.

use std::iter::FromIterator;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    repo::RepositoryExtended,
    stack::{Error, Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("pop", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("pop")
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
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Pop all applied patches")
                .conflicts_with("number"),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Pop specified number of patches")
                .long_help(
                    "Pop the specified number of patches.\n\
                     \n\
                     A negative number indicates to pop all but that number \
                     of patches",
                )
                .takes_value(true)
                .allow_hyphen_values(true) // i.e. for negative ints
                .value_name("NUMBER")
                .validator(|s| {
                    s.parse::<isize>()
                        .map_err(|_| format!("'{s}' is not an integer"))
                }),
        )
        .arg(
            Arg::new("spill")
                .long("spill")
                .short('s')
                .help("Keep patches' modifications in working tree after popping"),
        )
        .arg(&*crate::argset::KEEP_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to pop")
                .multiple_values(true)
                .conflicts_with_all(&["all", "number"]),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let opt_number: Option<isize> = matches.value_of("number").map(|num_str| {
        num_str
            .parse::<isize>()
            .expect("validator previously parsed this")
    });

    if Some(0) == opt_number {
        return Ok(());
    }

    if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let mut patches: indexmap::IndexSet<PatchName> = if matches.is_present("all") {
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
    } else if let Some(patch_ranges) = matches.values_of("patches") {
        indexmap::IndexSet::from_iter(
            patchrange::parse(patch_ranges, &stack, patchrange::Allow::Applied).map_err(
                |e| match e {
                    patchrange::Error::BoundaryNotAllowed { patchname, range }
                        if stack.is_unapplied(&patchname) =>
                    {
                        anyhow!("Patch `{patchname}` from `{range}` is already unapplied")
                    }
                    patchrange::Error::PatchNotAllowed { patchname }
                        if stack.is_unapplied(&patchname) =>
                    {
                        anyhow!("Patch `{patchname}` is already unapplied")
                    }
                    _ => e.into(),
                },
            )?,
        )
    } else {
        stack.applied().iter().rev().take(1).cloned().collect()
    };

    assert!(!patches.is_empty());

    let opt_keep = matches.is_present("keep");
    let opt_spill = matches.is_present("spill");
    repo.check_repository_state()?;
    repo.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !opt_keep && !opt_spill {
        repo.check_index_and_worktree_clean()?;
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

    if opt_spill {
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
        .use_index_and_worktree(!opt_spill)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            trans.reorder_patches(Some(&new_applied), Some(&new_unapplied), None)?;
            Ok(())
        })
        .execute("pop")?;

    Ok(())
}
