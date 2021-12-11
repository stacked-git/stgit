use clap::{App, Arg, ArgMatches, ArgSettings};
use std::iter::FromIterator;

use crate::{
    error::Error,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{ConflictMode, Stack, StackTransaction},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("pop", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("pop")
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
                .setting(ArgSettings::TakesValue)
                .setting(ArgSettings::AllowHyphenValues) // i.e. for negative ints
                .value_name("NUMBER")
                .validator(|s| {
                    s.parse::<isize>()
                        .map_err(|_| format!("'{}' is not an integer", s))
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

fn run(matches: &ArgMatches) -> super::Result {
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

    if stack.state.applied.is_empty() {
        return Err(Error::NoAppliedPatches);
    }

    let mut patches: indexmap::IndexSet<PatchName> = if matches.is_present("all") {
        stack.state.applied.iter().cloned().collect()
    } else if let Some(number) = opt_number {
        let num_applied = stack.state.applied.len();
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
            .state
            .applied
            .iter()
            .rev()
            .take(num_to_take)
            .cloned()
            .collect()
    } else if let Some(patch_ranges) = matches.values_of("patches") {
        indexmap::IndexSet::from_iter(
            parse_patch_ranges(
                patch_ranges,
                &stack.state.applied,
                stack.state.all_patches(),
            )
            .map_err(|e| {
                if let Error::PatchRange(patch_range, extra) = e {
                    // See if one or more patches are unapplied in order to produce better
                    // error message.
                    let patch_ranges = matches.values_of("patches").unwrap();
                    if let Ok(patches) = parse_patch_ranges(
                        patch_ranges,
                        stack.state.all_patches(),
                        stack.state.all_patches(),
                    ) {
                        match patches
                            .iter()
                            .filter(|pn| stack.state.unapplied.contains(pn))
                            .count()
                        {
                            0 => Error::PatchRange(patch_range, extra),
                            1 => Error::PatchRange(
                                patch_range,
                                "patch already unapplied".to_string(),
                            ),
                            _ => Error::PatchRange(
                                patch_range,
                                "patches already unapplied".to_string(),
                            ),
                        }
                    } else {
                        Error::PatchRange(patch_range, extra)
                    }
                } else {
                    e
                }
            })?,
        )
    } else {
        stack.state.applied.iter().rev().take(1).cloned().collect()
    };

    assert!(!patches.is_empty());

    let opt_keep = matches.is_present("keep");
    let opt_spill = matches.is_present("spill");
    let conflicts_okay = false;
    stack.check_repository_state(conflicts_okay)?;
    stack.check_head_top_mismatch()?;
    if !opt_keep && !opt_spill {
        stack.check_index_clean()?;
        stack.check_worktree_clean()?;
    }

    let mut new_unapplied: Vec<PatchName> = vec![];
    let mut new_applied: Vec<PatchName> = vec![];

    for pn in &stack.state.applied {
        if let Some(patchname) = patches.swap_take(pn) {
            new_unapplied.push(patchname);
        } else {
            new_applied.push(pn.clone());
        }
    }

    if opt_spill {
        let topmost_applied: Vec<PatchName> = stack
            .state
            .applied
            .iter()
            .rev()
            .take(new_unapplied.len())
            .rev()
            .cloned()
            .collect();
        if new_unapplied != topmost_applied {
            return Err(Error::Generic(
                "only topmost patches may be spilled".to_string(),
            ));
        }
    }

    new_unapplied.reserve(stack.state.unapplied.len());
    stack
        .state
        .unapplied
        .iter()
        .for_each(|pn| new_unapplied.push(pn.clone()));

    let mut stdout = crate::color::get_color_stdout(matches);

    let discard_changes = false;
    let use_index_and_worktree = !opt_spill;
    let trans_context = StackTransaction::make_context(
        stack,
        ConflictMode::Disallow,
        discard_changes,
        use_index_and_worktree,
    );
    let exec_context = trans_context.transact(|trans| {
        trans.reorder_patches(Some(&new_applied), Some(&new_unapplied), None, &mut stdout)?;
        Ok(())
    });

    exec_context.execute("pop")?;

    Ok(())
}
