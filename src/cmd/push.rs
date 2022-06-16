// SPDX-License-Identifier: GPL-2.0-only

//! `stg push` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    repo::RepositoryExtended,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("push", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("push")
        .about("Push (apply) one or more unapplied patches")
        .long_about(
            "Push one or more unapplied patches from the series onto the stack.\n\
             \n\
             By default, the first unapplied patch is pushed.\n\
             \n\
             Unapplied patches may be pushed in arbitrary order, but out of \
             order pushes may result in merge conflicts. If there are conflicts \
             while pushing a patch, the conflicts are written to the work tree \
             and the push command halts. Conflicts may then be resolved using \
             the normal Git methods, or alternatively the push may be undone \
             using 'stg undo'.",
        )
        .override_usage(
            "stg push [OPTIONS] [patch]...\n    \
             stg push [OPTIONS] -n <number>\n    \
             stg push [OPTIONS] --all",
        )
        .arg(
            Arg::new("patchranges-unapplied")
                .help("Patches to push")
                .value_name("patch")
                .multiple_values(true)
                .forbid_empty_values(true)
                .conflicts_with_all(&["all", "number"]),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Push all unapplied patches")
                .conflicts_with("number"),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Push specified number of patches")
                .long_help(
                    "Push the specified number of patches.\n\
                     \n\
                     A negative number indicates to push all but that number \
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
            Arg::new("reverse")
                .long("reverse")
                .help("Push the patches in reverse order"),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Reorder patches by pushing without applying")
                .conflicts_with_all(&["all", "number"])
                .requires("patchranges-unapplied")
                .conflicts_with_all(&["set-tree", "merged"]),
        )
        .arg(
            Arg::new("set-tree")
                .long("set-tree")
                .help("Push patches keeping their original trees")
                .long_help(
                    "Push patches keeping their original trees.\n\
                     \n\
                     For each patch pushed, instead of performing a merge, the \
                     patch is pushed such the resulting tree will be identical \
                     to the tree associated with the patch.\n\
                     \n\
                     This can be useful when splitting a patch by first \
                     popping the patch and creating a new patch with some of \
                     the changes. Pushing the original patch with '--set-tree' \
                     will avoid conflicts and only the remaining changes will \
                     be in the patch.",
                ),
        )
        .arg(&*crate::argset::KEEP_ARG)
        .arg(&*crate::argset::MERGED_ARG)
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

    if stack.unapplied().is_empty() {
        return Err(anyhow!("No unapplied patches"));
    }

    let mut patches: Vec<PatchName> = if matches.is_present("all") {
        stack.unapplied().to_vec()
    } else if let Some(number) = opt_number {
        let num_unapplied = stack.unapplied().len();
        let num_to_take: usize = {
            if number >= 0 {
                std::cmp::min(number as usize, num_unapplied)
            } else if number.unsigned_abs() < num_unapplied {
                num_unapplied - number.unsigned_abs()
            } else {
                0
            }
        };
        stack
            .unapplied()
            .iter()
            .take(num_to_take)
            .cloned()
            .collect()
    } else if let Some(patchranges) = matches.values_of("patchranges-unapplied") {
        patchrange::parse(patchranges, &stack, patchrange::Allow::Unapplied).map_err(
            |e| match e {
                crate::patchrange::Error::BoundaryNotAllowed { patchname, range }
                    if stack.is_applied(&patchname) =>
                {
                    anyhow!("Patch `{patchname}` from `{range}` is already applied")
                }
                crate::patchrange::Error::PatchNotAllowed { patchname, .. }
                    if stack.is_applied(&patchname) =>
                {
                    anyhow!("Patch `{patchname}` is already applied")
                }
                _ => e.into(),
            },
        )?
    } else {
        stack.unapplied().iter().take(1).cloned().collect()
    };

    assert!(!patches.is_empty());

    let opt_reverse = matches.is_present("reverse");
    let opt_noapply = matches.is_present("noapply");
    let opt_settree = matches.is_present("set-tree");
    let opt_merged = matches.is_present("merged");
    let opt_keep = matches.is_present("keep");

    repo.check_repository_state()?;
    repo.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !opt_keep && !opt_noapply {
        repo.check_index_and_worktree_clean()?;
    }

    if opt_reverse {
        patches.reverse();
    }

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            if opt_settree {
                for (i, patchname) in (&patches).iter().enumerate() {
                    let is_last = i + 1 == patches.len();
                    trans.push_tree(patchname, is_last)?;
                }
                Ok(())
            } else if opt_noapply {
                let mut unapplied = patches.clone();
                unapplied.extend(
                    trans
                        .unapplied()
                        .iter()
                        .filter(|pn| !patches.contains(pn))
                        .cloned(),
                );
                trans.reorder_patches(None, Some(&unapplied), None)
            } else {
                trans.push_patches(&patches, opt_merged)
            }
        })
        .execute("push")?;

    Ok(())
}
