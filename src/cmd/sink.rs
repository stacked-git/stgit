// SPDX-License-Identifier: GPL-2.0-only

//! `stg sink` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchrange, LocationConstraint, PatchLocator, PatchName, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "sink",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Move patches deeper in the stack")
        .long_about(
            "Move the specified patches down the stack.\n\
             \n\
             If no patch is specified on the command line, the current (topmost) patch \
             is sunk. By default, patches are sunk to the bottom of the stack, but the \
             '--to' option may be used to place them under any applied patch.\n\
             \n\
             Internally, sinking involves popping all patches to the bottom (or to the \
             target patch if '--to' is used), then pushing the patches to sink, and \
             then, unless '--nopush' is specified, pushing back any other formerly \
             applied patches.\n\
             \n\
             Sinking may be useful, for example, to group stable patches at the bottom \
             of the stack where they less likely to be impacted by the push of another \
             patch, and from where they can be more easily committed or pushed to \
             another repository.\n\
             ",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to sink")
                .value_name("patch")
                .num_args(1..)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchRange)),
        )
        .arg(
            Arg::new("nopush")
                .long("nopush")
                .short('n')
                .help("Do not push any patches back after sinking")
                .long_help(
                    "Do not push any formerly applied patches after sinking. \
                     Only the patches to sink are pushed.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("target")
                .long("to")
                .short('t')
                .help("Sink patches below <target> patch")
                .long_help(
                    "Sink patches below <target> patch.\n\
                     \n\
                     Specified patches are placed below <target> instead of at the \
                     bottom of the stack.",
                )
                .value_name("target")
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchLocator)),
        )
        .arg(argset::keep_arg())
        .arg(argset::committer_date_is_author_date_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    let nopush_flag = matches.get_flag("nopush");
    let keep_flag = matches.contains_id("keep");

    repo.check_repository_state()?;
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !keep_flag {
        statuses.check_index_and_worktree_clean()?;
    }

    let opt_target: Option<PatchName> = matches
        .get_one::<PatchLocator>("target")
        .map(|loc| loc.resolve_name(&stack))
        .transpose()
        .map_err(|e| anyhow!("target: {e}"))?
        .map(|name| name.constrain(&stack, LocationConstraint::Applied))
        .transpose()
        .map_err(|e| match e {
            crate::patch::name::Error::PatchNotAllowed { patchname, .. } => {
                anyhow!("cannot sink below `{patchname}` since it is not applied")
            }
            _ => e.into(),
        })?;

    let patches: Vec<PatchName> =
        if let Some(range_specs) = matches.get_many::<PatchRange>("patchranges") {
            patchrange::resolve_names(&stack, range_specs, RangeConstraint::All)?
        } else if let Some(patchname) = stack.applied().last() {
            vec![patchname.clone()]
        } else {
            return Err(super::Error::NoAppliedPatches.into());
        };

    if let Some(target_patch) = &opt_target {
        if patches.contains(target_patch) {
            return Err(anyhow!(
                "target patch `{target_patch}` may not also be a patch to sink",
            ));
        }
    }

    let mut remaining_unapplied: Vec<PatchName> = stack
        .unapplied()
        .iter()
        .filter(|pn| !patches.contains(pn))
        .cloned()
        .collect();

    let mut remaining_applied: Vec<PatchName> = stack
        .applied()
        .iter()
        .filter(|pn| !patches.contains(pn))
        .cloned()
        .collect();

    let target_pos = if let Some(target_patch) = &opt_target {
        remaining_applied
            .iter()
            .position(|pn| pn == target_patch)
            .expect("already validated that target is applied")
    } else {
        0
    };

    let mut patches = patches;

    let (applied, unapplied) = if nopush_flag {
        let mut applied: Vec<PatchName> = Vec::with_capacity(target_pos + patches.len());
        applied.extend(remaining_applied.drain(0..target_pos));
        applied.append(&mut patches);
        let mut unapplied: Vec<PatchName> =
            Vec::with_capacity(remaining_applied.len() + remaining_unapplied.len());
        unapplied.append(&mut remaining_applied);
        unapplied.append(&mut remaining_unapplied);
        (applied, unapplied)
    } else {
        let mut applied: Vec<PatchName> =
            Vec::with_capacity(remaining_applied.len() + patches.len());
        applied.extend(remaining_applied.drain(0..target_pos));
        applied.append(&mut patches);
        applied.append(&mut remaining_applied);
        (applied, remaining_unapplied)
    };

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .committer_date_is_author_date(matches.get_flag("committer-date-is-author-date"))
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.reorder_patches(Some(&applied), Some(&unapplied), None))
        .execute("sink")?;

    Ok(())
}
