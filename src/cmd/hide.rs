// SPDX-License-Identifier: GPL-2.0-only

//! `stg hide` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    branchloc::BranchLocator,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchrange, PatchName, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "hide",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Hide patches in the series")
        .long_about(
            "Hide patches in the series.\n\
             \n\
             Hidden patches are no longer shown in the plain 'series' output.",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to hide")
                .value_name("patch")
                .num_args(1..)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchRange))
                .required(true),
        )
        .arg(argset::branch_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = patchrange::resolve_names(
        &stack,
        matches
            .get_many::<PatchRange>("patchranges")
            .expect("clap ensures at least one range is provided"),
        RangeConstraint::All,
    )?;

    // Already hidden patches are silent no-ops.
    let to_hide: Vec<PatchName> = patches
        .iter()
        .filter(|pn| !stack.is_hidden(pn))
        .cloned()
        .collect();

    stack
        .setup_transaction()
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.hide_patches(&to_hide))
        .execute("hide")?;

    Ok(())
}
