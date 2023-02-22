// SPDX-License-Identifier: GPL-2.0-only

//! `stg unhide` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchrange, PatchName, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack},
    wrap::PartialRefName,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "unhide",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Unhide hidden patches")
        .long_about(
            "Unhide hidden patches in the series.\n\
             \n\
             Hidden patches are no longer shown in the plain 'series' output.",
        )
        .arg(
            Arg::new("patchranges-hidden")
                .help("Patches to unhide")
                .value_name("patch")
                .num_args(1..)
                .value_parser(clap::value_parser!(PatchRange))
                .required(true),
        )
        .arg(argset::branch_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(
        &repo,
        matches.get_one::<PartialRefName>("branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    stack.check_head_top_mismatch()?;

    let range_specs = matches
        .get_many::<PatchRange>("patchranges-hidden")
        .expect("clap ensures at least one range is provided");

    let patches: Vec<PatchName> =
        patchrange::resolve_names(&stack, range_specs, RangeConstraint::Hidden).map_err(
            |e| match e {
                patchrange::Error::Name(crate::patch::name::Error::PatchNotAllowed {
                    patchname,
                    ..
                }) => {
                    anyhow!("patch `{patchname}` is not hidden")
                }
                _ => e.into(),
            },
        )?;

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.unhide_patches(&patches))
        .execute("unhide")?;

    Ok(())
}
