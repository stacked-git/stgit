// SPDX-License-Identifier: GPL-2.0-only

//! `stg unhide` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{argset, color::get_color_stdout, patchname::PatchName, patchrange, stack::Stack};

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
                .value_parser(clap::value_parser!(patchrange::Specification))
                .required(true),
        )
        .arg(argset::branch_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    stack.check_head_top_mismatch()?;

    let specs = matches
        .get_many::<patchrange::Specification>("patchranges-hidden")
        .expect("clap ensures at least one range is provided");

    let patches: Vec<PatchName> =
        patchrange::patches_from_specs(specs, &stack, patchrange::Allow::Hidden).map_err(|e| {
            match e {
                crate::patchrange::Error::BoundaryNotAllowed { patchname, range } => {
                    anyhow!("Patch `{patchname}` from `{range}` is not hidden")
                }
                crate::patchrange::Error::PatchNotAllowed { patchname, .. } => {
                    anyhow!("Patch `{patchname}` is not hidden")
                }
                _ => e.into(),
            }
        })?;

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.unhide_patches(&patches))
        .execute("unhide")?;

    Ok(())
}
