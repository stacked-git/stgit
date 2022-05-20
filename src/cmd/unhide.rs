// SPDX-License-Identifier: GPL-2.0-only

//! `stg unhide` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{color::get_color_stdout, patchname::PatchName, patchrange, stack::Stack};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("unhide", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("unhide")
        .about("Unhide hidden patches")
        .long_about(
            "Unhide hidden patches in the series.\n\
             \n\
             Hidden patches are no longer shown in the plain 'series' output.",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to unhide")
                .value_name("patch")
                .multiple_values(true)
                .forbid_empty_values(true)
                .required(true),
        )
        .arg(&*crate::argset::BRANCH_ARG)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    stack.check_head_top_mismatch()?;

    let patchranges = matches
        .values_of("patchranges")
        .expect("clap ensures at least one range is provided");

    let patches: Vec<PatchName> = patchrange::parse(patchranges, &stack, patchrange::Allow::Hidden)
        .map_err(|e| match e {
            crate::patchrange::Error::BoundaryNotAllowed { patchname, range } => {
                anyhow!("Patch `{patchname}` from `{range}` is not hidden")
            }
            crate::patchrange::Error::PatchNotAllowed { patchname, .. } => {
                anyhow!("Patch `{patchname}` is not hidden")
            }
            _ => e.into(),
        })?;

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.unhide_patches(&patches))
        .execute("unhide")?;

    Ok(())
}
