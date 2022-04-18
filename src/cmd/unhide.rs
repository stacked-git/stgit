//! `stg unhide` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{Stack, StackStateAccess},
};

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
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to unhide")
                .required(true)
                .multiple_values(true)
                .forbid_empty_values(true),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    stack.check_head_top_mismatch()?;

    let patch_ranges = matches
        .values_of("patches")
        .expect("clap ensures at least one range is provided");

    let patches: Vec<PatchName> =
        parse_patch_ranges(patch_ranges, stack.hidden(), stack.all_patches()).map_err(
            |e| match e {
                crate::patchrange::Error::BoundaryNotAllowed { patchname, range } => {
                    anyhow!("patch `{patchname}` from `{range}` is not hidden")
                }
                crate::patchrange::Error::PatchNotAllowed { patchname } => {
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
