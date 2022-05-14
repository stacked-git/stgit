//! `stg hide` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("hide", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("hide")
        .about("Hide patches in the series")
        .long_about(
            "Hide patches in the series.\n\
             \n\
             Hidden patches are no longer shown in the plain 'series' output.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to hide")
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

    let patches: Vec<PatchName> = patchrange::parse(patch_ranges, &stack, patchrange::Allow::All)?;

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
