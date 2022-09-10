// SPDX-License-Identifier: GPL-2.0-only

//! `stg hide` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    (
        "hide",
        StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackManipulation,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("hide")
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
                .multiple_values(true)
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

    let patches: Vec<PatchName> = patchrange::patches_from_specs(
        matches
            .get_many::<patchrange::Specification>("patchranges")
            .expect("clap ensures at least one range is provided"),
        &stack,
        patchrange::Allow::All,
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
