// SPDX-License-Identifier: GPL-2.0-only

//! `stg hide` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patchname::PatchName,
    patchrange,
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
                .value_parser(clap::value_parser!(patchrange::Specification))
                .required(true),
        )
        .arg(argset::branch_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let stack = Stack::from_branch(
        &repo,
        argset::get_one_str(matches, "branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

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
