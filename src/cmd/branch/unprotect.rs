// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --unprotect` implementation.

use anyhow::Result;

use crate::{
    branchloc::BranchLocator,
    stack::{InitializationPolicy, Stack},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--unprotect")
        .short_flag('u')
        .override_usage(super::super::make_usage(
            "stg branch --unprotect",
            &["[branch]"],
        ))
        .about("Allow StGit to modify a previously protected branch")
        .arg(
            clap::Arg::new("branch")
                .help("Branch to unprotect")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let stack = Stack::from_branch_locator(
        repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    stack.set_protected(false)
}
