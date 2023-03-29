// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --protect` implementation.

use anyhow::Result;

use crate::{
    branchloc::BranchLocator,
    stack::{InitializationPolicy, Stack},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--protect")
        .short_flag('p')
        .override_usage(super::super::make_usage(
            "stg branch --protect",
            &["[branch]"],
        ))
        .about("Prevent StGit from modifying a branch")
        .arg(
            clap::Arg::new("branch")
                .help("Branch to protect")
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
    stack.set_protected(true)
}
