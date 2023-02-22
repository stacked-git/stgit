// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --protect` implementation.

use anyhow::Result;

use crate::{
    argset::{self, get_one_str},
    stack::{InitializationPolicy, Stack},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--protect")
        .short_flag('p')
        .override_usage("stg branch {--protect,-p} [branch]")
        .about("Prevent StGit from modifying a branch")
        .arg(
            clap::Arg::new("branch")
                .help("Branch to protect")
                .value_name("branch")
                .value_parser(argset::parse_branch_name),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(
        repo,
        get_one_str(matches, "branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    stack.set_protected(true)
}
