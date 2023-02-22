// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --unprotect` implementation.

use anyhow::Result;

use crate::{
    argset::{self, get_one_str},
    stack::{InitializationPolicy, Stack},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--unprotect")
        .short_flag('u')
        .override_usage("stg branch {--unprotect,-u} [branch]")
        .about("Allow StGit to modify a previously protected branch")
        .arg(
            clap::Arg::new("branch")
                .help("Branch to unprotect")
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
    stack.set_protected(false)
}
