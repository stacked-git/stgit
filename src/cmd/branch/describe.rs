// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --describe` implementation.

use anyhow::Result;

use crate::{
    argset::{self, get_one_str},
    ext::RepositoryExtended,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--describe")
        .short_flag('d')
        .override_usage("stg branch {--describe,-d} <description> [branch]")
        .alias("--description")
        .about("Set the branch description")
        .arg(
            clap::Arg::new("description")
                .help("Description string for branch")
                .required(true),
        )
        .arg(
            clap::Arg::new("branch-any")
                .help("Branch to describe")
                .value_name("branch")
                .value_parser(argset::parse_branch_name),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let branch = repo.get_branch(get_one_str(matches, "branch-any"))?;
    let description = get_one_str(matches, "description").expect("required argument");
    let branchname = branch.get_branch_name()?;
    super::set_description(repo, branchname, description)
}
