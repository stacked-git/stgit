// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --describe` implementation.

use anyhow::Result;

use crate::{argset::get_one_str, ext::RepositoryExtended, wrap::PartialRefName};

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
                .value_parser(clap::value_parser!(PartialRefName)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let branch = if let Some(name) = matches.get_one::<PartialRefName>("branch-any") {
        repo.get_branch(name)?
    } else {
        repo.get_current_branch()?
    };
    let description = get_one_str(matches, "description").expect("required argument");
    let branchname = branch.get_branch_partial_name()?;
    super::set_description(repo, &branchname, description)
}
