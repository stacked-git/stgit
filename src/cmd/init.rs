// SPDX-License-Identifier: GPL-2.0-only

//! `stg init` implementation.

use anyhow::Result;
use clap::ArgMatches;

use crate::stack::Stack;

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "init",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command<'static> {
    clap::Command::new(STGIT_COMMAND.name).about("Initialize a StGit stack on current branch")
}

fn run(_: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let branch_name = None;
    Stack::initialize(&repo, branch_name)?;
    Ok(())
}
