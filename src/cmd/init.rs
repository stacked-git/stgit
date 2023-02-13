// SPDX-License-Identifier: GPL-2.0-only

//! `stg init` implementation.

use anyhow::Result;
use clap::ArgMatches;

use crate::{
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "init",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Initialize a StGit stack on current branch")
        .long_about(
            "Initialize a StGit stack on the current branch.\n\
             \n\
             A branch must be initialized with a StGit stack before patches may be \
             created with 'stg new', imported with 'stg import', or picked with 'stg \
             pick'.\n\
             \n\
             The branch and its git repository must already exist and contain at least \
             one commit before initializing a StGit stack. Branches created with `stg \
             branch --create` are automatically initialized.\n\
             \n\
             StGit stack metadata can be deinitialized from a branch using `stg branch \
             --cleanup`. See 'stg branch' for more details.",
        )
}

fn run(_: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    Stack::from_branch(&repo, None, InitializationPolicy::MustInitialize)?;
    Ok(())
}
