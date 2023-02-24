// SPDX-License-Identifier: GPL-2.0-only

//! `stg init` implementation.

use anyhow::Result;
use clap::ArgMatches;

use crate::{
    argset,
    branchloc::BranchLocator,
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
        .about("Initialize a StGit stack on a branch")
        .long_about(
            "Initialize a StGit stack on a branch.\n\
             \n\
             Initializing a branch with a StGit stack commits initial, empty stack \
             state for the branch to the repository. Theses stack metadata commits are \
             tracked by the `refs/stacks/<branch>` reference. Updated stack state is \
             committed by each StGit command that modifies the stack. StGit users do \
             not have to do anything with the `refs/stacks/<branch>` ref directly.\n\
             \n\
             Some StGit commands, such as `stg new` and `stg uncommit`, will \
             automatically initialize the stack, so it is often not necessary to \
             explicitly initialize the stack on a branch. Also, branches created with \
             `stg branch --create` are automatically initialized.\n\
             \n\
             The branch must already exist and point to a commit before initializing a \
             StGit stack.\n\
             \n\
             StGit stack metadata can be deinitialized from a branch using `stg branch \
             --cleanup`. See 'stg branch' for more details.",
        )
        .arg(argset::branch_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::MustInitialize,
    )?;
    Ok(())
}
