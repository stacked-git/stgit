// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --reset` implementation.

use std::rc::Rc;

use anyhow::{anyhow, Result};

use crate::{
    color::get_color_stdout,
    print_info_message,
    stack::{InitializationPolicy, Stack, StackAccess, StackState, StackStateAccess},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--reset")
        .about("Soft reset the stack marking all patches as unapplied")
        .long_about(
            "Reset the stack head to the current git HEAD and mark all patches as \
             unapplied.\n\
             \n\
             This command is useful when the branch has diverged from the stack state \
             and you want to reset the stack without losing patches. After running this \
             command, you can reconcile the state by hand either by iteratively running \
             `stg push --merged` or by scrapping the patches and starting anew with \
             `stg uncommit`.",
        )
        .arg(
            clap::Arg::new("branch-any")
                .help("Branch to reset (defaults to current branch)")
                .value_name("branch")
                .value_parser(clap::value_parser!(crate::branchloc::BranchLocator)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let stack = if let Some(branch_loc) =
        matches.get_one::<crate::branchloc::BranchLocator>("branch-any")
    {
        let branch = branch_loc.resolve(repo)?;
        Stack::from_branch(repo, branch, InitializationPolicy::RequireInitialized)?
    } else {
        Stack::current(repo, InitializationPolicy::RequireInitialized)?
    };

    let config = repo.config_snapshot();
    if stack.is_protected(&config) {
        return Err(anyhow!(
            "this branch is protected; modification is not permitted."
        ));
    }

    if stack.get_branch_head().id == stack.head().id {
        print_info_message(
            matches,
            "git head already matching stack state, doing nothing",
        );
        return Ok(());
    }

    stack
        .setup_transaction()
        .use_index_and_worktree(false)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            let commit = trans.stack().get_branch_head().to_owned();

            let stack = trans.stack();
            let repo = stack.repo;
            let stack_state_commit = repo
                .find_reference(stack.get_stack_refname())?
                .peel_to_commit()
                .map(Rc::new)?;

            let new_stack_state = StackState::from_commit(trans.stack().repo, &stack_state_commit)?
                .reset_branch_state(commit, stack_state_commit);

            trans.reset_to_state(new_stack_state)
        })
        .execute("branch-reset")?;

    Ok(())
}
