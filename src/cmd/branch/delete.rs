// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --delete` implementation.

use anyhow::{anyhow, Context, Result};

use crate::{
    branchloc::BranchLocator,
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

use super::get_stgit_parent;

pub(super) fn command() -> clap::Command {
    clap::Command::new("--delete")
        .short_flag('D')
        .override_usage(super::super::make_usage(
            "stg branch --delete",
            &["[--force] [<branch>]"],
        ))
        .about("Delete a branch")
        .long_about(
            "Delete a branch.\n\
             \n\
             The branch will not be deleted if there are any patches remaining unless \
             the '--force' option is provided.\n\
             \n\
             If the current branch is selected for deletion, its parent branch must be \
             configured and the worktree must be clean. The parent branch will be \
             checked-out after the current branch is deleted.\n\
             \n\
             A protected branch may not be deleted; it must be unprotected first.",
        )
        .arg(
            clap::Arg::new("branch-any")
                .help("Branch to delete")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
        .arg(
            clap::Arg::new("force")
                .long("force")
                .help("Force deletion even if branch has patches")
                .action(clap::ArgAction::SetTrue),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let (target_branch, target_branchname) = if let Some(branch_loc) = matches.get_one::<BranchLocator>("branch-any") {
        let branch = branch_loc.resolve(repo)?;
        let branchname = branch.get_branch_partial_name()?;
        (branch, branchname)
    } else if let Ok(branch) = repo.get_current_branch() {
        let branchname = branch.get_branch_partial_name()?;
        (branch, branchname)
    } else {
        return Err(anyhow!("no target branch specified and no current branch"));
    };

    let current_branch = repo.get_current_branch().ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_partial_name().ok());
    let config_snapshot = repo.config_snapshot();
    let stupid = repo.stupid();

    let switch_to_branch = if Some(&target_branchname) == current_branchname.as_ref() {
        if let Some(parent_branch) = get_stgit_parent(&config_snapshot, &target_branchname) {
            let statuses = stupid.statuses(None)?;
            if let Err(e) = statuses
                .check_worktree_clean()
                .and_then(|_| statuses.check_conflicts())
            {
                return Err(anyhow!("cannot delete the current branch: {e}"));
            }
            Some(parent_branch)
        } else {
            return Err(anyhow!(
                "cannot delete the current branch without a known parent branch"
            ));
        }
    } else {
        None
    };

    if let Ok(stack) = Stack::from_branch(
        repo,
        target_branch.clone(),
        InitializationPolicy::RequireInitialized,
    ) {
        if stack.is_protected(&config_snapshot) {
            return Err(anyhow!("delete not permitted: this branch is protected"));
        } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
            return Err(anyhow!(
                "delete not permitted: the series still contains patches (override with --force)"
            ));
        }
        stack.deinitialize()?;
    }

    if let Some(branch_name) = switch_to_branch {
        stupid
            .checkout(&branch_name)
            .context("switching to parent branch")?;
    }

    target_branch.delete()?;

    let mut local_config_file = repo.local_config_file()?;
    local_config_file.remove_section("branch", Some(target_branchname.as_ref().into()));
    repo.write_local_config(local_config_file)
        .context("writing local config file")?;

    Ok(())
}
