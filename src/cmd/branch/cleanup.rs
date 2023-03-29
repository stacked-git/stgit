// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --cleanup` implementation.

use anyhow::{anyhow, Result};

use crate::{
    branchloc::BranchLocator,
    stack::{InitializationPolicy, Stack, StackStateAccess},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--cleanup")
        .override_usage(super::super::make_usage(
            "stg branch --cleanup",
            &["[--force] [branch]"],
        ))
        .about("Remove StGit patch stack from branch")
        .long_about(
            "Remove StGit patch stack from branch. The operation will be refused if \
             any patches remain, unless the '--force' option is provided.\n\
             \n\
             A protected branch will not be cleaned up; it must be unprotected \
             first.\n\
             \n\
             A cleaned up branch may be reinitialized using 'stg init'.",
        )
        .arg(
            clap::Arg::new("branch")
                .help("Branch to clean up")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
        .arg(
            clap::Arg::new("force")
                .long("force")
                .help("Force clean up even if branch has patches")
                .action(clap::ArgAction::SetTrue),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let stack = Stack::from_branch_locator(
        repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    if stack.is_protected(&repo.config_snapshot()) {
        return Err(anyhow!("clean up not permitted: this branch is protected"));
    } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
        return Err(anyhow!(
            "clean up not permitted: the series still contains patches (override with --force)"
        ));
    }
    stack.deinitialize()?;
    Ok(())
}
