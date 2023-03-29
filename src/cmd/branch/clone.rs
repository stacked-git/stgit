// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --clone` implementation.

use std::str::FromStr;

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    ext::RepositoryExtended,
    stack::{state_refname_from_branch_name, InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
    wrap::PartialRefName,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--clone")
        .override_usage(super::super::make_usage(
            "stg branch --clone",
            &["[new-branch]"],
        ))
        .about("Clone the contents of the current branch")
        .long_about(
            "Clone the current branch as <new-branch>, if specified, or using the \
             current branch name with a timestamp.\n\
             \n\
             The description of the new branch will indicate it is a clone of the \
             current branch. The parent information of the new branch is copied from \
             the current branch.",
        )
        .arg(
            Arg::new("new-branch")
                .help("New branch name")
                .value_parser(clap::value_parser!(PartialRefName)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let current_branch = repo.get_current_branch()?;
    let current_branchname = current_branch.get_branch_partial_name()?;

    let generated_branchname;
    let new_branchname = if let Some(name) = matches.get_one::<PartialRefName>("new-branch") {
        name
    } else {
        let suffix = gix::actor::Time::now_local_or_utc().format(
            time::macros::format_description!("[year][month][day]-[hour][minute][second]"),
        );
        generated_branchname = PartialRefName::from_str(&format!("{current_branchname}-{suffix}"))
            .expect("valid partial reference name");
        &generated_branchname
    };

    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_worktree_clean()?;
    repo.check_repository_state()?;
    statuses.check_conflicts()?;

    if let Ok(stack) = Stack::current(repo, InitializationPolicy::RequireInitialized) {
        stack.check_head_top_mismatch()?;
        let state_ref = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference");
        let state_commit = state_ref.id().object()?.try_into_commit()?;
        repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: format!("clone from {current_branchname}").into(),
                },
                expected: gix::refs::transaction::PreviousValue::Any,
                new: gix::refs::Target::Peeled(state_commit.id),
            },
            name: gix::refs::FullName::try_from(state_refname_from_branch_name(
                new_branchname.as_ref(),
            ))?,
            deref: false,
        })?;
        stupid.branch_copy(None, new_branchname.as_ref())?;
    } else {
        stupid.branch_copy(None, new_branchname.as_ref())?;
        Stack::from_branch_name(repo, new_branchname, InitializationPolicy::MustInitialize)?;
    };

    super::set_stgit_parent(repo, new_branchname, Some(&current_branchname))?;

    super::set_description(
        repo,
        new_branchname,
        &format!("clone of {current_branchname}"),
    )?;

    let new_branch = repo.get_branch(new_branchname)?;
    stupid.checkout(new_branch.get_branch_name().unwrap())
}
