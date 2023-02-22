// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --create` implementation.

use std::rc::Rc;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

use crate::{
    argset::{self, get_one_str},
    ext::RepositoryExtended,
    patch::SingleRevisionSpec,
    print_info_message,
    stack::{InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
    wrap::Branch,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--create")
        .short_flag('c')
        .override_usage("stg branch {--create,-c} <new-branch> [committish]")
        .about("Create and switch to a new branch")
        .long_about(
            "Create and switch to a new branch. The new branch is initialized as a \
             StGit patch stack. The new branch will be based on the current HEAD, by \
             default, unless an optional committish provided for the base.\n\
             \n\
             StGit attempts to detect the branch from which the new branch forked, as \
             well as the remote repository of that parent branch such that 'stg pull' \
             will pull from the correct remote branch. A warning will be printed if \
             the parent branch cannot be determined.",
        )
        .arg(
            clap::Arg::new("new-branch")
                .help("New branch name")
                .required(true)
                .value_parser(argset::parse_branch_name),
        )
        .arg(
            clap::Arg::new("committish")
                .help("Base commit for new branch")
                .value_parser(clap::value_parser!(SingleRevisionSpec)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let new_branchname = get_one_str(matches, "new-branch").expect("required argument");
    let new_fullname = gix::refs::FullName::try_from(format!("refs/heads/{new_branchname}"))?;
    if repo.try_find_reference(&new_fullname)?.is_some() {
        return Err(anyhow!("branch `{new_branchname}` already exists"));
    }

    repo.check_repository_state()?;
    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;

    let maybe_committish = matches.get_one::<SingleRevisionSpec>("committish");
    let maybe_committish_str = matches
        .get_raw("committish")
        .map(|raw_values| raw_values.into_iter().next().unwrap().to_str().unwrap());

    let parent_branch = if let Some(committish_str) = maybe_committish_str {
        statuses.check_worktree_clean()?;

        if let Ok(parent_reference) = repo.find_reference(committish_str) {
            let parent_branchname = parent_reference
                .name()
                .shorten()
                .to_str()
                .expect("reference name is valid UTF-8");
            print_info_message(
                matches,
                &format!("Recording `{parent_branchname}` as parent branch"),
            );
            Some(Branch::wrap(parent_reference))
        } else {
            print_info_message(
                matches,
                &format!("Do not know how to determine parent branch from `{committish_str}`"),
            );
            None
        }
    } else if let Ok(current_branch) = repo.get_branch(None) {
        Some(current_branch)
    } else {
        None
    };

    let (target_commit, target_name) = if let Some(parent_branch) = parent_branch.as_ref() {
        (
            Rc::new(parent_branch.get_commit()?),
            parent_branch.get_branch_name()?,
        )
    } else if let Some(committish) = maybe_committish {
        (
            committish.resolve(repo, None::<&Stack>)?.commit,
            maybe_committish_str.unwrap(),
        )
    } else {
        (Rc::new(repo.head_commit()?), "HEAD")
    };

    let parent_branchname = parent_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_name().ok());

    repo.edit_reference(gix::refs::transaction::RefEdit {
        change: gix::refs::transaction::Change::Update {
            log: gix::refs::transaction::LogChange {
                mode: gix::refs::transaction::RefLog::AndReference,
                force_create_reflog: false,
                message: format!("branch: Created from {target_name}").into(),
            },
            expected: gix::refs::transaction::PreviousValue::MustNotExist,
            new: gix::refs::Target::Peeled(target_commit.id),
        },
        name: gix::refs::FullName::try_from(format!("refs/heads/{new_branchname}"))?,
        deref: false,
    })?;

    let new_branch = Branch::wrap(repo.find_reference(new_branchname)?);

    let stack = match Stack::from_branch(
        repo,
        Some(new_branchname),
        InitializationPolicy::MustInitialize,
    ) {
        Ok(stack) => stack,
        Err(e) => {
            new_branch.delete()?;
            return Err(e);
        }
    };

    if let Some(parent_branch) = parent_branch.as_ref() {
        super::set_stgit_parent(repo, new_branchname, parent_branchname)?;
        if let Some(upstream_name) = copy_upstream(parent_branch, &new_branch, repo)? {
            print_info_message(
                matches,
                &format!("Using remote `{upstream_name}` to pull parent from"),
            );
        } else {
            print_info_message(matches, "Recording as a local branch");
        }
    }

    match stupid.checkout(new_branch.get_branch_name().unwrap()) {
        Ok(()) => Ok(()),
        Err(e) => {
            new_branch.delete()?;
            if let Ok(reference) = repo.find_reference(stack.get_stack_refname()) {
                reference.delete().ok();
            }
            Err(e)
        }
    }
}

fn copy_upstream(
    from_branch: &Branch,
    to_branch: &Branch,
    repo: &gix::Repository,
) -> Result<Option<String>> {
    let (category, from_short_name) = from_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("from reference is a local branch");
    assert!(matches!(category, gix::refs::Category::LocalBranch));
    let (category, to_short_name) = to_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("to reference is a local branch");
    assert!(matches!(category, gix::refs::Category::LocalBranch));
    let config = repo.config_snapshot();
    let remote = config.string(format!("branch.{from_short_name}.remote").as_str());
    let merge = config.string(format!("branch.{from_short_name}.merge").as_str());
    if let (Some(remote), Some(merge)) = (remote, merge) {
        let merge_name = gix::refs::FullName::try_from(merge.as_bstr())?;
        let merge_short_name = merge_name.shorten().to_str_lossy();
        let mut local_config_file = repo.local_config_file()?;
        local_config_file.set_raw_value(
            "branch",
            Some(to_short_name),
            "remote",
            remote.as_bstr(),
        )?;
        local_config_file.set_raw_value("branch", Some(to_short_name), "merge", merge.as_bstr())?;
        repo.write_local_config(local_config_file)?;
        Ok(Some(format!("{remote}/{merge_short_name}")))
    } else {
        Ok(None)
    }
}
