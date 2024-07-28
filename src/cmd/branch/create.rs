// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --create` implementation.

use std::rc::Rc;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

use crate::{
    ext::RepositoryExtended,
    patch::SingleRevisionSpec,
    print_info_message,
    stack::{InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
    wrap::{Branch, PartialRefName},
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--create")
        .short_flag('c')
        .override_usage(super::super::make_usage(
            "stg branch --create",
            &["<new-branch> [committish]"],
        ))
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
                .value_parser(clap::value_parser!(PartialRefName)),
        )
        .arg(
            clap::Arg::new("committish")
                .help("Base commit for new branch")
                .value_parser(clap::value_parser!(SingleRevisionSpec)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let new_branchname = matches
        .get_one::<PartialRefName>("new-branch")
        .expect("required argument");
    let new_fullname = if new_branchname.as_ref().starts_with("refs/heads/") {
        gix::refs::FullName::try_from(new_branchname.as_ref())?
    } else if new_branchname.as_ref().starts_with("refs/") {
        return Err(anyhow!(
            "invalid reference name for local branch `{new_branchname}`"
        ));
    } else {
        gix::refs::FullName::try_from(format!("refs/heads/{new_branchname}"))?
    };
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
        if let Some(parent_reference) =
            repo.find_reference(committish_str)
                .ok()
                .filter(|parent_reference| {
                    use gix::refs::Category;
                    matches!(
                        parent_reference.name().category(),
                        Some(Category::LocalBranch | Category::RemoteBranch)
                    )
                })
        {
            let parent_shortname = parent_reference.name().shorten();
            print_info_message(
                matches,
                &format!("Recording `{parent_shortname}` as parent branch"),
            );
            Some(Branch::wrap(parent_reference))
        } else {
            print_info_message(
                matches,
                &format!("Do not know how to determine parent branch from `{committish_str}`"),
            );
            None
        }
    } else if let Ok(current_branch) = repo.get_current_branch() {
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

    let new_branch = Branch::wrap(repo.find_reference(&new_fullname)?);

    let stack = match Stack::from_branch_name(
        repo,
        new_branchname,
        InitializationPolicy::ForceInitialize,
    ) {
        Ok(stack) => stack,
        Err(e) => {
            new_branch.delete()?;
            return Err(e);
        }
    };

    if let Some(parent_branch) = parent_branch.as_ref() {
        let parent_branchname = parent_branch.get_branch_partial_name().ok();
        super::set_stgit_parent(repo, new_branchname, parent_branchname.as_ref())?;
        if let Some(upstream_name) = set_upstream(parent_branch, &new_branch, repo)? {
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

fn set_upstream(
    from_branch: &Branch,
    to_branch: &Branch,
    repo: &gix::Repository,
) -> Result<Option<String>> {
    use gix::refs::Category;

    let (from_category, from_short_name) = from_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("local or remote branch");

    let (to_category, to_short_name) = to_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("to reference is a local branch");
    assert!(matches!(to_category, Category::LocalBranch));

    match from_category {
        Category::LocalBranch => {
            let config = repo.config_snapshot();
            let remote = config.string(format!("branch.{from_short_name}.remote").as_str());
            let merge = config.string(format!("branch.{from_short_name}.merge").as_str());

            if let (Some(remote), Some(merge)) = (remote, merge) {
                let merge_name = gix::refs::FullName::try_from(merge.as_bstr())?;
                let merge_short_name = merge_name.shorten();
                let mut local_config_file = repo.local_config_file()?;
                local_config_file.set_raw_value_by(
                    "branch",
                    Some(to_short_name),
                    "remote",
                    remote.as_bstr(),
                )?;
                local_config_file.set_raw_value_by(
                    "branch",
                    Some(to_short_name),
                    "merge",
                    merge.as_bstr(),
                )?;
                repo.write_local_config(local_config_file)?;
                Ok(Some(format!("{remote}/{merge_short_name}")))
            } else {
                Ok(None)
            }
        }

        Category::RemoteBranch => {
            let mut local_config_file = repo.local_config_file()?;
            let (remote_name, remote_branch_name) = from_short_name
                .split_once_str(b"/")
                .expect("remote branch short name has form <remote>/<branch>");
            local_config_file.set_raw_value_by(
                "branch",
                Some(to_short_name),
                "remote",
                remote_name,
            )?;
            local_config_file.set_raw_value_by(
                "branch",
                Some(to_short_name),
                "merge",
                remote_branch_name,
            )?;
            repo.write_local_config(local_config_file)?;
            Ok(Some(from_short_name.to_string()))
        }

        _ => panic!("from branch must be LocalBranch or RemoteBranch"),
    }
}
