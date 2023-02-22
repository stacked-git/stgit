// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --rename` implementation.

use std::str::FromStr;

use anyhow::Result;

use crate::{
    ext::RepositoryExtended,
    stack::{state_refname_from_branch_name, InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
    wrap::PartialRefName,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--rename")
        .short_flag('r')
        .override_usage("stg branch {--rename,-r} [old-name] <new-name>")
        .about("Rename an existing branch")
        .arg(
            clap::Arg::new("branch-any")
                .help("Optional name of branch to rename and new branch name")
                .hide_short_help(true)
                .required(true)
                .num_args(1..=2)
                .value_parser(clap::value_parser!(PartialRefName)),
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let names: Vec<_> = matches
        .get_many::<PartialRefName>("branch-any")
        .unwrap()
        .collect();
    let current_branch_name;
    let (old_branchname, new_branchname) = if names.len() == 2 {
        repo.get_branch(Some(names[0]))?;
        (names[0], names[1])
    } else {
        current_branch_name = repo.get_branch(None)?.get_branch_partial_name()?;
        (&current_branch_name, names[0])
    };

    let stupid = repo.stupid();
    let parent_branchname = super::get_stgit_parent(&repo.config_snapshot(), old_branchname);
    let parent_branchname = parent_branchname
        .map(|name| PartialRefName::from_str(name.as_str()))
        .transpose()?;

    if let Ok(stack) = Stack::from_branch(
        repo,
        Some(old_branchname),
        InitializationPolicy::RequireInitialized,
    ) {
        let state_commit = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference")
            .into_fully_peeled_id()?
            .object()?
            .try_into_commit()?;
        repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: format!("rename {old_branchname} to {new_branchname}").into(),
                },
                expected: gix::refs::transaction::PreviousValue::MustNotExist,
                new: gix::refs::Target::Peeled(state_commit.id),
            },
            name: gix::refs::FullName::try_from(state_refname_from_branch_name(
                new_branchname.as_ref(),
            ))?,
            deref: false,
        })?;
        stupid
            .config_rename_section(
                &format!("branch.{old_branchname}.stgit"),
                &format!("branch.{new_branchname}.stgit"),
            )
            .ok();
        stupid.branch_move(Some(old_branchname.as_ref()), new_branchname.as_ref())?;
        stack.deinitialize()?;
    } else {
        stupid.branch_move(Some(old_branchname.as_ref()), new_branchname.as_ref())?;
    }
    super::set_stgit_parent(repo, new_branchname, parent_branchname.as_ref())?;
    Ok(())
}
