// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --rename` implementation.

use std::str::FromStr;

use anyhow::{Context, Result};
use bstr::{BString, ByteSlice};

use crate::{
    ext::RepositoryExtended,
    stack::{state_refname_from_branch_name, InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
    wrap::PartialRefName,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--rename")
        .short_flag('r')
        .override_usage(super::super::make_usage(
            "stg branch --rename",
            &["[old-name] <new-name>"],
        ))
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
        repo.get_branch(names[0])?;
        (names[0], names[1])
    } else {
        current_branch_name = repo.get_current_branch()?.get_branch_partial_name()?;
        (&current_branch_name, names[0])
    };

    let stupid = repo.stupid();
    let parent_branchname = super::get_stgit_parent(&repo.config_snapshot(), old_branchname);
    let parent_branchname = parent_branchname
        .map(|name| PartialRefName::from_str(name.as_str()))
        .transpose()?;

    if let Ok(stack) = Stack::from_branch_name(
        repo,
        old_branchname,
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

        let mut local_config_file = repo.local_config_file().context("opening local config")?;
        let old_section_name = format!("{old_branchname}.stgit");
        let old_section_name = old_section_name.as_bytes().as_bstr();
        if local_config_file
            .section("branch", Some(old_section_name))
            .is_ok()
        {
            let new_section_name =
                std::borrow::Cow::Owned(BString::from(format!("{new_branchname}.stgit")));
            local_config_file
                .rename_section(
                    "branch",
                    Some(old_section_name),
                    "branch",
                    Some(new_section_name),
                )
                .with_context(|| {
                    format!(
                        "renaming config section `branch.{old_branchname}.stgit` \
                         to `branch.{new_branchname}.stgit`"
                    )
                })?;
            repo.write_local_config(local_config_file)
                .context("writing local config")?;
        }

        stupid.branch_move(Some(old_branchname.as_ref()), new_branchname.as_ref())?;
        stack.deinitialize()?;
    } else {
        stupid.branch_move(Some(old_branchname.as_ref()), new_branchname.as_ref())?;
    }
    super::set_stgit_parent(repo, new_branchname, parent_branchname.as_ref())?;
    Ok(())
}
