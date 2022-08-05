// SPDX-License-Identifier: GPL-2.0-only

//! `stg squash` implementation.

use std::fmt::Write;
use std::str::FromStr;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    commit::CommitExtended,
    index::TemporaryIndex,
    patchedit,
    patchname::PatchName,
    patchrange, print_info_message,
    repo::RepositoryExtended,
    signature::SignatureExtended,
    stack::{Stack, StackStateAccess, StackTransaction},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "squash",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackManipulation,
        },
    )
}

fn make() -> clap::Command<'static> {
    let command = clap::Command::new("squash")
        .about("Squash two or more patches into one")
        .long_about(
            "Squash two or more patches, creating one patch with their combined \
            changes.\n\
            \n\
            The squash process, at a high level:\n\
            \n  \
            1. Pop all the given patches, plus any other patches on top of them.\n\
            \n  \
            2. Push the given patches in the order they were given on the command \
            line. This establishes a tree containing the combined changes from the \
            given patches.\n\
            \n  \
            3. Replace given patches with a new, squashed patch.\n\
            \n  \
            4. Allow the user to interactively edit the commit message of the new, \
            squashed patch.\n\
            \n  \
            5. Push other patches that were popped in step (1), if any.\n\
            \n\
            Conflicts can occur whenever a patch is pushed; this is, in steps (2) and \
            (5). If conflicts occur, the squash command will halt such that the \
            conflicts may be resolved manually.",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to squash")
                .value_name("patch")
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .required(true),
        )
        .arg(
            Arg::new("name")
                .long("name")
                .short('n')
                .help("Use <name> for the squashed patch")
                .value_name("name")
                .value_parser(PatchName::from_str),
        );
    patchedit::add_args(command, true, true)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let config = repo.config()?;

    repo.check_repository_state()?;
    repo.check_conflicts()?;
    stack.check_head_top_mismatch()?;

    let squash_patchnames: Vec<PatchName> = patchrange::patches_from_specs(
        matches
            .get_many::<patchrange::Specification>("patchranges")
            .expect("clap ensures two or more patches"),
        &stack,
        patchrange::Allow::All,
    )?;

    let patchname: Option<PatchName> = matches.get_one::<PatchName>("name").cloned();

    if let Some(patchname) = patchname.as_ref() {
        if !squash_patchnames.contains(patchname) {
            if let Some(colliding_patchname) = stack.collides(patchname) {
                return Err(anyhow!("Patch name `{colliding_patchname}` already taken"));
            }
        }
    }

    if squash_patchnames.len() < 2 {
        return Err(anyhow!("Need at least two patches"));
    }

    if matches.contains_id("save-template") {
        let first_patch_commit = stack.get_patch_commit(&squash_patchnames[0]);
        if let patchedit::EditOutcome::TemplateSaved(template_path) =
            patchedit::EditBuilder::default()
                .existing_patch_commit(first_patch_commit) // Dummy commit
                .allow_diff_edit(false)
                .allow_template_save(true)
                .template_patchname(patchname.as_ref())
                .default_author(git2::Signature::make_author(Some(&config), matches)?)
                .default_message(prepare_message(&stack, &squash_patchnames)?)
                .edit(&stack, &repo, matches)?
        {
            let template_path = template_path.to_string_lossy();
            print_info_message(
                matches,
                &format!("patch template saved to `{template_path}`"),
            );
            Ok(())
        } else {
            panic!("expected template to be saved")
        }
    } else {
        let should_push_squashed = stack
            .applied()
            .iter()
            .any(|pn| squash_patchnames.contains(pn));

        stack
            .setup_transaction()
            .allow_conflicts(true)
            .use_index_and_worktree(true)
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| {
                squash(
                    trans,
                    &config,
                    matches,
                    &squash_patchnames,
                    patchname.as_ref(),
                    should_push_squashed,
                )?;
                Ok(())
            })
            .execute("squash")?;
        Ok(())
    }
}

fn prepare_message<'repo>(
    stack_state: &impl StackStateAccess<'repo>,
    patchnames: &[PatchName],
) -> Result<String> {
    let mut squash_message = String::new();
    for (i, patchname) in patchnames.iter().enumerate() {
        let commit = stack_state.get_patch_commit(patchname);
        let message = commit.message_ex();
        let message = message.decode()?;
        let message = message.trim_end();
        let patch_number = i + 1;
        write!(
            squash_message,
            "# Commit message from patch #{patch_number}: {patchname}\n\
             {message}\n\
             \n"
        )?;
    }
    Ok(squash_message)
}

pub(super) fn squash(
    trans: &mut StackTransaction,
    config: &git2::Config,
    matches: &ArgMatches,
    patchnames: &[PatchName],
    patchname: Option<&PatchName>,
    should_push_squashed: bool,
) -> Result<PatchName> {
    let (new_patchname, commit_id, to_push) = if let Some((new_patchname, commit_id)) =
        try_squash(trans, config, matches, patchnames, patchname)?
    {
        // Squashed commit could be created with simple merges, so the
        // constituent patches can just be deleted.
        let to_push = trans.delete_patches(|pn| patchnames.contains(pn))?;
        (new_patchname, commit_id, to_push)
    } else {
        // Simple approach failed, need to do pops and pushes...
        let to_push = trans.pop_patches(|pn| patchnames.contains(pn))?;
        trans.push_patches(patchnames, false)?;
        if let Some((new_patchname, commit_id)) =
            try_squash(trans, config, matches, patchnames, patchname)?
        {
            let popped_extra = trans.delete_patches(|pn| patchnames.contains(pn))?;
            assert!(popped_extra.is_empty());
            (new_patchname, commit_id, to_push)
        } else {
            return Err(crate::stack::Error::CausedConflicts(
                "Conflicts while squashing".to_string(),
            )
            .into());
        }
    };

    trans.new_unapplied(&new_patchname, commit_id, 0)?;

    let mut to_push = to_push;

    if should_push_squashed {
        to_push.insert(0, new_patchname.clone());
    }

    trans.push_patches(&to_push, false)?;

    Ok(new_patchname)
}

fn try_squash(
    trans: &StackTransaction,
    config: &git2::Config,
    matches: &ArgMatches,
    patchnames: &[PatchName],
    patchname: Option<&PatchName>,
) -> Result<Option<(PatchName, git2::Oid)>> {
    let repo = trans.repo();
    let base_commit = trans.get_patch_commit(&patchnames[0]);
    if let Some(tree_id) = repo.with_temp_index_file(|temp_index| {
        let stupid = repo.stupid();
        let stupid = stupid.with_index_path(temp_index.path().unwrap());
        stupid.read_tree(base_commit.tree_id())?;
        for commit in patchnames[1..].iter().map(|pn| trans.get_patch_commit(pn)) {
            let parent = commit.parent(0)?;
            if parent.tree_id() != commit.tree_id()
                && !stupid.apply_treediff_to_index(parent.tree_id(), commit.tree_id())?
            {
                return Ok(None);
            }
        }

        let tree_id = stupid.write_tree()?;
        Ok(Some(tree_id))
    })? {
        if let patchedit::EditOutcome::Committed {
            patchname,
            commit_id,
        } = patchedit::EditBuilder::default()
            .override_parent_id(base_commit.parent_id(0).expect("first patch has a parent"))
            .override_tree_id(tree_id)
            .allow_implicit_edit(true)
            .allow_diff_edit(false)
            .allow_template_save(false)
            .template_patchname(patchname)
            .extra_allowed_patchnames(patchnames)
            .default_author(git2::Signature::make_author(Some(config), matches)?)
            .default_message(prepare_message(trans, patchnames)?)
            .edit(trans, repo, matches)?
        {
            Ok(Some((patchname, commit_id)))
        } else {
            panic!("expected edit to commit, not save template")
        }
    } else {
        Ok(None)
    }
}
