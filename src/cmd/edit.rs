// SPDX-License-Identifier: GPL-2.0-only

//! `stg edit` implementation.

use anyhow::{Context, Result};
use clap::{Arg, ArgMatches, ValueHint};

use crate::{
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchedit, PatchLocator, SingleRevisionSpec},
    stack::{Error, InitializationPolicy, Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "edit",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    let app = clap::Command::new("edit")
        .about("Edit a patch")
        .long_about(
            "Edit a patch. Various aspects of a patch may be edited, including the \
             message, author, patch name, or even the patch's diff.\n\
             \n\
             By default, the topmost patch is edited.\n\
             \n\
             With no options or when '--edit' is specified, the patch details are \
             edited interactively. Alternatively, command line options may be used to \
             modify the patch non-interactively.\n\
             \n\
             The '--diff' option causes the patch's diff to be appended to the patch \
             description when editing interactively. This diff may be edited \
             interactively (or just used as a reference when editing the patch's \
             message). The StGit attempts to apply the modified diff to the patch's \
             parent tree. If the updated diff does not apply, no changes are made to \
             the patch and the edited patch is saved to a file which may be corrected \
             and then fed-back into `stg edit --file`.",
        )
        .arg(
            Arg::new("patch")
                .help("Patch to edit")
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchLocator))
                .value_hint(ValueHint::Other),
        );
    patchedit::add_args(app, true, true).arg(
        Arg::new("set-tree")
            .long("set-tree")
            .short('t')
            .help("Set patch's tree to treeish")
            .long_help(
                "Set the patch's git tree to the specified treeish without changing \
                 the tree of any other patches. When used on the top patch, the index \
                 and work tree will be updated to match the new tree. This low-level \
                 option is primarily meant to be used by tools built on top of StGit, \
                 such as the Emacs mode. See also the '--set-tree' flag of 'stg \
                 push'.",
            )
            .allow_hyphen_values(true)
            .value_parser(clap::value_parser!(SingleRevisionSpec))
            .value_name("treeish"),
    )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;
    stack.check_head_top_mismatch()?;

    let patchname = if let Some(patch_loc) = matches.get_one::<PatchLocator>("patch") {
        patch_loc.resolve_name(&stack)?
    } else if let Some(top_patchname) = stack.applied().last() {
        top_patchname.clone()
    } else {
        return Err(Error::NoAppliedPatches.into());
    };

    let patch_commit = stack.get_patch_commit(&patchname);

    let tree_id = if let Some(spec) = matches.get_one::<SingleRevisionSpec>("set-tree") {
        spec.resolve_tree(&repo, &stack)
            .context("resolving `--set-tree` value")?
            .id
    } else {
        patch_commit.tree_id()?.detach()
    };

    match patchedit::EditBuilder::default()
        .original_patchname(Some(&patchname))
        .existing_patch_commit(patch_commit)
        .allow_diff_edit(true)
        .allow_implicit_edit(!matches.contains_id("set-tree"))
        .allow_template_save(true)
        .override_tree_id(tree_id)
        .edit(&stack, &repo, matches)?
    {
        patchedit::EditOutcome::TemplateSaved(_) => Ok(()),
        patchedit::EditOutcome::Edited {
            new_patchname,
            new_commit_id,
        } => {
            if new_patchname.is_some() || new_commit_id.is_some() {
                stack
                    .setup_transaction()
                    .allow_conflicts(true)
                    .use_index_and_worktree(true)
                    .with_output_stream(get_color_stdout(matches))
                    .transact(|trans| {
                        let popped = if let Some(pos) =
                            trans.applied().iter().position(|pn| pn == &patchname)
                        {
                            let to_pop = trans.applied()[pos + 1..].to_vec();
                            let popped_extra = trans.pop_patches(|pn| to_pop.contains(pn))?;
                            assert!(popped_extra.is_empty());
                            to_pop
                        } else {
                            vec![]
                        };

                        let patchname = if let Some(new_patchname) = new_patchname.as_ref() {
                            trans.rename_patch(&patchname, new_patchname)?;
                            // TODO: log stack state here?
                            new_patchname
                        } else {
                            &patchname
                        };

                        if let Some(commit_id) = new_commit_id {
                            trans.update_patch(patchname, commit_id)?;
                        }

                        if matches.contains_id("set-tree") {
                            trans.push_tree_patches(&popped)
                        } else {
                            trans.push_patches(&popped, false)
                        }
                    })
                    .execute(&format!("edit: {patchname}"))?;
            }
            Ok(())
        }
    }
}
