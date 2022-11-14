// SPDX-License-Identifier: GPL-2.0-only

//! `stg repair` implementation.

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use indexmap::{indexset, IndexSet};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    print_info_message, print_warning_message,
    stack::{Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "repair",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Repair stack after branch is modified with git commands")
        .long_about(
            "If a branch with a StGit stack is modified with certain git commands such \
             as git-commit(1), git-pull(1), git-merge(1), or git-rebase(1), the StGit \
             stack metadata will become inconsistent with the branch state. There are \
             a few options for resolving this kind of situation:\n\
             \n\
             1. Use 'stg undo' to undo the effect of the git commands. Or similarly \
             use 'stg reset' to reset the stack/branch to any previous stack state.\n\
             \n\
             2. Use `stg repair`. This will repair the StGit stack metadata to \
             accommodate the modifications to the branch made by the git commands. \
             Specifically, it will do the following:\n\
             \n\
             - If regular git commits were made on top of the stack of StGit patches \
             (i.e. by using plain `git commit`), `stg repair` will convert those \
             commits to StGit patches, preserving their content.\n\
             \n\
             - However, merge commits cannot become patches. So if a merge was \
             committed on top of the stack, `stg repair` will mark all patches below \
             the merge commit as unapplied, since they are no longer reachable. An \
             alternative when this is not the desired behavior is to use `stg undo` to \
             first get rid of the offending merge and then run `stg repair` again.\n\
             \n\
             - The applied patches are supposed to be precisely those that are \
             reachable from the branch head. If, for example, git-reset(1) was used to \
             move the head, some applied patches may no longer be reachable and some \
             unapplied patches may have become reachable. In this case, `stg repair` \
             will correct the applied/unapplied state of such patches.\n\
             \n\
             `stg repair` will repair these inconsistencies reliably, so there are \
             valid workflows where git commands are used followed by `stg repair`. For \
             example, new patches can be created by first making commits with a \
             graphical commit tool and then running `stg repair` to convert those \
             commits into patches.",
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let config = repo.config()?;
    if stack.is_protected(&config) {
        return Err(anyhow!(
            "This branch is protected. Modification is not permitted."
        ));
    }

    let patchname_len_limit = PatchName::get_length_limit(&config);

    // Find commits that are not patches as well as applied patches.

    // Commits to definitely patchify
    let mut patchify: Vec<git2::Commit> = Vec::new();

    // Commits to patchify if a patch is found below
    let mut maybe_patchify: Vec<git2::Commit> = Vec::new();

    let mut applied: Vec<PatchName> = Vec::new();

    let mut commit = stack.branch_head.clone();

    while commit.parent_count() == 1 {
        let parent = commit.parent(0)?;
        if let Some(patchname) = stack
            .all_patches()
            .find(|pn| stack.get_patch_commit(pn).id() == commit.id())
        {
            applied.push(patchname.clone());
            patchify.append(&mut maybe_patchify);
        } else {
            maybe_patchify.push(commit.clone());
        }

        commit = parent;

        if stack.base().id() == commit.id() {
            // Reaching the original stack base can happen if, for example, the first
            // applied patch is amended. In this case, any commits descending from the
            // stack base should be patchified.
            patchify.append(&mut maybe_patchify);
            break;
        }
    }

    applied.reverse();
    patchify.reverse();

    // Find patches unreachable behind a merge.
    if commit.id() != stack.base().id() {
        let merge_commit_id = commit.id();
        let mut todo = indexset! { merge_commit_id };
        let mut seen = indexset! {};
        let mut unreachable = 0;

        while !todo.is_empty() {
            let todo_commit_id = todo.pop().unwrap();
            seen.insert(todo_commit_id);
            let commit = stack.repo.find_commit(todo_commit_id)?;
            let parents: IndexSet<git2::Oid> = commit.parent_ids().collect();
            let unseen_parents: IndexSet<git2::Oid> = parents.difference(&seen).copied().collect();
            todo = todo.union(&unseen_parents).copied().collect();

            if stack
                .all_patches()
                .any(|pn| stack.get_patch_commit(pn).id() == todo_commit_id)
            {
                unreachable += 1;
            }
        }

        if unreachable > 0 {
            print_warning_message(
                matches,
                &format!(
                    "{unreachable} patch{} hidden below the merge commit {merge_commit_id} \
                     and will be considered unapplied",
                    if unreachable == 1 { " is" } else { "es are" },
                ),
            );
        }
    }

    let mut unapplied: Vec<PatchName> = stack
        .applied()
        .iter()
        .filter(|&pn| !applied.contains(pn))
        .cloned()
        .collect();

    unapplied.extend(
        stack
            .unapplied()
            .iter()
            .filter(|&pn| !applied.contains(pn))
            .cloned(),
    );

    let hidden: Vec<PatchName> = stack
        .hidden()
        .iter()
        .filter(|&pn| !applied.contains(pn))
        .cloned()
        .collect();

    applied
        .iter()
        .filter(|&pn| !stack.applied().contains(pn))
        .for_each(|pn| print_info_message(matches, &format!("`{pn}` is now applied")));
    unapplied
        .iter()
        .filter(|&pn| !stack.unapplied().contains(pn))
        .for_each(|pn| print_info_message(matches, &format!("`{pn}` is now unapplied")));

    stack
        .setup_transaction()
        .use_index_and_worktree(false)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            trans.repair_appliedness(applied, unapplied, hidden);

            // Make patches of any linear sequence of commits on top of a patch.
            if !patchify.is_empty() {
                print_info_message(
                    matches,
                    &format!(
                        "Creating {} new patch{}",
                        patchify.len(),
                        if patchify.len() == 1 { "" } else { "es" }
                    ),
                );

                for commit in patchify {
                    let message = commit.message_raw_bytes().to_str_lossy();
                    let allow = &[];
                    let disallow: Vec<_> = trans.all_patches().collect();
                    let patchname = PatchName::make(&message, true, patchname_len_limit)
                        .uniquify(allow, &disallow);
                    trans.new_applied(&patchname, commit.id())?;
                }
            }
            Ok(())
        })
        .execute("repair")?;

    Ok(())
}
