// SPDX-License-Identifier: GPL-2.0-only

//! Modify the StGit stack state atomically.
//!
//! Modifying the StGit stack typically involves performing a sequence of fallible
//! operations, where each operation depends on the previous. The stack transaction
//! mechanism found in this module allows these operations to be performed in an
//! all-or-nothing fashion such that the stack, working tree, and index will either
//! successfully transition to their new state or fallback to their starting state.
//!
//! The entry point to stack transactions is via the `Stack::setup_transaction()`
//! method. The transaction operations are defined in a closure passed to the (required)
//! `transact()` method. And the transaction is finalized via the `execute()` method.
//!
//! # Example
//!
//! ```no_run
//! let new_stack = stack
//!     .setup_transaction()
//!     .with_output_stream(...)
//!     ...  // Transaction option method calls
//!     .transact(|trans| {
//!         // Call StackTransaction methods
//!         ...
//!     })
//!     .execute("<reflog message>")?;
//! ```

mod builder;
mod options;
mod ui;

use std::{collections::BTreeMap, rc::Rc};

use anyhow::{anyhow, Result};
use indexmap::IndexSet;

pub(crate) use self::builder::TransactionBuilder;
use self::{
    options::{ConflictMode, TransactionOptions},
    ui::TransactionUserInterface,
};
use super::{state::StackState, StackAccess};
use crate::{
    ext::{CommitExtended, RepositoryExtended},
    patch::PatchName,
    stack::{PatchState, Stack, StackStateAccess},
    stupid::{Stupid, StupidContext},
    wrap::Branch,
};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("{0}")]
    CheckoutConflicts(String),

    #[error("{msg}")]
    TransactionHalt { msg: String, conflicts: bool },
}

/// Stack transaction state.
pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    ui: TransactionUserInterface,
    options: TransactionOptions,

    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    updated_patches: BTreeMap<PatchName, Option<PatchState<'repo>>>,
    updated_head: Option<Rc<gix::Commit<'repo>>>,
    updated_base: Option<Rc<gix::Commit<'repo>>>,

    current_tree_id: gix::ObjectId,
    error: Option<anyhow::Error>,
}

/// Status of a pushed patch.
///
/// Pushing a patch successfully may result in one of several states. This status is
/// used for control flow in the event of conflicts as well as to fine-tune the
/// user-facing output of push operations.
#[derive(Debug, PartialEq, Clone, Copy)]
enum PushStatus {
    /// The pushed patch is newly added to the stack.
    New,

    /// The pushed patch's changes have been determined to have already been merged into
    /// the stack's base tree.
    AlreadyMerged,

    /// The push resulted in merge conflicts.
    Conflict,

    /// The push resulted in the patch's diff becoming empty.
    Empty,

    /// The push resulted in the patch's diff being modified.
    Modified,

    /// The push resulted in the patch's diff remaining the same.
    Unmodified,
}

/// Context for executing a [`StackTransaction`].
///
/// Wraps [`StackTransaction`] to ensure [`ExecuteContext::execute()`] is called after
/// [`TransactionBuilder::transact()`].
pub(crate) struct ExecuteContext<'repo>(StackTransaction<'repo>);

impl<'repo> ExecuteContext<'repo> {
    /// Execute the transaction.
    ///
    /// If any of the transaction operations (i.e. from `transact()`) fail, the stack,
    /// index, and worktree state will be rolled back.
    ///
    /// A new `Stack` instance is returned.
    pub(crate) fn execute(self, reflog_msg: &str) -> Result<Stack<'repo>> {
        let transaction = self.0;

        // Check consistency
        for (patchname, oid) in &transaction.updated_patches {
            if oid.is_none() {
                assert!(transaction.stack.has_patch(patchname));
            } else {
                assert!(transaction.all_patches().any(|pn| pn == patchname));
            }
        }

        // Need to cache a few bits of transaction state for later use.
        let trans_head = transaction.head().clone();
        let trans_head_tree_id = trans_head.tree_id()?.detach();
        let trans_top_patchname = transaction.applied().last().cloned();

        let StackTransaction {
            stack,
            ui,
            options,
            applied,
            unapplied,
            hidden,
            updated_patches,
            current_tree_id,
            error,
            ..
        } = transaction;

        let repo = stack.repo;
        let stack_top_patchname = stack.applied().last().cloned();
        let rollback_tree_id = stack.get_branch_head().tree_id()?.detach();

        // Only proceed for halt errors
        let has_conflicts = if let Some(err) = &error {
            match err.downcast_ref::<Error>() {
                Some(Error::TransactionHalt { conflicts, .. }) => *conflicts,
                _ => return Err(error.unwrap()),
            }
        } else {
            false
        };

        // Log external modifications
        let mut stack = if stack.is_head_top() {
            stack
        } else {
            stack.log_external_mods(None)?
        };

        // Roll back by checking out the stack top tree prior to any changes from this
        // stack transaction. The stack state reference must only be updated after all
        // possible rollback points are passed successfully.
        let rollback = |old_tree_id, err| -> anyhow::Error {
            if let Err(checkout_err) = checkout(
                repo,
                &options,
                stack_top_patchname.as_ref(),
                trans_top_patchname.as_ref(),
                old_tree_id,
                rollback_tree_id,
            ) {
                return checkout_err;
            }
            if let Err(print_err) = ui.print_rolled_back(stack_top_patchname.as_ref()) {
                return print_err;
            }
            anyhow!(
                "{err:#};\n\
                 command aborted (all changes rolled back)"
            )
        };

        if options.set_head && options.use_index_and_worktree {
            if !options.allow_bad_head {
                stack.check_head_top_mismatch()?;
            }
            checkout(
                repo,
                &options,
                stack_top_patchname.as_ref(),
                trans_top_patchname.as_ref(),
                current_tree_id,
                trans_head_tree_id,
            )
            .map_err(|e| rollback(current_tree_id, e))?;
        }

        crate::signal::critical(|| {
            // Commit updated stack state
            let conflict_msg;
            let state_reflog_msg = if has_conflicts {
                conflict_msg = format!("{reflog_msg} (CONFLICT)");
                &conflict_msg
            } else {
                reflog_msg
            };
            let branch_ref_name = stack.get_branch_refname().to_owned();
            let prev_state_commit = repo
                .find_reference(stack.get_stack_refname())?
                .peel_to_commit()?;
            let state = stack.state_mut();
            for (patchname, maybe_patch) in &updated_patches {
                if let Some(patch) = maybe_patch {
                    state.patches.insert(patchname.clone(), patch.clone());
                } else {
                    state.patches.remove(patchname);
                }
            }
            state.prev = Some(Rc::new(prev_state_commit));
            state.head = trans_head.clone();
            state.applied = applied;
            state.unapplied = unapplied;
            state.hidden = hidden;
            let state_commit_id = state.commit(repo, None, state_reflog_msg)?;

            // Update various refs as a single transaction. This reference transaction is
            // not quite atomic--it is possible for some, but not all references to be
            // updated--but atomic enough in practice for our purposes.
            let mut ref_edits = Vec::new();
            let log = gix::refs::transaction::LogChange {
                mode: gix::refs::transaction::RefLog::AndReference,
                force_create_reflog: false,
                message: state_reflog_msg.into(),
            };
            for (patchname, maybe_patch) in &updated_patches {
                let change = if let Some(patch) = maybe_patch {
                    gix::refs::transaction::Change::Update {
                        log: log.clone(),
                        expected: gix::refs::transaction::PreviousValue::Any, // TODO?
                        new: gix::refs::Target::Object(patch.commit.id),
                    }
                } else {
                    gix::refs::transaction::Change::Delete {
                        expected: gix::refs::transaction::PreviousValue::Any,
                        log: gix::refs::transaction::RefLog::AndReference,
                    }
                };
                ref_edits.push(gix::refs::transaction::RefEdit {
                    change,
                    name: gix::refs::FullName::try_from(stack.patch_refname(patchname))
                        .expect("patch reference name is valid"),
                    deref: false,
                });
            }

            ref_edits.push(gix::refs::transaction::RefEdit {
                change: gix::refs::transaction::Change::Update {
                    log: log.clone(),
                    expected: if let Some(prev_state_commit) = stack.state_mut().prev.as_ref() {
                        gix::refs::transaction::PreviousValue::ExistingMustMatch(
                            gix::refs::Target::Object(prev_state_commit.id),
                        )
                    } else {
                        gix::refs::transaction::PreviousValue::MustNotExist
                    },
                    new: gix::refs::Target::Object(state_commit_id),
                },
                name: gix::refs::FullName::try_from(stack.get_stack_refname())
                    .expect("stack reference name is valid"),
                deref: false,
            });

            if options.set_head {
                ref_edits.push(gix::refs::transaction::RefEdit {
                    change: gix::refs::transaction::Change::Update {
                        log,
                        expected: gix::refs::transaction::PreviousValue::Any,
                        new: gix::refs::Target::Object(trans_head.id),
                    },
                    name: branch_ref_name.clone(),
                    deref: false,
                })
            }

            repo.edit_references(ref_edits)?;

            if options.set_head {
                stack.update_head(
                    Branch::wrap(repo.find_reference(&branch_ref_name)?),
                    trans_head.clone(),
                );
            }

            Ok(())
        })
        .map_err(|e| rollback(trans_head_tree_id, e))?;

        if let Some(err) = error {
            Err(err)
        } else {
            if !ui.printed_top() {
                if let Some(top_patchname) = trans_top_patchname.as_ref() {
                    ui.print_top(top_patchname)?;
                }
            }

            Ok(stack)
        }
    }
}

fn checkout(
    repo: &gix::Repository,
    options: &TransactionOptions,
    stack_top: Option<&PatchName>,
    trans_top: Option<&PatchName>,
    current_tree_id: gix::ObjectId,
    tree_id: gix::ObjectId,
) -> Result<()> {
    let stupid = repo.stupid();

    if current_tree_id == tree_id && !options.discard_changes {
        match options.conflict_mode {
            ConflictMode::Allow => {}
            ConflictMode::AllowIfSameTop => {
                if trans_top.is_none() || trans_top != stack_top {
                    stupid.statuses(None)?.check_conflicts()?;
                }
            }
            ConflictMode::Disallow => stupid.statuses(None)?.check_conflicts()?,
        };
    } else if options.discard_changes {
        stupid.read_tree_checkout_hard(tree_id)?;
    } else {
        stupid.update_index_refresh()?;
        stupid
            .read_tree_checkout(current_tree_id, tree_id)
            .map_err(|e| Error::CheckoutConflicts(format!("{e:#}")))?;
    }

    Ok(())
}

impl<'repo> StackTransaction<'repo> {
    /// Get an immutable reference to the original stack.
    pub(crate) fn stack(&self) -> &Stack<'repo> {
        &self.stack
    }

    /// Get a reference to the repo.
    pub(crate) fn repo(&self) -> &'repo gix::Repository {
        self.stack.repo
    }

    /// Reset stack to a previous stack state.
    pub(crate) fn reset_to_state(&mut self, state: StackState<'repo>) -> Result<()> {
        for pn in self.all_patches().cloned().collect::<Vec<_>>() {
            self.updated_patches.insert(pn, None);
        }
        let StackState {
            prev: _prev,
            head,
            applied,
            unapplied,
            hidden,
            patches,
        } = state;
        self.updated_base = Some(if let Some(pn) = applied.first() {
            Rc::new(patches[pn].commit.get_parent_commit()?)
        } else {
            head.clone()
        });
        self.updated_head = Some(head);
        for (pn, patch_state) in patches {
            self.updated_patches.insert(pn, Some(patch_state));
        }
        self.applied = applied;
        self.unapplied = unapplied;
        self.hidden = hidden;
        Ok(())
    }

    /// Reset stack to previous stack state, but only for the specified patch names.
    pub(crate) fn reset_to_state_partially<P>(
        &mut self,
        state: &StackState<'repo>,
        patchnames: &[P],
    ) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        let only_patches: IndexSet<_> = patchnames.iter().map(AsRef::as_ref).collect();
        let state_patches: IndexSet<_> = state.all_patches().collect();
        let to_reset_patches: IndexSet<_> =
            state_patches.intersection(&only_patches).copied().collect();
        let existing_patches: IndexSet<_> = self.all_patches().cloned().collect();
        let existing_patches: IndexSet<_> = existing_patches.iter().collect();
        let original_applied_order = self.applied.clone();
        let to_delete_patches: IndexSet<_> = existing_patches
            .difference(&to_reset_patches)
            .copied()
            .collect::<IndexSet<_>>()
            .intersection(&only_patches)
            .copied()
            .collect();

        let matching_patches: IndexSet<_> = state
            .patches
            .iter()
            .filter_map(|(pn, patch_state)| {
                if self.has_patch(pn) && self.get_patch_commit_id(pn) == patch_state.commit.id {
                    Some(pn)
                } else {
                    None
                }
            })
            .collect();

        self.pop_patches(|pn| {
            if !only_patches.contains(pn) {
                false
            } else if !to_delete_patches.contains(pn) {
                true
            } else {
                !matching_patches.contains(pn)
            }
        })?;

        self.delete_patches(|pn| to_delete_patches.contains(pn))?;

        for pn in to_reset_patches {
            if existing_patches.contains(pn) {
                if matching_patches.contains(pn) {
                    continue;
                }
            } else if state.hidden.contains(pn) {
                self.hidden.push(pn.clone());
            } else {
                self.unapplied.push(pn.clone());
            }
            self.updated_patches
                .insert(pn.clone(), Some(state.patches[pn].clone()));
            self.ui.print_updated(pn, self.applied())?;
        }

        let to_push_patches: Vec<_> = original_applied_order
            .iter()
            .filter(|pn| self.unapplied.contains(pn) || self.hidden.contains(pn))
            .collect();

        self.push_patches(&to_push_patches, false)?;

        Ok(())
    }

    /// Update a patch with a different commit object.
    ///
    /// Any notes associated with the patch's previous commit are copied to the new
    /// commit.
    pub(crate) fn update_patch(
        &mut self,
        patchname: &PatchName,
        commit_id: gix::ObjectId,
    ) -> Result<()> {
        let commit = self.stack.repo.find_commit(commit_id)?;
        let old_commit = self.get_patch_commit(patchname);
        // Failure to copy is okay. The old commit may not have a note to copy.
        self.stack
            .repo
            .stupid()
            .notes_copy(old_commit.id, commit_id)
            .ok();
        self.updated_patches.insert(
            patchname.clone(),
            Some(PatchState {
                commit: Rc::new(commit),
            }),
        );
        self.ui.print_updated(patchname, self.applied())?;
        Ok(())
    }

    /// Add new patch to the top of the stack.
    ///
    /// The commit for the new patch must be parented by the former top commit of the
    /// stack.
    pub(crate) fn new_applied(&mut self, patchname: &PatchName, oid: gix::ObjectId) -> Result<()> {
        let commit = self.stack.repo.find_commit(oid)?;
        assert_eq!(commit.parent_ids().next().unwrap().detach(), self.top().id);
        self.applied.push(patchname.clone());
        self.updated_patches.insert(
            patchname.clone(),
            Some(PatchState {
                commit: Rc::new(commit),
            }),
        );
        self.ui.print_pushed(patchname, PushStatus::New, true)?;
        Ok(())
    }

    /// Add new unapplied patch to the stack.
    ///
    /// The new patch may be pushed to any position in the unapplied list.
    pub(crate) fn new_unapplied(
        &mut self,
        patchname: &PatchName,
        commit_id: gix::ObjectId,
        insert_pos: usize,
    ) -> Result<()> {
        let commit = self.stack.repo.find_commit(commit_id)?;
        self.unapplied.insert(insert_pos, patchname.clone());
        self.updated_patches.insert(
            patchname.clone(),
            Some(PatchState {
                commit: Rc::new(commit),
            }),
        );
        self.ui.print_popped(std::slice::from_ref(patchname))?;
        Ok(())
    }

    /// Push patches, but keep their existing trees.
    pub(crate) fn push_tree_patches<P>(&mut self, patchnames: &[P]) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        for (i, patchname) in patchnames.iter().enumerate() {
            let is_last = i + 1 == patchnames.len();
            self.push_tree(patchname.as_ref(), is_last)?;
        }
        Ok(())
    }

    /// Push patch keeping its existing tree.
    ///
    /// For a normal patch push, the patch's diff is applied to the topmost patch's tree
    /// which typically results in a new tree being associated with the pushed patch's
    /// commit. For this operation, instead of applying the pushed patch's diff to the
    /// topmost patch's tree, the pushed patch's tree is preserved as-is.
    pub(crate) fn push_tree(&mut self, patchname: &PatchName, is_last: bool) -> Result<()> {
        let patch_commit = self.get_patch_commit(patchname);
        let repo = self.stack.repo;
        let parent = patch_commit.get_parent_commit()?;
        let is_empty = parent.tree_id()? == patch_commit.tree_id()?;

        let push_status = if patch_commit.parent_ids().next().unwrap() == self.top().id() {
            PushStatus::Unmodified
        } else {
            let author = patch_commit.author_strict()?;
            let default_committer = repo.get_committer()?;
            let committer = if self.options.committer_date_is_author_date {
                let mut committer = default_committer.to_owned()?;
                committer.time = author.time;
                committer
            } else {
                default_committer.to_owned()?
            };
            let message = patch_commit.message_ex();
            let parent_ids = [self.top().id];
            let new_commit_id = repo.commit_ex(
                &author,
                &committer,
                &message,
                patch_commit.tree_id()?.detach(),
                parent_ids,
            )?;

            let commit = repo.find_commit(new_commit_id)?;
            repo.stupid()
                .notes_copy(patch_commit.id, new_commit_id)
                .ok();
            self.updated_patches.insert(
                patchname.clone(),
                Some(PatchState {
                    commit: Rc::new(commit),
                }),
            );

            PushStatus::Modified
        };

        let push_status = if is_empty {
            PushStatus::Empty
        } else {
            push_status
        };

        if let Some(pos) = self.unapplied.iter().position(|pn| pn == patchname) {
            self.unapplied.remove(pos);
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == patchname) {
            self.hidden.remove(pos);
        } else {
            panic!("push_tree `{patchname}` was not in unapplied or hidden");
        }

        self.applied.push(patchname.clone());

        self.ui.print_pushed(patchname, push_status, is_last)
    }

    /// Update patches' applied, unapplied, and hidden dispositions.
    ///
    /// This is used by `stg repair` to account for changes to the repository made by
    /// StGit-unaware git tooling. All existing patchnames must be present in the
    /// updated lists and no new patchnames may be introduced to the updated lists. I.e.
    /// this is strictly a rearrangement of existing patches.
    pub(crate) fn repair_appliedness(
        &mut self,
        applied: Vec<PatchName>,
        unapplied: Vec<PatchName>,
        hidden: Vec<PatchName>,
    ) {
        let mut old: IndexSet<PatchName> = self
            .applied
            .drain(..)
            .chain(self.unapplied.drain(..).chain(self.hidden.drain(..)))
            .collect();
        self.applied = applied;
        self.unapplied = unapplied;
        self.hidden = hidden;

        for pn in self.all_patches() {
            old.shift_take(pn)
                .expect("new patchname must be an old patchname");
        }
        assert!(
            old.is_empty(),
            "all old patchnames must be in the new applied/unapplied/hidden: {old:?}"
        );
    }

    /// Perform push and pop operations to achieve a new stack ordering.
    ///
    /// The current ordering is maintained for any patch list that is not provided.
    pub(crate) fn reorder_patches(
        &mut self,
        applied: Option<&[PatchName]>,
        unapplied: Option<&[PatchName]>,
        hidden: Option<&[PatchName]>,
    ) -> Result<()> {
        if let Some(applied) = applied {
            let num_common = self
                .applied
                .iter()
                .zip(applied)
                .take_while(|(old, new)| old == new)
                .count();

            let to_pop: IndexSet<PatchName> = self.applied[num_common..].iter().cloned().collect();
            self.pop_patches(|pn| to_pop.contains(pn))?;

            let to_push = &applied[num_common..];
            self.push_patches(to_push, false)?;

            assert_eq!(self.applied, applied);

            if to_push.is_empty() {
                if let Some(last) = applied.last() {
                    self.ui.print_pushed(last, PushStatus::Unmodified, true)?;
                }
            }
        }

        if let Some(unapplied) = unapplied {
            self.unapplied = unapplied.to_vec();
        }

        if let Some(hidden) = hidden {
            self.hidden = hidden.to_vec();
        }

        Ok(())
    }

    // Finalize patches to be regular Git commits.
    //
    // Committed patches are no longer managed by StGit, but their commit objects remain
    // part of the regular git commit history. Committed patches are/become the base for
    // the remaining StGit stack.
    //
    // If the chosen `to_commit` patches are not currently the bottommost patches in the
    // stack, pops and pushes will be performed to move them to the bottom of the stack.
    // This may result in merge conflicts.
    pub(crate) fn commit_patches(&mut self, to_commit: &[PatchName]) -> Result<()> {
        let num_common = self
            .applied()
            .iter()
            .zip(to_commit.iter())
            .take_while(|(pn0, pn1)| pn0 == pn1)
            .count();

        let to_push: Vec<PatchName> = if num_common < to_commit.len() {
            let to_push: Vec<PatchName> = self.applied()[num_common..]
                .iter()
                .filter(|pn| !to_commit.contains(*pn))
                .cloned()
                .collect();

            self.pop_patches(|pn| to_push.contains(pn))?;
            self.push_patches(&to_commit[num_common..], false)?;
            to_push
        } else {
            vec![]
        };

        self.ui.print_committed(to_commit)?;

        self.updated_base = Some(self.get_patch_commit(to_commit.last().unwrap()).clone());
        for patchname in to_commit {
            self.updated_patches.insert(patchname.clone(), None);
        }
        self.applied = self.applied.split_off(to_commit.len());
        self.push_patches(&to_push, false)
    }

    /// Transform regular git commits from the base of the stack into StGit patches.
    ///
    /// The `(patchname, commit_id)` pairs must be in application order. I.e. the
    /// furthest ancestor of the current base first and the current base last.
    pub(crate) fn uncommit_patches<'a>(
        &mut self,
        patches: impl IntoIterator<Item = (&'a PatchName, gix::ObjectId)>,
    ) -> Result<()> {
        let mut new_applied: Vec<_> = Vec::with_capacity(self.applied.len());
        for (patchname, commit_id) in patches {
            let commit = self.stack.repo.find_commit(commit_id)?;
            self.updated_patches.insert(
                patchname.clone(),
                Some(PatchState {
                    commit: Rc::new(commit),
                }),
            );
            new_applied.push(patchname.clone());
        }
        self.ui.print_uncommitted(new_applied.as_ref())?;
        new_applied.append(&mut self.applied);
        self.applied = new_applied;
        Ok(())
    }

    /// Hide patches that are currently applied or unapplied.
    ///
    /// Hidden patches are not shown by default by `stg series` and are excluded from
    /// several other operations.
    pub(crate) fn hide_patches(&mut self, to_hide: &[PatchName]) -> Result<()> {
        let applied: Vec<PatchName> = self
            .applied
            .iter()
            .filter(|pn| !to_hide.contains(pn))
            .cloned()
            .collect();

        let unapplied: Vec<PatchName> = self
            .unapplied
            .iter()
            .filter(|pn| !to_hide.contains(pn))
            .cloned()
            .collect();

        let hidden: Vec<PatchName> = to_hide.iter().chain(self.hidden.iter()).cloned().collect();

        self.reorder_patches(Some(&applied), Some(&unapplied), Some(&hidden))?;

        self.ui.print_hidden(to_hide)
    }

    /// Move hidden patches to the unapplied list.
    pub(crate) fn unhide_patches(&mut self, to_unhide: &[PatchName]) -> Result<()> {
        let unapplied: Vec<PatchName> = self
            .unapplied
            .iter()
            .chain(to_unhide.iter())
            .cloned()
            .collect();

        let hidden: Vec<PatchName> = self
            .hidden
            .iter()
            .filter(|pn| !to_unhide.contains(pn))
            .cloned()
            .collect();

        self.reorder_patches(None, Some(&unapplied), Some(&hidden))?;

        self.ui.print_unhidden(to_unhide)
    }

    /// Rename a patch.
    ///
    /// An error will be returned if either the old patchname does not exist or if the
    /// new patchname conflicts with an existing patch.
    pub(crate) fn rename_patch(
        &mut self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
    ) -> Result<()> {
        if new_patchname == old_patchname {
            return Ok(());
        } else if let Some(colliding_patchname) = self.stack.collides(new_patchname) {
            if colliding_patchname != old_patchname
                && self.updated_patches.contains_key(colliding_patchname)
            {
                return Err(anyhow!(
                    "new patch name `{new_patchname}` collides with `{colliding_patchname}`"
                ));
            }
        }
        if !self.stack.has_patch(old_patchname) {
            return Err(anyhow!("patch `{old_patchname}` does not exist"));
        }

        if let Some(pos) = self.applied.iter().position(|pn| pn == old_patchname) {
            self.applied[pos] = new_patchname.clone();
        } else if let Some(pos) = self.unapplied.iter().position(|pn| pn == old_patchname) {
            self.unapplied[pos] = new_patchname.clone();
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == old_patchname) {
            self.hidden[pos] = new_patchname.clone();
        } else {
            panic!("old `{old_patchname}` not found in applied, unapplied, or hidden");
        }

        if let Some(Some(patch_state)) = self.updated_patches.remove(old_patchname) {
            // The renamed patch may have been previously updated in this transaction.
            // This can happen, for example, for `stg refresh`.
            self.updated_patches.insert(old_patchname.clone(), None);
            self.updated_patches
                .insert(new_patchname.clone(), Some(patch_state));
        } else {
            let patch_state = self.stack.get_patch(old_patchname).clone();
            self.updated_patches.insert(old_patchname.clone(), None);
            self.updated_patches
                .insert(new_patchname.clone(), Some(patch_state));
        }

        self.ui.print_rename(old_patchname, new_patchname)
    }

    /// Delete one or more patches from the stack.
    ///
    /// Deleted patches' commits become disconnected from the regular git history and
    /// are thus subject to eventual garbage collection.
    pub(crate) fn delete_patches<F>(&mut self, should_delete: F) -> Result<Vec<PatchName>>
    where
        F: Fn(&PatchName) -> bool,
    {
        let all_popped = if let Some(first_pop_pos) = self.applied.iter().position(&should_delete) {
            self.applied.split_off(first_pop_pos)
        } else {
            vec![]
        };

        let incidental: Vec<PatchName> = all_popped
            .iter()
            .filter(|pn| !should_delete(pn))
            .cloned()
            .collect();

        let unapplied_size = incidental.len() + self.unapplied.len();
        let unapplied = std::mem::replace(&mut self.unapplied, Vec::with_capacity(unapplied_size));
        self.unapplied.append(&mut incidental.clone());

        self.ui.print_popped(&all_popped)?;

        // Gather contiguous groups of deleted patchnames for printing.
        let mut deleted_group: Vec<PatchName> = Vec::with_capacity(all_popped.len());

        for patchname in all_popped {
            if should_delete(&patchname) {
                deleted_group.push(patchname.clone());
                self.updated_patches.insert(patchname, None);
            } else if !deleted_group.is_empty() {
                self.ui.print_deleted(&deleted_group)?;
                deleted_group.clear();
            }
        }

        for patchname in unapplied {
            if should_delete(&patchname) {
                deleted_group.push(patchname.clone());
                self.updated_patches.insert(patchname, None);
            } else {
                self.ui.print_deleted(&deleted_group)?;
                deleted_group.clear();
                self.unapplied.push(patchname);
            }
        }

        let mut i = 0;
        while i < self.hidden.len() {
            if should_delete(&self.hidden[i]) {
                let patchname = self.hidden.remove(i);
                deleted_group.push(patchname.clone());
                self.updated_patches.insert(patchname, None);
            } else {
                i += 1;
                self.ui.print_deleted(&deleted_group)?;
                deleted_group.clear();
            }
        }

        if !deleted_group.is_empty() {
            self.ui.print_deleted(&deleted_group)?;
        }

        Ok(incidental)
    }

    /// Pop applied patches, making them unapplied.
    ///
    /// The `should_pop` closure should return true for each patch name to be popped and
    /// false for patches that are to remain applied.
    pub(crate) fn pop_patches<F>(&mut self, should_pop: F) -> Result<Vec<PatchName>>
    where
        F: Fn(&PatchName) -> bool,
    {
        let all_popped = if let Some(first_pop_pos) = self.applied.iter().position(&should_pop) {
            self.applied.split_off(first_pop_pos)
        } else {
            vec![]
        };

        let incidental: Vec<PatchName> = all_popped
            .iter()
            .filter(|pn| !should_pop(pn))
            .cloned()
            .collect();

        let mut requested: Vec<PatchName> = all_popped
            .iter()
            .filter(|pn| should_pop(pn))
            .cloned()
            .collect();

        let unapplied_size = incidental.len() + requested.len() + self.unapplied.len();
        let mut unapplied =
            std::mem::replace(&mut self.unapplied, Vec::with_capacity(unapplied_size));

        self.unapplied.append(&mut incidental.clone());
        self.unapplied.append(&mut requested);
        self.unapplied.append(&mut unapplied);

        self.ui.print_popped(&all_popped)?;

        Ok(incidental)
    }

    /// Push unapplied patches to become applied.
    ///
    /// Pushing a patch may result in a merge conflict. When this occurs, a
    /// `Error::TransactionHalt` will be returned which will cause the current
    /// transaction to halt. This condition is not an error, per-se, so the stack state
    /// is *not* rolled back. Instead, the conflicts will be left in the working tree
    /// and index for the user to resolve.
    ///
    /// The `check_merged` option, when true, performs an extra check to determine
    /// whether the patches' changes have already been merged into the stack's base
    /// tree. Patches that are determined to have already been merged will still be
    /// pushed successfully, but their diff will be empty.
    pub(crate) fn push_patches<P>(&mut self, patchnames: &[P], check_merged: bool) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        self.push_patches_impl(patchnames, check_merged, &[])
    }

    /// Push unapplied patches, running exec commands after each successful push.
    ///
    /// After each patch is successfully pushed, all provided exec commands are run
    /// in sequence. If any exec command fails, the entire transaction is rolled back
    /// (no patches remain applied).
    ///
    /// This supports the `stg rebase --exec` functionality. Note that this differs from
    /// `git rebase --exec` which leaves you at the failing point; stgit's rollback
    /// behavior is safer and consistent with how stgit transactions work.
    pub(crate) fn push_patches_with_exec<P>(
        &mut self,
        patchnames: &[P],
        check_merged: bool,
        exec_cmds: &[&str],
    ) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        self.push_patches_impl(patchnames, check_merged, exec_cmds)
    }

    /// Core implementation for pushing patches with optional exec commands.
    fn push_patches_impl<P>(
        &mut self,
        patchnames: &[P],
        check_merged: bool,
        exec_cmds: &[&str],
    ) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        let stupid = self.stack.repo.stupid();
        stupid.with_temp_index(|stupid_temp| {
            let mut temp_index_tree_id: Option<gix::ObjectId> = None;

            let merged = if check_merged {
                Some(self.check_merged(patchnames, stupid_temp, &mut temp_index_tree_id)?)
            } else {
                None
            };

            for (i, patchname) in patchnames.iter().enumerate() {
                let patchname = patchname.as_ref();
                // When exec commands are provided, we can't optimize the final checkout
                // because the commands may modify the working tree. Only treat the last
                // patch as "last" (enabling checkout optimization) when there are no
                // exec commands.
                let should_optimize_final_checkout =
                    i + 1 == patchnames.len() && exec_cmds.is_empty();
                let already_merged = merged
                    .as_ref()
                    .is_some_and(|merged| merged.contains(&patchname));
                self.push_patch(
                    patchname,
                    already_merged,
                    should_optimize_final_checkout,
                    stupid_temp,
                    &mut temp_index_tree_id,
                )?;

                for cmd in exec_cmds {
                    self.ui.print_exec(cmd)?;
                    stupid.exec_cmd(cmd)?;
                }
            }

            Ok(())
        })
    }

    fn push_patch(
        &mut self,
        patchname: &PatchName,
        already_merged: bool,
        is_last: bool,
        stupid_temp: &StupidContext,
        temp_index_tree_id: &mut Option<gix::ObjectId>,
    ) -> Result<()> {
        let repo = self.stack.repo;
        let config = repo.config_snapshot();
        let stupid = repo.stupid();
        let default_committer = repo.get_committer()?;
        let patch_commit = self.get_patch_commit(patchname).clone();
        let old_parent = patch_commit.get_parent_commit()?;
        let new_parent = self.top().clone();
        let patch_commit_ref = patch_commit.decode()?;
        let old_parent_ref = old_parent.decode()?;
        let new_parent_ref = new_parent.decode()?;

        let mut push_status = PushStatus::Unmodified;

        let new_tree_id = if already_merged {
            push_status = PushStatus::AlreadyMerged;
            new_parent_ref.tree()
        } else if old_parent_ref.tree() == new_parent_ref.tree() {
            patch_commit_ref.tree()
        } else if old_parent_ref.tree() == patch_commit_ref.tree() {
            new_parent_ref.tree()
        } else if new_parent_ref.tree() == patch_commit_ref.tree() {
            patch_commit_ref.tree()
        } else {
            let (ours, theirs) = if temp_index_tree_id == &Some(patch_commit_ref.tree()) {
                (patch_commit_ref.tree(), new_parent_ref.tree())
            } else {
                (new_parent_ref.tree(), patch_commit_ref.tree())
            };
            let base = old_parent_ref.tree();

            if temp_index_tree_id != &Some(ours) {
                stupid_temp.read_tree(ours)?;
                *temp_index_tree_id = Some(ours);
            }

            let maybe_tree_id = if stupid_temp.apply_treediff_to_index(base, theirs, true)? {
                stupid_temp.write_tree().ok()
            } else {
                None
            };

            if let Some(tree_id) = maybe_tree_id {
                tree_id
            } else if !self.options.use_index_and_worktree {
                return Err(Error::TransactionHalt {
                    msg: format!("{patchname} does not apply cleanly"),
                    conflicts: false,
                }
                .into());
            } else if !self
                .options
                .allow_push_conflicts
                .unwrap_or_else(|| config.boolean("stgit.push.allow-conflicts").unwrap_or(true))
            {
                return Err(Error::TransactionHalt {
                    msg: format!(
                        "pushing patch `{patchname}` would result in conflicts \
                         and push conflicts are disallowed"
                    ),
                    conflicts: false,
                }
                .into());
            } else {
                if stupid
                    .read_tree_checkout(self.current_tree_id, ours)
                    .is_err()
                {
                    return Err(Error::TransactionHalt {
                        msg: "index/worktree dirty".to_string(),
                        conflicts: false,
                    }
                    .into());
                }
                self.current_tree_id = ours;

                let use_mergetool = config.boolean("stgit.autoimerge").unwrap_or(false);
                match stupid.merge_recursive_or_mergetool(base, ours, theirs, use_mergetool) {
                    Ok(true) => {
                        // Success, no conflicts
                        let tree_id = stupid.write_tree().map_err(|_| Error::TransactionHalt {
                            msg: "conflicting merge".to_string(),
                            conflicts: false,
                        })?;
                        self.current_tree_id = tree_id;
                        push_status = PushStatus::Modified;
                        tree_id
                    }
                    Ok(false) => {
                        push_status = PushStatus::Conflict;
                        ours
                    }
                    Err(e) => {
                        return Err(Error::TransactionHalt {
                            msg: format!("{e:#}"),
                            conflicts: false,
                        }
                        .into())
                    }
                }
            }
        };

        if new_tree_id != patch_commit_ref.tree() || new_parent.id != old_parent.id {
            let author = patch_commit.author_strict()?;
            let committer = if self.options.committer_date_is_author_date {
                let mut committer = default_committer.to_owned()?;
                committer.time = author.time;
                committer
            } else {
                default_committer.to_owned()?
            };
            let commit_id = repo.commit_ex(
                &author,
                &committer,
                &patch_commit.message_ex(),
                new_tree_id,
                [new_parent.id],
            )?;
            let commit = Rc::new(repo.find_commit(commit_id)?);
            stupid.notes_copy(patch_commit.id, commit_id).ok();
            if push_status == PushStatus::Conflict {
                // In the case of a conflict, update() will be called after the
                // execute() performs the checkout. Setting the transaction head
                // here ensures that the real stack top will be checked-out.
                self.updated_head = Some(commit.clone());
            } else if push_status != PushStatus::AlreadyMerged
                && new_tree_id == new_parent_ref.tree()
            {
                push_status = PushStatus::Empty;
            }

            self.updated_patches
                .insert(patchname.clone(), Some(PatchState { commit }));
        }

        if push_status == PushStatus::Conflict {
            // The final checkout at execute-time must allow these push conflicts.
            self.options.conflict_mode = ConflictMode::Allow;
        }

        if let Some(pos) = self.unapplied.iter().position(|pn| pn == patchname) {
            self.unapplied.remove(pos);
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == patchname) {
            self.hidden.remove(pos);
        }
        self.applied.push(patchname.clone());

        self.ui.print_pushed(patchname, push_status, is_last)?;

        if push_status == PushStatus::Conflict {
            Err(Error::TransactionHalt {
                msg: "merge conflicts; \
                      resolve conflicts manually then refresh or \
                      undo the operation with `stg undo --hard`."
                    .to_string(),
                conflicts: true,
            }
            .into())
        } else {
            Ok(())
        }
    }

    /// Find patches that have already been merged into the stack base's tree.
    ///
    /// The diffs for each provided patchname are applied to the stack's base tree (in
    /// the context of the provided temp index) to determine whether the patches'
    /// changes are already manifest in the base tree.
    fn check_merged<'a, P>(
        &self,
        patchnames: &'a [P],
        stupid_temp: &StupidContext,
        temp_index_tree_id: &mut Option<gix::ObjectId>,
    ) -> Result<Vec<&'a PatchName>>
    where
        P: AsRef<PatchName>,
    {
        let head_tree_id = self.stack.get_branch_head().tree_id()?.detach();
        let mut merged: Vec<&PatchName> = vec![];

        if temp_index_tree_id != &Some(head_tree_id) {
            stupid_temp.read_tree(head_tree_id)?;
            *temp_index_tree_id = Some(head_tree_id);
        }

        for patchname in patchnames.iter().rev() {
            let patchname = patchname.as_ref();
            let patch_commit = self.get_patch_commit(patchname);

            if patch_commit.is_no_change()? {
                continue; // No change
            }

            let parent_commit = patch_commit.get_parent_commit()?;

            if stupid_temp.apply_treediff_to_index(
                patch_commit.tree_id()?.detach(),
                parent_commit.tree_id()?.detach(),
                false,
            )? {
                merged.push(patchname);
                *temp_index_tree_id = None;
            }
        }

        self.ui.print_merged(&merged)?;

        Ok(merged)
    }
}

impl<'repo> StackAccess<'repo> for StackTransaction<'repo> {
    fn get_branch_name(&self) -> &str {
        self.stack.get_branch_name()
    }

    fn get_branch_refname(&self) -> &gix::refs::FullNameRef {
        self.stack.get_branch_refname()
    }

    fn get_stack_refname(&self) -> &str {
        self.stack.get_stack_refname()
    }

    fn get_branch_head(&self) -> &Rc<gix::Commit<'repo>> {
        self.stack.get_branch_head()
    }

    fn base(&self) -> &Rc<gix::Commit<'repo>> {
        if let Some(commit) = self.updated_base.as_ref() {
            commit
        } else {
            self.stack.base()
        }
    }
}

impl<'repo> StackStateAccess<'repo> for StackTransaction<'repo> {
    fn applied(&self) -> &[PatchName] {
        &self.applied
    }

    fn unapplied(&self) -> &[PatchName] {
        &self.unapplied
    }

    fn hidden(&self) -> &[PatchName] {
        &self.hidden
    }

    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo> {
        if let Some(maybe_patch) = self.updated_patches.get(patchname) {
            maybe_patch
                .as_ref()
                .expect("should not attempt to access deleted patch")
        } else {
            self.stack.get_patch(patchname)
        }
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        if let Some(maybe_patch) = self.updated_patches.get(patchname) {
            maybe_patch.is_some()
        } else {
            self.stack.has_patch(patchname)
        }
    }

    fn top(&self) -> &Rc<gix::Commit<'repo>> {
        if let Some(patchname) = self.applied.last() {
            self.get_patch_commit(patchname)
        } else {
            self.base()
        }
    }

    fn head(&self) -> &Rc<gix::Commit<'repo>> {
        if let Some(commit) = self.updated_head.as_ref() {
            commit
        } else {
            self.top()
        }
    }
}
