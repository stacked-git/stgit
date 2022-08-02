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

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::io::Write;

use anyhow::{anyhow, Result};
use git2::{Commit, Oid};
use indexmap::IndexSet;
use termcolor::WriteColor;

use crate::{
    commit::{CommitExtended, RepositoryCommitExtended},
    index::TemporaryIndex,
    patchname::PatchName,
    repo::RepositoryExtended,
    signature::SignatureExtended,
    stack::{PatchState, Stack, StackStateAccess},
    stupid::Stupid,
};

use super::{error::Error, state::StackState};

/// Options for fine-tuning stack transaction behaviors.
struct TransactionOptions {
    pub(self) conflict_mode: ConflictMode,
    pub(self) discard_changes: bool,
    pub(self) use_index_and_worktree: bool,
    pub(self) set_head: bool,
    pub(self) allow_bad_head: bool,
}

impl Default for TransactionOptions {
    fn default() -> Self {
        Self {
            conflict_mode: ConflictMode::Disallow,
            discard_changes: false,
            use_index_and_worktree: false,
            set_head: true,
            allow_bad_head: false,
        }
    }
}

/// Builder used to setup a stack transaction.
pub(crate) struct TransactionBuilder<'repo> {
    stack: Stack<'repo>,
    output: Option<termcolor::StandardStream>,
    options: TransactionOptions,
}

/// Stack transaction state.
pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    output: RefCell<termcolor::StandardStream>,
    options: TransactionOptions,

    patch_updates: BTreeMap<PatchName, Option<PatchState<'repo>>>,
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    updated_head: Option<Commit<'repo>>,
    updated_base: Option<Commit<'repo>>,
    current_tree_id: Oid,
    error: Option<anyhow::Error>,
    printed_top: bool,
}

/// Policies for whether a transaction may execute when conflicts emerge from the
/// transactions operations.
pub(crate) enum ConflictMode {
    /// Transaction execution will fail if there are conflicts recorded in the index.
    ///
    /// This is the default.
    Disallow,

    /// Transaction execution will succeed even if there are outstanding conflicts.
    Allow,

    /// Transaction execution will succeed with conflicts, but only if the topmost patch
    /// is unchanged by the transaction.
    AllowIfSameTop,
}

impl Default for ConflictMode {
    fn default() -> Self {
        Self::Disallow
    }
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

/// Builder for setting up transaction state and options.
impl<'repo> TransactionBuilder<'repo> {
    /// Create a new `TransactionBuilder` instance with default options.
    #[must_use]
    pub(crate) fn new(stack: Stack<'repo>) -> Self {
        Self {
            stack,
            output: None,
            options: TransactionOptions::default(),
        }
    }

    /// Allow the transaction to execute if the stack's actual head (branch head commit)
    /// does not match the head recorded in stack state. By default, the transaction
    /// will not execute if the stack head does not match the branch head.
    #[must_use]
    pub(crate) fn allow_bad_head(mut self, allow: bool) -> Self {
        self.options.allow_bad_head = allow;
        self
    }

    /// Allow the transaction to execute even if the transaction operations result in
    /// outstanding merge conflicts. By default, the transaction will not execute if
    /// there are outstanding conflicts.
    #[must_use]
    pub(crate) fn allow_conflicts(mut self, allow: bool) -> Self {
        self.options.conflict_mode = if allow {
            ConflictMode::Allow
        } else {
            ConflictMode::Disallow
        };
        self
    }

    /// Allow the transaction to execute even if the transaction operations result in
    /// outstanding merge conflicts, but only if the transaction does not change the
    /// topmost patch. By default, the transaction will not execute if there are
    /// outstanding conflicts.
    #[must_use]
    pub(crate) fn allow_conflicts_if_same_top(mut self, allow: bool) -> Self {
        self.options.conflict_mode = if allow {
            ConflictMode::AllowIfSameTop
        } else {
            ConflictMode::Disallow
        };
        self
    }

    /// Discard any modifications to files in the working tree when the transaction
    /// executes. By default, the transaction will not execute if there are any
    /// modified files in the working tree.
    #[must_use]
    pub(crate) fn discard_changes(mut self, discard: bool) -> Self {
        self.options.discard_changes = discard;
        self
    }

    /// Allow the stack transaction operations modify index and/or work tree state. This
    /// primarily affects operations that cause patches to be pushed. When use of the
    /// index and worktree is disallowed (the default), all pushes must apply cleanly
    /// because any conflicts cannot be written the index and worktree.
    #[must_use]
    pub(crate) fn use_index_and_worktree(mut self, allow: bool) -> Self {
        self.options.use_index_and_worktree = allow;
        self
    }

    /// Set the output stream for the transaction. This method must be called.
    #[must_use]
    pub(crate) fn with_output_stream(mut self, output: termcolor::StandardStream) -> Self {
        self.output = Some(output);
        self
    }

    /// Determines whether the branch and stack metadata refs should be updated when the
    /// transaction executes succesfully. This is the default. Disabling this is only useful
    /// in very special circumstances (e.g. for `stg uncommit`).
    #[must_use]
    pub(crate) fn set_head(mut self, yes: bool) -> Self {
        self.options.set_head = yes;
        self
    }

    /// Perform stack transaction operations.
    ///
    /// The closure provided to this method may call various methods on the provided
    /// [`StackTransaction`] instance. The closure returns `Err`, any changes to the stack,
    /// index, or work tree will be rolled-back during the subsequent execution phase.
    ///
    /// N.B. [`Error::TransactionHalt`] errors do not trigger rollback.
    ///
    /// This method must be called. It returns an [`ExecuteContext`] which must then be used
    /// to execute the transaction by calling [`ExecuteContext::execute()`].
    #[must_use]
    pub(crate) fn transact<F>(self, f: F) -> ExecuteContext<'repo>
    where
        F: FnOnce(&mut StackTransaction) -> Result<()>,
    {
        let Self {
            stack,
            output,
            options,
        } = self;

        let output = output.expect("with_output_stream() must be called");
        let output = RefCell::new(output);

        let current_tree_id = stack.branch_head.tree_id();
        let applied = stack.applied().to_vec();
        let unapplied = stack.unapplied().to_vec();
        let hidden = stack.hidden().to_vec();

        let mut transaction = StackTransaction {
            stack,
            output,
            options,
            patch_updates: BTreeMap::new(),
            applied,
            unapplied,
            hidden,
            error: None,
            updated_head: None,
            updated_base: None,
            current_tree_id,
            printed_top: false,
        };

        transaction.error = f(&mut transaction).err();

        ExecuteContext(transaction)
    }
}

/// Context for executing a [`StackTransaction`].
///
/// Wraps [`StackTransaction`] to ensure [`ExecuteContext::execute()`] is called after
/// [`TransactionBuilder::transact()`].
pub(crate) struct ExecuteContext<'repo>(StackTransaction<'repo>);

impl<'repo> ExecuteContext<'repo> {
    /// Execute the transaction.
    ///
    /// If any of the transaction operations (i.e. from `transact()`) fail, the
    /// stack, index, and worktree state will be rolled back.
    ///
    /// A new `Stack` instance is returned.
    pub(crate) fn execute(self, reflog_msg: &str) -> Result<Stack<'repo>> {
        let mut transaction = self.0;

        // Only proceed for halt errors
        let has_conflicts = if let Some(err) = &transaction.error {
            match err.downcast_ref::<Error>() {
                Some(Error::TransactionHalt { conflicts, .. }) => *conflicts,
                _ => return Err(transaction.error.unwrap()),
            }
        } else {
            false
        };

        // Check consistency
        for (patchname, oid) in transaction.patch_updates.iter() {
            if oid.is_none() {
                assert!(transaction.stack.has_patch(patchname));
            } else {
                assert!(transaction.all_patches().any(|pn| pn == patchname));
            }
        }

        // Log external modifications
        transaction.stack = if transaction.stack.is_head_top() {
            transaction.stack
        } else {
            // TODO: why update the stack state ref unconditional of transaction.error?
            transaction.stack.log_external_mods()?
        };

        let repo = transaction.stack.repo;

        if transaction.options.set_head {
            let trans_head = transaction.head().clone();

            if transaction.options.use_index_and_worktree {
                let stack_head = transaction.stack.branch_head.clone();
                let result = transaction.checkout(&trans_head);
                if let Err(err) = result {
                    transaction.options.allow_bad_head = true;
                    transaction.checkout(&stack_head)?;
                    return Err(anyhow!(
                        "{err}\n\
                         Command aborted (all changes rolled back)"
                    ));
                }
            }

            let updated_ref = transaction
                .stack
                .branch
                .get_mut()
                .set_target(trans_head.id(), reflog_msg)?;
            transaction
                .stack
                .update_head(git2::Branch::wrap(updated_ref), trans_head);
        }

        let conflict_msg;
        let reflog_msg = if has_conflicts {
            conflict_msg = format!("{reflog_msg} (CONFLICT)");
            &conflict_msg
        } else {
            reflog_msg
        };

        // Update patch refs and stack state refs
        let mut git_trans = repo.transaction()?;
        let reflog_signature = None; // Use default signature

        git_trans.lock_ref(&transaction.stack.refname)?;

        for (patchname, maybe_patch) in &transaction.patch_updates {
            let patch_refname = transaction.stack.patch_refname(patchname);
            let state = transaction.stack.state_mut();
            git_trans.lock_ref(&patch_refname)?;

            if let Some(patch) = maybe_patch {
                git_trans.set_target(
                    &patch_refname,
                    patch.commit.id(),
                    reflog_signature,
                    reflog_msg,
                )?;
                state.patches.insert(patchname.clone(), patch.clone());
            } else {
                git_trans.remove(&patch_refname)?;
                state.patches.remove(patchname);
            }
        }

        // For printing applied patch name...
        let _old_applied_pn = transaction.stack.applied().last().map(|pn| pn.to_string());
        let _new_applied_pn = transaction.applied.last().map(|pn| pn.to_string());
        let new_top_patchname = transaction.applied.last().cloned();

        if !transaction.printed_top {
            if let Some(top_patchname) = new_top_patchname.as_ref() {
                transaction.print_pushed(top_patchname, PushStatus::Unmodified, true)?;
            }
        }

        let stack_ref = repo.find_reference(&transaction.stack.refname)?;
        let prev_state_commit = stack_ref.peel_to_commit()?;
        let head = transaction.head().clone();

        let state = transaction.stack.state_mut();
        state.prev = Some(prev_state_commit);
        state.head = head;
        state.applied = transaction.applied;
        state.unapplied = transaction.unapplied;
        state.hidden = transaction.hidden;

        let state_commit_id = state.commit(repo, None, reflog_msg)?;
        git_trans.set_target(
            &transaction.stack.refname,
            state_commit_id,
            reflog_signature,
            reflog_msg,
        )?;

        git_trans.commit()?;

        if let Some(err) = transaction.error {
            Err(err)
        } else {
            Ok(transaction.stack)
        }
    }
}

impl<'repo> StackTransaction<'repo> {
    fn checkout(&mut self, commit: &Commit<'_>) -> Result<()> {
        let repo = self.stack.repo;
        if !self.options.allow_bad_head {
            self.stack.check_head_top_mismatch()?;
        }

        if self.current_tree_id == commit.tree_id() && !self.options.discard_changes {
            return match self.options.conflict_mode {
                ConflictMode::Allow => Ok(()),
                ConflictMode::AllowIfSameTop => {
                    let top = self.applied.last();
                    if top.is_some() && top == self.stack.applied().last() {
                        Ok(())
                    } else {
                        repo.check_conflicts()
                    }
                }
                ConflictMode::Disallow => repo.check_conflicts(),
            };
        }

        if self.options.discard_changes {
            let mut checkout_builder = git2::build::CheckoutBuilder::new();
            checkout_builder.force();
            repo.checkout_tree_ex(commit.as_object(), Some(&mut checkout_builder))?;
        } else {
            let stupid = self.repo().stupid();
            stupid.update_index_refresh()?;
            stupid
                .read_tree_checkout(self.current_tree_id, commit.tree_id())
                .map_err(|e| Error::CheckoutConflicts(format!("{e:#}")))?;
        }

        self.current_tree_id = commit.tree_id();
        Ok(())
    }

    /// Get an immutable reference to the original stack.
    pub(crate) fn stack(&self) -> &Stack<'repo> {
        &self.stack
    }

    /// Get a reference to the repo.
    pub(crate) fn repo(&self) -> &'repo git2::Repository {
        self.stack.repo
    }

    /// Reset stack to a previous stack state.
    pub(crate) fn reset_to_state(&mut self, state: StackState<'repo>) -> Result<()> {
        for pn in self.all_patches().cloned().collect::<Vec<_>>() {
            self.patch_updates.insert(pn, None);
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
            patches[pn].commit.parent(0)?
        } else {
            head.clone()
        });
        self.updated_head = Some(head);
        for (pn, patch_state) in patches {
            self.patch_updates.insert(pn, Some(patch_state));
        }
        self.applied = applied;
        self.unapplied = unapplied;
        self.hidden = hidden;
        Ok(())
    }

    /// Reset stack to previous stack state, but only for the specified patch names.
    pub(crate) fn reset_to_state_partially<P>(
        &mut self,
        state: StackState<'repo>,
        patchnames: &[P],
    ) -> Result<()>
    where
        P: AsRef<PatchName>,
    {
        let only_patches: IndexSet<_> = patchnames.iter().map(|pn| pn.as_ref()).collect();
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
                if self.has_patch(pn) && self.get_patch_commit(pn).id() == patch_state.commit.id() {
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
            self.patch_updates
                .insert(pn.clone(), Some(state.patches[pn].clone()));
            self.print_updated(pn)?;
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
    pub(crate) fn update_patch(&mut self, patchname: &PatchName, commit_id: Oid) -> Result<()> {
        let commit = self.stack.repo.find_commit(commit_id)?;
        let old_commit = self.get_patch_commit(patchname);
        // Failure to copy is okay. The old commit may not have a note to copy.
        self.stack
            .repo
            .stupid()
            .notes_copy(old_commit.id(), commit_id)
            .ok();
        self.patch_updates
            .insert(patchname.clone(), Some(PatchState { commit }));
        self.print_updated(patchname)?;
        Ok(())
    }

    /// Add new patch to the top of the stack.
    ///
    /// The commit for the new patch must be parented by the former top commit of the
    /// stack.
    pub(crate) fn new_applied(&mut self, patchname: &PatchName, oid: Oid) -> Result<()> {
        let commit = self.stack.repo.find_commit(oid)?;
        assert_eq!(commit.parent_id(0).unwrap(), self.top().id());
        self.applied.push(patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchState { commit }));
        self.print_pushed(patchname, PushStatus::New, true)?;
        Ok(())
    }

    /// Add new unapplied patch to the stack.
    ///
    /// The new patch may be pushed to any position in the unapplied list.
    pub(crate) fn new_unapplied(
        &mut self,
        patchname: &PatchName,
        commit_id: Oid,
        insert_pos: usize,
    ) -> Result<()> {
        let commit = self.stack.repo.find_commit(commit_id)?;
        self.unapplied.insert(insert_pos, patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchState { commit }));
        self.print_popped(&[patchname.clone()])?;
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
        let config = repo.config()?;
        let parent = patch_commit.parent(0)?;
        let is_empty = parent.tree_id() == patch_commit.tree_id();

        let push_status = if patch_commit.parent_id(0)? != self.top().id() {
            let default_committer = git2::Signature::default_committer(Some(&config))?;
            let message = patch_commit.message_ex();
            let parent_ids = [self.top().id()];
            let new_commit_id = repo.commit_ex(
                &patch_commit.author_strict()?,
                &default_committer,
                &message,
                patch_commit.tree_id(),
                parent_ids,
            )?;

            let commit = repo.find_commit(new_commit_id)?;
            repo.stupid()
                .notes_copy(patch_commit.id(), new_commit_id)
                .ok();
            self.patch_updates
                .insert(patchname.clone(), Some(PatchState { commit }));

            PushStatus::Modified
        } else {
            PushStatus::Unmodified
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

        self.print_pushed(patchname, push_status, is_last)
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
    ) -> Result<()> {
        let mut old: IndexSet<PatchName> = IndexSet::from_iter(
            self.applied
                .drain(..)
                .chain(self.unapplied.drain(..).chain(self.hidden.drain(..))),
        );
        self.applied = applied;
        self.unapplied = unapplied;
        self.hidden = hidden;

        for pn in self.all_patches() {
            old.take(pn)
                .expect("new patchname must be an old patchname");
        }
        assert!(
            old.is_empty(),
            "all old patchnames must be in the new applied/unapplied/hidden: {old:?}"
        );
        Ok(())
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
                    self.print_pushed(last, PushStatus::Unmodified, true)?;
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

        self.print_committed(to_commit)?;

        self.updated_base = Some(self.get_patch_commit(to_commit.last().unwrap()).clone());
        for patchname in to_commit {
            self.patch_updates.insert(patchname.clone(), None);
        }
        self.applied = self.applied.split_off(to_commit.len());
        self.push_patches(&to_push, false)
    }

    /// Transform regular git commits from the base of the stack into StGit patches.
    ///
    /// The (patchname, commit_id) pairs must be in application order. I.e. the furthest
    /// ancestor of the current base first and the current base last.
    pub(crate) fn uncommit_patches<'a>(
        &mut self,
        patches: impl IntoIterator<Item = (&'a PatchName, git2::Oid)>,
    ) -> Result<()> {
        let mut new_applied: Vec<_> = Vec::with_capacity(self.applied.len());
        for (patchname, commit_id) in patches {
            let commit = self.stack.repo.find_commit(commit_id)?;
            self.patch_updates
                .insert(patchname.clone(), Some(PatchState { commit }));
            new_applied.push(patchname.clone());
        }
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

        self.print_hidden(to_hide)
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

        self.print_unhidden(to_unhide)
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
            if self
                .patch_updates
                .get(colliding_patchname)
                .map_or(true, |maybe_patch| maybe_patch.is_some())
            {
                return Err(anyhow!("Patch `{colliding_patchname}` already exists"));
            }
        } else if !self.stack.has_patch(old_patchname) {
            return Err(anyhow!("Patch `{old_patchname}` does not exist"));
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

        let patch = self.stack.get_patch(old_patchname).clone();
        self.patch_updates.insert(old_patchname.clone(), None);
        self.patch_updates
            .insert(new_patchname.clone(), Some(patch));

        self.print_rename(old_patchname, new_patchname)
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

        self.print_popped(&all_popped)?;

        // Gather contiguous groups of deleted patchnames for printing.
        let mut deleted_group: Vec<PatchName> = Vec::with_capacity(all_popped.len());

        for patchname in all_popped {
            if should_delete(&patchname) {
                deleted_group.push(patchname.clone());
                self.patch_updates.insert(patchname, None);
            } else if !deleted_group.is_empty() {
                self.print_deleted(&deleted_group)?;
                deleted_group.clear();
            }
        }

        for patchname in unapplied {
            if should_delete(&patchname) {
                deleted_group.push(patchname.clone());
                self.patch_updates.insert(patchname, None);
            } else {
                self.print_deleted(&deleted_group)?;
                deleted_group.clear();
                self.unapplied.push(patchname);
            }
        }

        let mut i = 0;
        while i < self.hidden.len() {
            if should_delete(&self.hidden[i]) {
                let patchname = self.hidden.remove(i);
                deleted_group.push(patchname.clone());
                self.patch_updates.insert(patchname, None);
            } else {
                i += 1;
                self.print_deleted(&deleted_group)?;
                deleted_group.clear();
            }
        }

        if !deleted_group.is_empty() {
            self.print_deleted(&deleted_group)?;
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

        self.print_popped(&all_popped)?;

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
        self.stack.repo.with_temp_index_file(|temp_index| {
            let mut temp_index_tree_id: Option<git2::Oid> = None;

            let merged = if check_merged {
                Some(self.check_merged(patchnames, temp_index, &mut temp_index_tree_id)?)
            } else {
                None
            };

            for (i, patchname) in patchnames.iter().enumerate() {
                let patchname = patchname.as_ref();
                let is_last = i + 1 == patchnames.len();
                let already_merged = merged
                    .as_ref()
                    .map(|merged| merged.contains(&patchname))
                    .unwrap_or(false);
                self.push_patch(
                    patchname,
                    already_merged,
                    is_last,
                    temp_index,
                    &mut temp_index_tree_id,
                )?;
            }
            Ok(())
        })
    }

    fn push_patch(
        &mut self,
        patchname: &PatchName,
        already_merged: bool,
        is_last: bool,
        temp_index: &mut git2::Index,
        temp_index_tree_id: &mut Option<git2::Oid>,
    ) -> Result<()> {
        let repo = self.stack.repo;
        let config = repo.config()?;
        let stupid = repo.stupid();
        let default_committer = git2::Signature::default_committer(Some(&config))?;
        let patch_commit = self.get_patch_commit(patchname).clone();
        let old_parent = patch_commit.parent(0)?;
        let new_parent = self.top().clone();

        let mut push_status = PushStatus::Unmodified;

        let new_tree_id = if already_merged {
            push_status = PushStatus::AlreadyMerged;
            new_parent.tree_id()
        } else if old_parent.tree_id() == new_parent.tree_id() {
            patch_commit.tree_id()
        } else if old_parent.tree_id() == patch_commit.tree_id() {
            new_parent.tree_id()
        } else if new_parent.tree_id() == patch_commit.tree_id() {
            patch_commit.tree_id()
        } else {
            let (ours, theirs) = if temp_index_tree_id == &Some(patch_commit.tree_id()) {
                (patch_commit.tree_id(), new_parent.tree_id())
            } else {
                (new_parent.tree_id(), patch_commit.tree_id())
            };
            let base = old_parent.tree_id();
            // let ours = new_parent.tree_id();
            // let theirs = patch_commit.tree_id();

            let stupid_temp = stupid.with_index_path(temp_index.path().unwrap());

            if temp_index_tree_id != &Some(ours) {
                stupid_temp.read_tree(ours)?;
                *temp_index_tree_id = Some(ours);
            }

            let maybe_tree_id = if stupid_temp.apply_treediff_to_index(base, theirs)? {
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
            } else {
                if stupid
                    .read_tree_checkout(self.current_tree_id, ours)
                    .is_err()
                {
                    return Err(Error::TransactionHalt {
                        msg: "Index/worktree dirty".to_string(),
                        conflicts: false,
                    }
                    .into());
                }
                self.current_tree_id = ours;

                let use_mergetool = config.get_bool("stgit.autoimerge").unwrap_or(false);
                match stupid.merge_recursive_or_mergetool(base, ours, theirs, use_mergetool) {
                    Ok(true) => {
                        // Success, no conflicts
                        let tree_id = stupid.write_tree().map_err(|_| Error::TransactionHalt {
                            msg: "Conflicting merge".to_string(),
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

        if new_tree_id != patch_commit.tree_id() || new_parent.id() != old_parent.id() {
            let commit_id = repo.commit_ex(
                &patch_commit.author_strict()?,
                &default_committer,
                &patch_commit.message_ex(),
                new_tree_id,
                [new_parent.id()],
            )?;
            let commit = repo.find_commit(commit_id)?;
            stupid.notes_copy(patch_commit.id(), commit_id).ok();
            if push_status == PushStatus::Conflict {
                // In the case of a conflict, update() will be called after the
                // execute() performs the checkout. Setting the transaction head
                // here ensures that the real stack top will be checked-out.
                self.updated_head = Some(commit.clone());
            } else if push_status != PushStatus::AlreadyMerged
                && new_tree_id == new_parent.tree_id()
            {
                push_status = PushStatus::Empty;
            }

            self.patch_updates
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

        self.print_pushed(patchname, push_status, is_last)?;

        if push_status == PushStatus::Conflict {
            Err(Error::TransactionHalt {
                msg: "Merge conflicts".to_string(),
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
        temp_index: &mut git2::Index,
        temp_index_tree_id: &mut Option<git2::Oid>,
    ) -> Result<Vec<&'a PatchName>>
    where
        P: AsRef<PatchName>,
    {
        let repo = self.stack.repo;
        let stupid = repo.stupid();
        let stupid = stupid.with_index_path(temp_index.path().unwrap());
        let head_tree_id = self.stack.branch_head.tree_id();
        let mut merged: Vec<&PatchName> = vec![];

        if temp_index_tree_id != &Some(head_tree_id) {
            stupid.read_tree(head_tree_id)?;
            *temp_index_tree_id = Some(head_tree_id);
        }

        for patchname in patchnames.iter().rev() {
            let patchname = patchname.as_ref();
            let patch_commit = self.get_patch_commit(patchname);

            if patch_commit.is_no_change()? {
                continue; // No change
            }

            let parent_commit = patch_commit.parent(0)?;

            if stupid.apply_treediff_to_index(patch_commit.tree_id(), parent_commit.tree_id())? {
                merged.push(patchname);
                *temp_index_tree_id = None;
            }
        }

        self.print_merged(&merged)?;

        Ok(merged)
    }

    fn print_merged(&self, merged_patches: &[&PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        write!(output, "Found ")?;
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, "{}", merged_patches.len())?;
        output.reset()?;
        let plural = if merged_patches.len() == 1 { "" } else { "es" };
        writeln!(output, " patch{plural} merged upstream")?;
        Ok(())
    }

    fn print_rename(&self, old_patchname: &PatchName, new_patchname: &PatchName) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_dimmed(true))?;
        write!(output, "{old_patchname}")?;
        color_spec.clear();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, " => ")?;
        output.reset()?;
        writeln!(output, "{new_patchname}")?;
        Ok(())
    }

    fn print_committed(&self, committed: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::White)))?;
        write!(output, "$ ")?;
        color_spec.set_fg(None);
        output.set_color(color_spec.set_intense(true))?;
        write!(output, "{}", committed[0])?;
        if committed.len() > 1 {
            output.set_color(color_spec.set_intense(false))?;
            write!(output, "..")?;
            output.set_color(color_spec.set_intense(true))?;
            let last = &committed[committed.len() - 1];
            write!(output, "{last}")?;
        }
        output.reset()?;
        writeln!(output)?;
        Ok(())
    }

    fn print_deleted(&self, deleted: &[PatchName]) -> Result<()> {
        if !deleted.is_empty() {
            let mut output = self.output.borrow_mut();
            let mut color_spec = termcolor::ColorSpec::new();
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
            write!(output, "# ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            write!(output, "{}", deleted[0])?;
            if deleted.len() > 1 {
                output.set_color(color_spec.set_dimmed(false))?;
                write!(output, "..")?;
                output.set_color(color_spec.set_dimmed(true))?;
                let last = &deleted[deleted.len() - 1];
                write!(output, "{last}")?;
            }
            output.reset()?;
            writeln!(output)?;
        }
        Ok(())
    }

    fn print_hidden(&self, hidden: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in hidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Red)))?;
            write!(output, "! ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true).set_italic(true))?;
            writeln!(output, "{patchname}")?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    fn print_unhidden(&self, unhidden: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in unhidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(output, "- ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            writeln!(output, "{patchname}")?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    fn print_popped(&self, popped: &[PatchName]) -> Result<()> {
        if !popped.is_empty() {
            let mut output = self.output.borrow_mut();
            let mut color_spec = termcolor::ColorSpec::new();
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(output, "- ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            write!(output, "{}", popped[0])?;
            if popped.len() > 1 {
                output.set_color(color_spec.set_dimmed(false))?;
                write!(output, "..")?;
                output.set_color(color_spec.set_dimmed(true))?;
                let last = &popped[popped.len() - 1];
                write!(output, "{last}")?;
            }
            output.reset()?;
            writeln!(output)?;
        }
        Ok(())
    }

    fn print_pushed(
        &mut self,
        patchname: &PatchName,
        status: PushStatus,
        is_last: bool,
    ) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let sigil = if is_last { '>' } else { '+' };
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(
            color_spec.set_fg(Some(if let PushStatus::Conflict = status {
                termcolor::Color::Red
            } else if is_last {
                termcolor::Color::Blue
            } else {
                termcolor::Color::Green
            })),
        )?;
        write!(output, "{sigil} ")?;
        color_spec.clear();
        output.set_color(color_spec.set_bold(is_last).set_intense(!is_last))?;
        write!(output, "{patchname}")?;
        output.reset()?;

        let status_str = match status {
            PushStatus::New => " (new)",
            PushStatus::AlreadyMerged => " (merged)",
            PushStatus::Conflict => " (conflict)",
            PushStatus::Empty => " (empty)",
            PushStatus::Modified => " (modified)",
            PushStatus::Unmodified => "",
        };

        writeln!(output, "{status_str}")?;
        if is_last {
            self.printed_top = true;
        }
        Ok(())
    }

    fn print_updated(&self, patchname: &PatchName) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let (is_applied, is_top) =
            if let Some(pos) = self.applied().iter().position(|pn| pn == patchname) {
                (true, pos + 1 == self.applied.len())
            } else {
                (false, false)
            };
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Cyan)))?;
        write!(output, "& ")?;
        color_spec.clear();
        output.set_color(
            color_spec
                .set_bold(is_top)
                .set_intense(is_applied && !is_top)
                .set_dimmed(!is_applied),
        )?;
        writeln!(output, "{patchname}")?;
        output.reset()?;
        Ok(())
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
        if let Some(maybe_patch) = self.patch_updates.get(patchname) {
            maybe_patch
                .as_ref()
                .expect("should not attempt to access deleted patch")
        } else {
            self.stack.get_patch(patchname)
        }
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        if let Some(maybe_patch) = self.patch_updates.get(patchname) {
            maybe_patch.is_some()
        } else {
            self.stack.has_patch(patchname)
        }
    }

    fn top(&self) -> &Commit<'repo> {
        if let Some(patchname) = self.applied.last() {
            self.get_patch_commit(patchname)
        } else {
            self.base()
        }
    }

    fn head(&self) -> &Commit<'repo> {
        if let Some(commit) = self.updated_head.as_ref() {
            commit
        } else {
            self.top()
        }
    }

    fn base(&self) -> &Commit<'repo> {
        if let Some(commit) = self.updated_base.as_ref() {
            commit
        } else {
            self.stack.base()
        }
    }
}
