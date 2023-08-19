// SPDX-License-Identifier: GPL-2.0-only

use std::collections::BTreeMap;

use anyhow::Result;

use super::{
    options::{ConflictMode, TransactionOptions},
    ui::TransactionUserInterface,
    ExecuteContext, StackTransaction,
};
use crate::stack::{Stack, StackAccess, StackStateAccess};

/// Builder used to setup a stack transaction.
pub(crate) struct TransactionBuilder<'repo> {
    stack: Stack<'repo>,
    output: Option<termcolor::StandardStream>,
    options: TransactionOptions,
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

    /// Allow pushing of a patch that results in conflicts. Will use the value of
    /// `"stgit.push.allow-conflicts"` if not set explicitly.
    #[must_use]
    pub(crate) fn allow_push_conflicts(mut self, allow: bool) -> Self {
        self.options.allow_push_conflicts = Some(allow);
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
    /// transaction executes successfully. This is the default. Disabling this is only useful
    /// in very special circumstances (e.g. for `stg uncommit`).
    #[must_use]
    pub(crate) fn set_head(mut self, yes: bool) -> Self {
        self.options.set_head = yes;
        self
    }

    /// Determines whether the committer date will be set to be the same as the author
    /// date when pushing patches.
    #[must_use]
    pub(crate) fn committer_date_is_author_date(mut self, yes: bool) -> Self {
        self.options.committer_date_is_author_date = yes;
        self
    }

    /// Perform stack transaction operations.
    ///
    /// The closure provided to this method may call various methods on the provided
    /// [`StackTransaction`] instance. The closure returns `Err`, any changes to the stack,
    /// index, or work tree will be rolled-back during the subsequent execution phase.
    ///
    /// N.B. [`super::Error::TransactionHalt`] errors do not trigger rollback.
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

        let ui =
            TransactionUserInterface::new(output.expect("with_output_stream() must be called"));

        let current_tree_id = stack
            .get_branch_head()
            .tree_id()
            .expect("branch head commit is decodable")
            .detach();
        let applied = stack.applied().to_vec();
        let unapplied = stack.unapplied().to_vec();
        let hidden = stack.hidden().to_vec();

        let mut transaction = StackTransaction {
            stack,
            ui,
            options,
            applied,
            unapplied,
            hidden,
            updated_patches: BTreeMap::new(),
            updated_head: None,
            updated_base: None,
            current_tree_id,
            error: None,
        };

        transaction.error = f(&mut transaction).err();

        ExecuteContext(transaction)
    }
}
