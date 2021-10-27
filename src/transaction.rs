use std::collections::BTreeMap;

use git2::Oid;

use crate::error::Error;
use crate::patchname::PatchName;
use crate::stack::{PatchDescriptor, Stack};

pub(crate) enum ConflictMode {
    Disallow,
    // Allow,
    // AllowIfSameTop,
}

pub(crate) struct TransactionContext(StackTransaction);
pub(crate) struct ExecuteContext(StackTransaction);

impl TransactionContext {
    #[must_use]
    pub(crate) fn transact<F>(mut self, f: F) -> ExecuteContext
    where
        F: FnOnce(&mut StackTransaction) -> Result<(), Error>,
    {
        // TODO: capture result as transaction.error.
        let _result = f(&mut self.0);
        ExecuteContext(self.0)
    }
}

impl ExecuteContext {
    pub(crate) fn execute(self, stack: Stack, _log_message: &str) -> Result<(), Error> {
        let transaction = self.0;

        // Check consistency
        for (patchname, oid) in transaction.patch_updates.iter() {
            if oid.is_none() {
                assert!(stack.state.patches.contains_key(patchname));
            } else {
                assert!(transaction.patch_updates.contains_key(patchname));
            }
        }

        // Log external modifications
        let stack = if !stack.is_head_top()? {
            let head_commit_oid = stack.head_commit()?.id();
            let prev_state_commit = stack.state_ref.peel_to_commit()?;
            let message = "external modifications\n\
                           \n\
                           Modifications by tools other than StGit (e.g. git).\n";
            let reflog_msg = Some("external modifications");
            // TODO: why update the stack state ref unconditional of transaction.error?
            let stack = stack.advance_state(
                head_commit_oid,
                prev_state_commit.id(),
                message,
                reflog_msg,
            )?;
            stack
        } else {
            stack
        };

        let set_head = true; // TODO: argument
        let allow_bad_head = false; // TODO: argument
        let use_index_and_worktree = true; // TODO: argument
        if set_head {
            if use_index_and_worktree {
                // TODO: checkout and such
                let result = transaction.checkout(&stack, allow_bad_head);
            }
        }

        // if let Some(err) = transaction.error {
        //     if Some(conflicts) = transaction.conflicts {
        //     } else {
        //     }
        // }

        Ok(()) // TODO
    }
}

pub(crate) struct StackTransaction {
    conflict_mode: ConflictMode,
    discard_changes: bool,
    patch_updates: BTreeMap<PatchName, Option<PatchDescriptor>>,
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    error: Option<Error>,
    updated_head_commit_id: Option<Oid>,
    updated_tree_id: Option<Oid>,
}

impl StackTransaction {
    pub(crate) fn make_context(
        stack: &Stack,
        conflict_mode: ConflictMode,
        discard_changes: bool,
    ) -> TransactionContext {
        TransactionContext(Self {
            conflict_mode,
            discard_changes,
            patch_updates: BTreeMap::new(),
            applied: stack.state.applied.clone(),
            unapplied: stack.state.unapplied.clone(),
            hidden: stack.state.hidden.clone(),
            error: None,
            updated_head_commit_id: None,
            updated_tree_id: None,
        })
    }

    pub(crate) fn push_applied(&mut self, patchname: &PatchName, oid: &Oid) {
        self.applied.push(patchname.clone());
        self.patch_updates.insert(
            patchname.clone(),
            Some(PatchDescriptor { oid: oid.clone() }),
        );
    }

    fn checkout(&self, stack: &Stack, allow_bad_head: bool) -> Result<(), Error> {
        if !allow_bad_head {
            stack.check_head_top_mismatch()?;
        }
        if let Some(tree_id) = self.updated_tree_id {
            if self.discard_changes {
                // checkout hard
            } else {
                // checkout not hard?
            }
            Ok(())
        } else {
            match self.conflict_mode {
                ConflictMode::Disallow => {
                    let conflicts = todo!();
                    if !conflicts.is_empty() {
                        // TODO "Need to resolve conflicts first"
                        Err(Error::Transaction)
                    }
                }
            }

            Ok(())
        }
    }
}
