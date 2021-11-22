use std::collections::BTreeMap;
use std::ffi::OsString;

use git2::{Object, Oid, RepositoryState};

use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;
use crate::stack::{PatchDescriptor, Stack};

use super::iter::AllPatches;

pub(crate) enum ConflictMode {
    Disallow,

    #[allow(dead_code)]
    Allow,

    #[allow(dead_code)]
    AllowIfSameTop,
}

pub(crate) struct TransactionContext(StackTransaction);
pub(crate) struct ExecuteContext(StackTransaction);

impl TransactionContext {
    #[must_use]
    pub(crate) fn transact<F>(mut self, f: F) -> ExecuteContext
    where
        F: FnOnce(&mut StackTransaction) -> Result<(), Error>,
    {
        self.0.error = f(&mut self.0).err();
        ExecuteContext(self.0)
    }
}

impl ExecuteContext {
    pub(crate) fn execute(self, stack: Stack, reflog_msg: &str) -> Result<(), Error> {
        let transaction = self.0;

        // Check consistency
        for (patchname, oid) in transaction.patch_updates.iter() {
            if oid.is_none() {
                assert!(stack.state.patches.contains_key(patchname));
            } else {
                assert!(transaction.all_patches().any(|pn| pn == patchname));
            }
        }

        // Log external modifications
        let mut stack = if stack.is_head_top() {
            stack
        } else {
            let prev_state_commit = stack.state_ref.peel_to_commit()?;
            let head_commit_id = stack.head_commit.id();
            let message = "external modifications\n\
                           \n\
                           Modifications by tools other than StGit (e.g. git).\n";
            let reflog_msg = Some("external modifications");
            // TODO: why update the stack state ref unconditional of transaction.error?
            let stack =
                stack.advance_state(head_commit_id, prev_state_commit.id(), message, reflog_msg)?;
            stack
        };

        let head_commit_id = transaction.head_commit_id();

        let set_head = true; // TODO: argument
        let allow_bad_head = false; // TODO: argument
        let use_index_and_worktree = true; // TODO: argument
        if set_head {
            if use_index_and_worktree {
                let head_commit = stack.repo.find_commit(head_commit_id)?;
                let result = transaction.checkout(&stack, head_commit.as_object(), allow_bad_head);
                if let Err(err) = result {
                    let allow_bad_head = true;
                    transaction.checkout(&stack, stack.head_tree.as_object(), allow_bad_head)?;
                    return Err(Error::TransactionAborted(err.to_string()));
                }
            }

            stack
                .branch
                .get_mut()
                .set_target(head_commit_id, reflog_msg)?;
        }

        let conflict_msg = format!("{} (CONFLICT)", reflog_msg);
        let reflog_msg = if transaction.conflicts.is_empty() {
            reflog_msg
        } else {
            &conflict_msg
        };

        // Update patch refs and stack state refs
        let state_refname = stack.state_ref.name().unwrap();
        let mut git_trans = stack.repo.transaction()?;
        let reflog_signature = None; // Use default signature

        git_trans.lock_ref(state_refname)?;

        for (patchname, maybe_desc) in transaction.patch_updates {
            let patch_refname = stack.patch_refname(&patchname);
            git_trans.lock_ref(&patch_refname)?;

            if let Some(patch_desc) = maybe_desc {
                git_trans.set_target(
                    &patch_refname,
                    patch_desc.oid,
                    reflog_signature,
                    reflog_msg,
                )?;
                stack.state.patches.insert(patchname, patch_desc);
            } else {
                git_trans.remove(&patch_refname)?;
                stack.state.patches.remove(&patchname);
            }
        }

        // For printing applied patch name...
        let _old_applied_pn = stack.state.applied.last().map(|pn| pn.to_string());
        let _new_applied_pn = transaction.applied.last().map(|pn| pn.to_string());

        let prev_state_commit = stack.state_ref.peel_to_commit()?;
        stack.state.prev = Some(prev_state_commit.id());
        stack.state.head = head_commit_id;
        stack.state.applied = transaction.applied;
        stack.state.unapplied = transaction.unapplied;
        stack.state.hidden = transaction.hidden;

        let state_commit_id = stack.state.commit(stack.repo, None, reflog_msg)?;
        git_trans.set_target(state_refname, state_commit_id, reflog_signature, reflog_msg)?;

        git_trans.commit()?;

        if let Some(err) = transaction.error {
            Err(err)
        } else {
            Ok(())
        }
    }
}

pub(crate) struct StackTransaction {
    conflict_mode: ConflictMode,
    discard_changes: bool,
    old_patches: BTreeMap<PatchName, PatchDescriptor>,
    patch_updates: BTreeMap<PatchName, Option<PatchDescriptor>>,
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    updated_head_commit_id: Option<Oid>,
    old_head_commit_id: Oid,
    updated_base_commit_id: Option<Oid>,
    current_tree_id: Oid,
    error: Option<Error>,
    conflicts: Vec<OsString>,
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
            old_patches: stack.state.patches.clone(),
            patch_updates: BTreeMap::new(),
            applied: stack.state.applied.clone(),
            unapplied: stack.state.unapplied.clone(),
            hidden: stack.state.hidden.clone(),
            error: None,
            updated_head_commit_id: None,
            old_head_commit_id: stack.head_commit.id(),
            updated_base_commit_id: None,
            current_tree_id: stack.head_tree.id(),
            conflicts: Vec::new(),
        })
    }

    pub(crate) fn top_commit_id(&self) -> Oid {
        if let Some(patchname) = self.applied.last() {
            if let Some(Some(patch_desc)) = self.patch_updates.get(patchname) {
                patch_desc.oid
            } else {
                self.old_patches[patchname].oid
            }
        } else {
            self.base_commit_id()
        }
    }

    pub(crate) fn head_commit_id(&self) -> Oid {
        if let Some(commit_id) = self.updated_head_commit_id {
            commit_id
        } else {
            self.top_commit_id()
        }
    }

    pub(crate) fn base_commit_id(&self) -> Oid {
        if let Some(commit_id) = self.updated_base_commit_id {
            commit_id
        } else {
            self.old_head_commit_id
        }
    }

    pub(crate) fn all_patches(&self) -> AllPatches {
        AllPatches::new(&self.applied, &self.unapplied, &self.hidden)
    }

    pub(crate) fn push_applied(&mut self, patchname: &PatchName, oid: Oid) {
        self.applied.push(patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchDescriptor { oid }));
    }

    fn checkout(
        &self,
        stack: &Stack,
        treeish: &Object<'_>,
        allow_bad_head: bool,
    ) -> Result<(), Error> {
        if !allow_bad_head {
            stack.check_head_top_mismatch()?;
        }

        if stack.repo.index()?.has_conflicts() {
            return Err(Error::OutstandingConflicts);
        }

        let tree = treeish.peel_to_tree()?;
        if self.current_tree_id != tree.id() && !self.discard_changes {
            return match stack.repo.state() {
                RepositoryState::Clean => Ok(()),
                RepositoryState::Merge | RepositoryState::RebaseMerge => match self.conflict_mode {
                    ConflictMode::Disallow => Err(Error::OutstandingConflicts),
                    ConflictMode::Allow => Ok(()),
                    ConflictMode::AllowIfSameTop => {
                        let top = self.applied.last();
                        if top.is_some() && top == stack.state.applied.last() {
                            Ok(())
                        } else {
                            Err(Error::OutstandingConflicts)
                        }
                    }
                },
                state => Err(Error::ActiveRepositoryState(
                    repo_state_to_str(state).to_string(),
                )),
            };
        }

        let mut checkout_builder = git2::build::CheckoutBuilder::new();
        if self.discard_changes {
            checkout_builder.force();
        }

        Ok(stack
            .repo
            .checkout_tree(treeish, Some(&mut checkout_builder))?)
    }
}
