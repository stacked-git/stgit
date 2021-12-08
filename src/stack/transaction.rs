use std::collections::BTreeMap;
use std::ffi::OsString;

use git2::{Commit, Oid, RepositoryState};

use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;
use crate::stack::{PatchDescriptor, Stack};

use super::iter::AllPatches;

pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    conflict_mode: ConflictMode,
    discard_changes: bool,
    patch_updates: BTreeMap<PatchName, Option<PatchDescriptor<'repo>>>,
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    updated_head: Option<Commit<'repo>>,
    updated_base: Option<Commit<'repo>>,
    current_tree_id: Oid,
    error: Option<Error>,
    conflicts: Vec<OsString>,
}

pub(crate) struct TransactionContext<'repo>(StackTransaction<'repo>);
pub(crate) struct ExecuteContext<'repo>(StackTransaction<'repo>);

pub(crate) enum ConflictMode {
    Disallow,

    #[allow(dead_code)]
    Allow,

    #[allow(dead_code)]
    AllowIfSameTop,
}

impl<'repo> TransactionContext<'repo> {
    #[must_use]
    pub(crate) fn transact<F>(mut self, f: F) -> ExecuteContext<'repo>
    where
        F: FnOnce(&mut StackTransaction) -> Result<(), Error>,
    {
        self.0.error = f(&mut self.0).err();
        ExecuteContext(self.0)
    }
}

impl<'repo> ExecuteContext<'repo> {
    pub(crate) fn execute(self, reflog_msg: &str) -> Result<Stack<'repo>, Error> {
        let mut transaction = self.0;

        // Check consistency
        for (patchname, oid) in transaction.patch_updates.iter() {
            if oid.is_none() {
                assert!(transaction.stack.state.patches.contains_key(patchname));
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

        let trans_head = transaction.head().clone();
        let trans_head_id = trans_head.id();

        let set_head = true; // TODO: argument
        let allow_bad_head = false; // TODO: argument
        let use_index_and_worktree = true; // TODO: argument
        if set_head {
            if use_index_and_worktree {
                let stack_head = transaction.stack.head.clone();
                let result = transaction.checkout(&trans_head, allow_bad_head);
                if let Err(err) = result {
                    let allow_bad_head = true;
                    transaction.checkout(&stack_head, allow_bad_head)?;
                    return Err(Error::TransactionAborted(err.to_string()));
                }
            }

            transaction
                .stack
                .branch
                .get_mut()
                .set_target(trans_head_id, reflog_msg)?;
        }

        let conflict_msg = format!("{} (CONFLICT)", reflog_msg);
        let reflog_msg = if transaction.conflicts.is_empty() {
            reflog_msg
        } else {
            &conflict_msg
        };

        // Update patch refs and stack state refs
        let state_refname = transaction.stack.state_ref.name().unwrap();
        let mut git_trans = repo.transaction()?;
        let reflog_signature = None; // Use default signature

        git_trans.lock_ref(state_refname)?;

        for (patchname, maybe_desc) in &transaction.patch_updates {
            let patch_refname = transaction.stack.patch_refname(patchname);
            git_trans.lock_ref(&patch_refname)?;

            if let Some(patch_desc) = maybe_desc {
                git_trans.set_target(
                    &patch_refname,
                    patch_desc.commit.id(),
                    reflog_signature,
                    reflog_msg,
                )?;
                transaction
                    .stack
                    .state
                    .patches
                    .insert(patchname.clone(), patch_desc.clone());
            } else {
                git_trans.remove(&patch_refname)?;
                transaction.stack.state.patches.remove(patchname);
            }
        }

        // For printing applied patch name...
        let _old_applied_pn = transaction
            .stack
            .state
            .applied
            .last()
            .map(|pn| pn.to_string());
        let _new_applied_pn = transaction.applied.last().map(|pn| pn.to_string());

        let prev_state_commit = transaction.stack.state_ref.peel_to_commit()?;
        let head = transaction.head().clone();
        transaction.stack.state.prev = Some(prev_state_commit);
        transaction.stack.state.head = head;
        transaction.stack.state.applied = transaction.applied;
        transaction.stack.state.unapplied = transaction.unapplied;
        transaction.stack.state.hidden = transaction.hidden;

        let state_commit_id = transaction.stack.state.commit(repo, None, reflog_msg)?;
        git_trans.set_target(state_refname, state_commit_id, reflog_signature, reflog_msg)?;

        git_trans.commit()?;

        if let Some(err) = transaction.error {
            Err(err)
        } else {
            Ok(transaction.stack)
        }
    }
}

impl<'repo> StackTransaction<'repo> {
    pub(crate) fn make_context(
        stack: Stack<'repo>,
        conflict_mode: ConflictMode,
        discard_changes: bool,
    ) -> TransactionContext {
        let current_tree_id = stack.head.tree_id();
        let applied = stack.state.applied.clone();
        let unapplied = stack.state.unapplied.clone();
        let hidden = stack.state.hidden.clone();
        TransactionContext(Self {
            stack,
            conflict_mode,
            discard_changes,
            patch_updates: BTreeMap::new(),
            applied,
            unapplied,
            hidden,
            error: None,
            updated_head: None,
            updated_base: None,
            current_tree_id,
            conflicts: Vec::new(),
        })
    }

    pub(crate) fn top(&self) -> &Commit<'repo> {
        if let Some(patchname) = self.applied.last() {
            if let Some(maybe_desc) = self.patch_updates.get(patchname) {
                &maybe_desc
                    .as_ref()
                    .expect("top should not attempt to access deleted patch")
                    .commit
            } else {
                &self.stack.state.patches[patchname].commit
            }
        } else {
            self.base()
        }
    }

    pub(crate) fn base(&self) -> &Commit<'repo> {
        if let Some(commit) = self.updated_base.as_ref() {
            commit
        } else {
            &self.stack.base
        }
    }

    pub(crate) fn head(&self) -> &Commit<'repo> {
        if let Some(commit) = self.updated_head.as_ref() {
            commit
        } else {
            self.top()
        }
    }

    pub(crate) fn all_patches(&self) -> AllPatches {
        AllPatches::new(&self.applied, &self.unapplied, &self.hidden)
    }

    pub(crate) fn push_applied(&mut self, patchname: &PatchName, oid: Oid) -> Result<(), Error> {
        let commit = self.stack.repo.find_commit(oid)?;
        self.applied.push(patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchDescriptor { commit }));
        Ok(())
    }

    fn checkout(&mut self, commit: &Commit<'_>, allow_bad_head: bool) -> Result<(), Error> {
        let repo = self.stack.repo;
        if !allow_bad_head {
            self.stack.check_head_top_mismatch()?;
        }

        if repo.index()?.has_conflicts() {
            return Err(Error::OutstandingConflicts);
        }

        if self.current_tree_id != commit.tree_id() && !self.discard_changes {
            return match repo.state() {
                RepositoryState::Clean => Ok(()),
                RepositoryState::Merge | RepositoryState::RebaseMerge => match self.conflict_mode {
                    ConflictMode::Disallow => Err(Error::OutstandingConflicts),
                    ConflictMode::Allow => Ok(()),
                    ConflictMode::AllowIfSameTop => {
                        let top = self.applied.last();
                        if top.is_some() && top == self.stack.state.applied.last() {
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
        repo.checkout_tree(commit.as_object(), Some(&mut checkout_builder))?;
        self.current_tree_id = commit.tree_id();
        Ok(())
    }
}
