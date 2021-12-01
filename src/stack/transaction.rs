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

pub(crate) struct TransactionContext<'repo>(StackTransaction<'repo>);
pub(crate) struct ExecuteContext<'repo>(StackTransaction<'repo>);

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
            let state_commit = transaction.stack.state_ref.peel_to_commit()?;
            let message = "external modifications\n\
                           \n\
                           Modifications by tools other than StGit (e.g. git).\n";
            let reflog_msg = Some("external modifications");
            // TODO: why update the stack state ref unconditional of transaction.error?
            let head_commit = transaction.stack.head_commit.clone();
            transaction
                .stack
                .advance_state(head_commit, state_commit, message, reflog_msg)?
        };

        let repo = transaction.stack.repo;

        let head_commit_id = transaction.head_commit_id();

        let set_head = true; // TODO: argument
        let allow_bad_head = false; // TODO: argument
        let use_index_and_worktree = true; // TODO: argument
        if set_head {
            if use_index_and_worktree {
                let head_commit = repo.find_commit(head_commit_id)?;
                let result = transaction.checkout(head_commit.as_object(), allow_bad_head);
                if let Err(err) = result {
                    let allow_bad_head = true;
                    transaction
                        .checkout(transaction.stack.head_tree.as_object(), allow_bad_head)?;
                    return Err(Error::TransactionAborted(err.to_string()));
                }
            }

            transaction
                .stack
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
        let state_refname = transaction.stack.state_ref.name().unwrap();
        let mut git_trans = repo.transaction()?;
        let reflog_signature = None; // Use default signature

        git_trans.lock_ref(state_refname)?;

        for (patchname, maybe_desc) in transaction.patch_updates {
            let patch_refname = transaction.stack.patch_refname(&patchname);
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
                    .insert(patchname, patch_desc);
            } else {
                git_trans.remove(&patch_refname)?;
                transaction.stack.state.patches.remove(&patchname);
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
        transaction.stack.state.prev = Some(prev_state_commit);
        transaction.stack.state.head = repo.find_commit(head_commit_id)?;
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

pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    conflict_mode: ConflictMode,
    discard_changes: bool,
    old_patches: BTreeMap<PatchName, PatchDescriptor<'repo>>,
    patch_updates: BTreeMap<PatchName, Option<PatchDescriptor<'repo>>>,
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

impl<'repo> StackTransaction<'repo> {
    pub(crate) fn make_context(
        stack: Stack<'repo>,
        conflict_mode: ConflictMode,
        discard_changes: bool,
    ) -> TransactionContext {
        let current_tree_id = stack.head_tree.id();
        let old_head_commit_id = stack.head_commit.id();
        let old_patches = stack.state.patches.clone();
        let applied = stack.state.applied.clone();
        let unapplied = stack.state.unapplied.clone();
        let hidden = stack.state.hidden.clone();
        TransactionContext(Self {
            stack,
            conflict_mode,
            discard_changes,
            old_patches,
            patch_updates: BTreeMap::new(),
            applied,
            unapplied,
            hidden,
            error: None,
            updated_head_commit_id: None,
            old_head_commit_id,
            updated_base_commit_id: None,
            current_tree_id,
            conflicts: Vec::new(),
        })
    }

    pub(crate) fn top_commit_id(&self) -> Oid {
        if let Some(patchname) = self.applied.last() {
            if let Some(Some(patch_desc)) = self.patch_updates.get(patchname) {
                patch_desc.commit.id()
            } else {
                self.old_patches[patchname].commit.id()
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

    pub(crate) fn push_applied(&mut self, patchname: &PatchName, oid: Oid) -> Result<(), Error> {
        let commit = self.stack.repo.find_commit(oid)?;
        self.applied.push(patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchDescriptor { commit }));
        Ok(())
    }

    fn checkout(&self, treeish: &Object<'_>, allow_bad_head: bool) -> Result<(), Error> {
        let repo = self.stack.repo;
        if !allow_bad_head {
            self.stack.check_head_top_mismatch()?;
        }

        if repo.index()?.has_conflicts() {
            return Err(Error::OutstandingConflicts);
        }

        let tree = treeish.peel_to_tree()?;
        if self.current_tree_id != tree.id() && !self.discard_changes {
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

        Ok(repo.checkout_tree(treeish, Some(&mut checkout_builder))?)
    }
}
