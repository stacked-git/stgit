use std::collections::BTreeMap;
use std::ffi::OsString;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

use git2::{Commit, Index, Oid, Repository, RepositoryState};

use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;
use crate::stack::{PatchDescriptor, Stack};
use crate::wrap::repository::commit_ex;
use crate::wrap::signature::{self, same_signature};
use crate::wrap::CommitData;

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
                if let Some(old_desc) = transaction
                    .stack
                    .state
                    .patches
                    .insert(patchname.clone(), patch_desc.clone())
                {
                    if let Ok(old_note) =
                        transaction.stack.repo.find_note(None, old_desc.commit.id())
                    {
                        transaction.stack.repo.note(
                            &old_note.author(),
                            &old_note.committer(),
                            None,
                            patch_desc.commit.id(),
                            old_note.message().unwrap(),
                            false,
                        )?;
                    }
                }
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

        if self.current_tree_id == commit.tree_id() && !self.discard_changes {
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

    fn get_patch_descriptor(&self, patchname: &PatchName) -> &PatchDescriptor {
        if let Some(maybe_desc) = self.patch_updates.get(patchname) {
            maybe_desc
                .as_ref()
                .expect("should not attempt to access deleted patch")
        } else {
            &self.stack.state.patches[patchname]
        }
    }

    pub(crate) fn push_tree(&mut self, patchname: &PatchName) -> Result<bool, Error> {
        let patch_desc = self.get_patch_descriptor(patchname);
        let patch_commit = &patch_desc.commit;
        let config = self.stack.repo.config()?;
        let default_committer = signature::default_committer(Some(&config))?;

        let patch_modified = !same_signature(&patch_commit.committer(), &default_committer)
            || patch_commit.parent_id(0)? != self.top().id();

        if patch_modified {
            let message = patch_commit
                .message_raw()
                .ok_or_else(|| Error::NonUtf8Message(patchname.to_string()))?;
            let parent_ids = vec![self.top().id()];
            let new_commit_id = commit_ex(
                self.stack.repo,
                &patch_commit.author(),
                &default_committer,
                message,
                patch_commit.tree_id(),
                &parent_ids,
            )?;

            let new_commit = self.stack.repo.find_commit(new_commit_id)?;
            let new_desc = PatchDescriptor { commit: new_commit };
            self.patch_updates.insert(patchname.clone(), Some(new_desc));
        }

        if let Some(pos) = self.unapplied.iter().position(|pn| pn == patchname) {
            self.unapplied.remove(pos);
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == patchname) {
            self.hidden.remove(pos);
        } else {
            panic!("push_tree `{}` was not in unapplied or hidden", patchname);
        }

        self.applied.push(patchname.clone());

        Ok(patch_modified)
    }

    // TODO: separate push_patch_no_iw() function
    pub(crate) fn push_patch(
        &mut self,
        patchname: &PatchName,
        already_merged: bool,
    ) -> Result<(), Error> {
        let patch_commit = {
            if let Some(maybe_desc) = self.patch_updates.get(patchname) {
                maybe_desc
                    .as_ref()
                    .expect("should not attempt to access deleted patch")
            } else {
                &self.stack.state.patches[patchname]
            }
        }
        .commit
        .clone();

        let repo = self.stack.repo;
        let config = repo.config()?;
        let default_committer = signature::default_committer(Some(&config))?;
        let new_parent = self.top();

        let mut cd = CommitData::from(&patch_commit);
        cd.parent_ids = vec![new_parent.id()];
        cd.committer = default_committer;

        let old_parent = patch_commit.parent(0)?;
        let old_base_tree = old_parent.tree()?;

        let mut merge_conflict = false;

        cd.tree_id = if already_merged {
            old_base_tree.id()
        } else {
            let base = old_base_tree;
            let ours = new_parent.tree()?;
            let theirs = patch_commit.tree()?;
            let merge_options = None;
            let mut temp_index = repo.merge_trees(&base, &ours, &theirs, merge_options)?;
            if temp_index.has_conflicts() {
                // TODO stgit.autoimerge
                let mut opts = git2::build::CheckoutBuilder::new();
                opts.allow_conflicts(false);
                opts.our_label("current");
                opts.their_label(patchname.as_ref()); // Compat: "patch"
                repo.checkout_index(Some(&mut temp_index), Some(&mut opts))
                    .map_err(|e| {
                        if e.class() == git2::ErrorClass::Checkout
                            && e.code() == git2::ErrorCode::Conflict
                        {
                            Error::CheckoutConflicts(e.message().to_string())
                        } else {
                            Error::Git(e)
                        }
                    })?;
                self.current_tree_id = ours.id();

                merge_conflict = true;
                for conflict in temp_index.conflicts()? {
                    let conflict = conflict?;
                    if let Some(entry) = conflict.our {
                        let path = std::ffi::OsStr::from_bytes(&entry.path).to_os_string();
                        self.conflicts.push(path);
                    }
                }
                ours.id()
            } else {
                temp_index.write_tree_to(repo)?
            }
        };

        let new_patch_commit = if *cd.parent_ids.first().unwrap()
            != patch_commit.parent_id(0).unwrap()
            || cd.tree_id != patch_commit.tree_id()
            || cd.message.as_bytes() != patch_commit.message_raw_bytes()
            || !same_signature(&cd.author, &patch_commit.author())
        {
            let new_patch_commit_id = cd.commit(repo)?;
            let new_patch_commit = repo.find_commit(new_patch_commit_id)?;
            if merge_conflict {
                // In the case of a conflict, update() will be called after the
                // execute() performs the checkout. Setting the transaction head
                // here ensures that the real stack top will be checked-out.
                self.updated_head = Some(new_patch_commit.clone());
            }
            Some(new_patch_commit)
        } else {
            None
        };

        if merge_conflict {
            // The final checkout at execute-time must allow these push conflicts.
            self.conflict_mode = ConflictMode::Allow;
        }

        if let Some(new_patch_commit) = new_patch_commit {
            let patch_desc = PatchDescriptor {
                commit: new_patch_commit,
            };
            self.patch_updates
                .insert(patchname.clone(), Some(patch_desc));
        }

        if let Some(pos) = self.unapplied.iter().position(|pn| pn == patchname) {
            self.unapplied.remove(pos);
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == patchname) {
            self.hidden.remove(pos);
        }
        self.applied.push(patchname.clone());

        if merge_conflict {
            Err(Error::MergeConflicts(self.conflicts.len()))
        } else {
            Ok(())
        }
    }

    pub(crate) fn check_merged<'a>(
        &mut self,
        patches: &'a [PatchName],
    ) -> Result<Vec<&'a PatchName>, Error> {
        fn with_temp_index<F>(repo: &Repository, f: F) -> Result<(), Error>
        where
            F: FnOnce(&mut Index) -> Result<(), Error>,
        {
            let mut temp_index = Index::new()?;
            let mut orig_index = repo.index()?;
            repo.set_index(&mut temp_index)?;
            let result = f(&mut temp_index);
            repo.set_index(&mut orig_index)
                .expect("can reset to original index");
            result
        }

        let repo = self.stack.repo;
        let mut merged: Vec<&PatchName> = vec![];
        let head_tree = self.stack.head.tree()?;

        with_temp_index(repo, |temp_index| {
            temp_index.read_tree(&head_tree)?;

            for patchname in patches.iter().rev() {
                let patch_desc = {
                    if let Some(maybe_desc) = self.patch_updates.get(patchname) {
                        maybe_desc
                            .as_ref()
                            .expect("should not attempt to access deleted patch")
                    } else {
                        &self.stack.state.patches[patchname]
                    }
                };
                let patch_commit = &patch_desc.commit;
                let patch_tree = patch_commit.tree()?;
                let parent_commit = patch_commit.parent(0)?;
                let parent_tree = parent_commit.tree()?;

                if patch_commit.parent_count() == 1 && patch_tree.id() == parent_tree.id() {
                    continue; // No change
                }

                let diff = repo.diff_tree_to_tree(Some(&patch_tree), Some(&parent_tree), None)?;
                let result = repo.apply_to_tree(&head_tree, &diff, None);
                match result {
                    Ok(index) => {
                        if !index.has_conflicts() {
                            merged.push(patchname);
                        }
                    }
                    Err(e) => {
                        dbg!(e);
                    }
                }
            }

            Ok(())
        })?;

        Ok(merged)
    }
}
