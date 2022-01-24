use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::io::Write;

use git2::{Commit, Oid, RepositoryState};
use indexmap::IndexSet;
use termcolor::WriteColor;

use crate::commit::{CommitExtended, CommitMessageExtended};
use crate::error::{repo_state_to_str, Error};
use crate::index::TemporaryIndex;
use crate::patchname::PatchName;
use crate::signature;
use crate::stack::{PatchState, Stack, StackStateAccess};
use crate::stupid;

pub(crate) struct TransactionBuilder<'repo> {
    stack: Stack<'repo>,
    output: Option<termcolor::StandardStream>,
    conflict_mode: ConflictMode,
    discard_changes: bool,
    use_index_and_worktree: bool,
}

pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    output: RefCell<termcolor::StandardStream>,
    conflict_mode: ConflictMode,
    discard_changes: bool,
    use_index_and_worktree: bool,

    patch_updates: BTreeMap<PatchName, Option<PatchState<'repo>>>,
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    updated_head: Option<Commit<'repo>>,
    updated_base: Option<Commit<'repo>>,
    current_tree_id: Oid,
    error: Option<Error>,
    conflicts: Vec<OsString>,
    printed_top: bool,
}

pub(crate) struct ExecuteContext<'repo>(StackTransaction<'repo>);

pub(crate) enum ConflictMode {
    Disallow,
    Allow,

    #[allow(dead_code)]
    AllowIfSameTop,
}

impl Default for ConflictMode {
    fn default() -> Self {
        Self::Disallow
    }
}

enum PushStatus {
    New,
    AlreadyMerged,
    Conflict,
    Empty,
    Modified,
    Unmodified,
}

impl<'repo> TransactionBuilder<'repo> {
    #[must_use]
    pub(crate) fn new(stack: Stack<'repo>) -> Self {
        Self {
            stack,
            output: None,
            conflict_mode: ConflictMode::Disallow,
            discard_changes: false,
            use_index_and_worktree: false,
        }
    }

    #[must_use]
    pub(crate) fn allow_conflicts(mut self, allow: bool) -> Self {
        self.conflict_mode = if allow {
            ConflictMode::Allow
        } else {
            ConflictMode::Disallow
        };
        self
    }

    #[must_use]
    pub(crate) fn use_index_and_worktree(mut self, allow: bool) -> Self {
        self.use_index_and_worktree = allow;
        self
    }

    #[must_use]
    pub(crate) fn with_output_stream(mut self, output: termcolor::StandardStream) -> Self {
        self.output = Some(output);
        self
    }

    #[must_use]
    pub(crate) fn transact<F>(self, f: F) -> ExecuteContext<'repo>
    where
        F: FnOnce(&mut StackTransaction) -> Result<(), Error>,
    {
        let Self {
            stack,
            output,
            conflict_mode,
            discard_changes,
            use_index_and_worktree,
        } = self;

        let output = output.expect("with_output_stream() must be called");
        let output = RefCell::new(output);

        let current_tree_id = stack.head.tree_id();
        let applied = stack.applied().to_vec();
        let unapplied = stack.unapplied().to_vec();
        let hidden = stack.hidden().to_vec();

        let mut transaction = StackTransaction {
            stack,
            output,
            conflict_mode,
            discard_changes,
            use_index_and_worktree,
            patch_updates: BTreeMap::new(),
            applied,
            unapplied,
            hidden,
            error: None,
            updated_head: None,
            updated_base: None,
            current_tree_id,
            conflicts: Vec::new(),
            printed_top: false,
        };

        transaction.error = f(&mut transaction).err();

        ExecuteContext(transaction)
    }
}

impl<'repo> ExecuteContext<'repo> {
    pub(crate) fn execute(self, reflog_msg: &str) -> Result<Stack<'repo>, Error> {
        let mut transaction = self.0;

        // Only proceed for halt errors
        match transaction.error {
            None => {}
            Some(Error::TransactionHalt(_)) => {}
            Some(e) => return Err(e),
        }

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

        let set_head = true; // TODO: argument
        let allow_bad_head = false; // TODO: argument
        if set_head {
            let trans_head = transaction.head().clone();

            if transaction.use_index_and_worktree {
                let stack_head = transaction.stack.head.clone();
                let result = transaction.checkout(&trans_head, allow_bad_head);
                if let Err(err) = result {
                    let allow_bad_head = true;
                    transaction.checkout(&stack_head, allow_bad_head)?;
                    return Err(Error::TransactionAborted(err.to_string()));
                }
            }

            let updated_ref = transaction
                .stack
                .branch
                .get_mut()
                .set_target(trans_head.id(), reflog_msg)?;
            transaction.stack.branch = git2::Branch::wrap(updated_ref);
            transaction.stack.head = trans_head;
        }

        let conflict_msg = format!("{} (CONFLICT)", reflog_msg);
        let reflog_msg = if transaction.conflicts.is_empty() {
            reflog_msg
        } else {
            &conflict_msg
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
    pub(crate) fn base(&self) -> &Commit<'repo> {
        if let Some(commit) = self.updated_base.as_ref() {
            commit
        } else {
            &self.stack.base
        }
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
                        if top.is_some() && top == self.stack.applied().last() {
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
        repo.checkout_tree(commit.as_object(), Some(&mut checkout_builder))
            .map_err(|e| {
                if e.class() == git2::ErrorClass::Checkout && e.code() == git2::ErrorCode::Conflict
                {
                    Error::CheckoutConflicts(e.message().to_string())
                } else {
                    Error::Git(e)
                }
            })?;
        self.current_tree_id = commit.tree_id();
        Ok(())
    }

    pub(crate) fn update_patch(
        &mut self,
        patchname: &PatchName,
        commit_id: Oid,
    ) -> Result<(), Error> {
        let commit = self.stack.repo.find_commit(commit_id)?;
        let old_commit = self.get_patch_commit(patchname);
        // Failure to copy is okay. The old commit may not have a note to copy.
        if let Err(e @ Error::GitExecute(_)) = stupid::notes_copy(old_commit.id(), commit_id) {
            return Err(e);
        }
        self.patch_updates
            .insert(patchname.clone(), Some(PatchState { commit }));
        self.print_updated(patchname)?;
        Ok(())
    }

    pub(crate) fn push_new(&mut self, patchname: &PatchName, oid: Oid) -> Result<(), Error> {
        let commit = self.stack.repo.find_commit(oid)?;
        self.applied.push(patchname.clone());
        self.patch_updates
            .insert(patchname.clone(), Some(PatchState { commit }));
        self.print_pushed(patchname, PushStatus::New, true)?;
        Ok(())
    }

    pub(crate) fn push_tree_patches<P>(&mut self, patchnames: &[P]) -> Result<(), Error>
    where
        P: AsRef<PatchName>,
    {
        for (i, patchname) in patchnames.iter().enumerate() {
            let is_last = i + 1 == patchnames.len();
            self.push_tree(patchname.as_ref(), is_last)?;
        }
        Ok(())
    }

    pub(crate) fn push_tree(&mut self, patchname: &PatchName, is_last: bool) -> Result<(), Error> {
        let patch_commit = self.get_patch_commit(patchname);
        let config = self.stack.repo.config()?;
        let parent = patch_commit.parent(0)?;
        let is_empty = parent.tree_id() == patch_commit.tree_id();

        let push_status = if patch_commit.parent_id(0)? != self.top().id() {
            let default_committer = signature::default_committer(Some(&config))?;
            let message = patch_commit.message_ex();
            let parent_ids = [self.top().id()];
            let new_commit_id = self.stack.repo.commit_ex(
                &patch_commit.author(),
                &default_committer,
                &message,
                patch_commit.tree_id(),
                parent_ids,
            )?;

            let commit = self.stack.repo.find_commit(new_commit_id)?;
            if let Err(e @ Error::GitExecute(_)) =
                stupid::notes_copy(patch_commit.id(), new_commit_id)
            {
                return Err(e);
            }
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
            panic!("push_tree `{}` was not in unapplied or hidden", patchname);
        }

        self.applied.push(patchname.clone());

        self.print_pushed(patchname, push_status, is_last)
    }

    pub(crate) fn reorder_patches(
        &mut self,
        applied: Option<&[PatchName]>,
        unapplied: Option<&[PatchName]>,
        hidden: Option<&[PatchName]>,
    ) -> Result<(), Error> {
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

    pub(crate) fn hide_patches(&mut self, to_hide: &[PatchName]) -> Result<(), Error> {
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

    pub(crate) fn unhide_patches(&mut self, to_unhide: &[PatchName]) -> Result<(), Error> {
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

    pub(crate) fn rename_patch(
        &mut self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
    ) -> Result<(), Error> {
        if new_patchname == old_patchname {
            return Ok(());
        } else if self.stack.has_patch(new_patchname)
            && self
                .patch_updates
                .get(new_patchname)
                .map_or(true, |maybe_patch| maybe_patch.is_some())
        {
            return Err(Error::PatchAlreadyExists(new_patchname.clone()));
        } else if !self.stack.has_patch(old_patchname) {
            return Err(Error::PatchDoesNotExist(old_patchname.clone()));
        }

        if let Some(pos) = self.applied.iter().position(|pn| pn == old_patchname) {
            self.applied[pos] = new_patchname.clone();
        } else if let Some(pos) = self.unapplied.iter().position(|pn| pn == old_patchname) {
            self.unapplied[pos] = new_patchname.clone();
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == old_patchname) {
            self.hidden[pos] = new_patchname.clone();
        } else {
            panic!(
                "old patchname `{}` not found in applied, unapplied, or hidden",
                old_patchname
            );
        }

        let patch = self.stack.get_patch(old_patchname).clone();
        self.patch_updates.insert(old_patchname.clone(), None);
        self.patch_updates
            .insert(new_patchname.clone(), Some(patch));

        self.print_rename(old_patchname, new_patchname)
    }

    pub(crate) fn delete_patches<F>(&mut self, should_delete: F) -> Result<Vec<PatchName>, Error>
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

    pub(crate) fn pop_patches<F>(&mut self, should_pop: F) -> Result<Vec<PatchName>, Error>
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

    pub(crate) fn push_patches<P>(
        &mut self,
        patchnames: &[P],
        check_merged: bool,
    ) -> Result<(), Error>
    where
        P: AsRef<PatchName>,
    {
        let default_index = self.stack.repo.index()?;
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
                    &default_index,
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
        default_index: &git2::Index,
        temp_index: &mut git2::Index,
        temp_index_tree_id: &mut Option<git2::Oid>,
    ) -> Result<(), Error> {
        let repo = self.stack.repo;
        let config = repo.config()?;
        let default_committer = signature::default_committer(Some(&config))?;
        let patch_commit = self.get_patch_commit(patchname).clone();
        let old_parent = patch_commit.parent(0)?;
        let new_parent = self.top().clone();

        let mut merge_conflict = false;
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

            let temp_index_path = temp_index.path().unwrap();
            if temp_index_tree_id != &Some(ours) {
                stupid::read_tree(ours, temp_index_path)?;
                *temp_index_tree_id = Some(ours);
            }

            let maybe_tree_id = if stupid::apply_treediff_to_index(
                base,
                theirs,
                repo.workdir().unwrap(),
                temp_index_path,
            )? {
                stupid::write_tree(temp_index_path).ok()
            } else {
                None
            };

            if let Some(tree_id) = maybe_tree_id {
                tree_id
            } else if !self.use_index_and_worktree {
                return Err(Error::TransactionHalt(format!(
                    "{} does not apply cleanly",
                    patchname
                )));
            } else {
                if stupid::read_tree_checkout(self.current_tree_id, ours).is_err() {
                    return Err(Error::TransactionHalt("index/worktree dirty".to_string()));
                }
                self.current_tree_id = ours;

                let default_index_path = default_index.path().unwrap();
                let use_mergetool = config.get_bool("stgit.autoimerge").unwrap_or(false);
                match stupid::merge_recursive_or_mergetool(
                    base,
                    ours,
                    theirs,
                    repo.workdir().unwrap(),
                    default_index_path,
                    use_mergetool,
                ) {
                    Ok(conflicts) if conflicts.is_empty() => {
                        // Success, no conflicts
                        let tree_id = stupid::write_tree(default_index_path)
                            .map_err(|_| Error::TransactionHalt("conflicting merge".to_string()))?;
                        self.current_tree_id = tree_id;
                        push_status = PushStatus::Modified;
                        tree_id
                    }
                    Ok(mut conflicts) => {
                        self.conflicts.append(&mut conflicts);
                        merge_conflict = true;
                        push_status = PushStatus::Conflict;
                        ours
                    }
                    Err(_) => {
                        return Err(Error::TransactionHalt("index/worktree dirty".to_string()))
                    }
                }
            }
        };

        if new_tree_id != patch_commit.tree_id() || new_parent.id() != old_parent.id() {
            let commit_id = repo.commit_ex(
                &patch_commit.author(),
                &default_committer,
                &patch_commit.message_ex(),
                new_tree_id,
                [new_parent.id()],
            )?;
            let commit = repo.find_commit(commit_id)?;
            if let Err(e @ Error::GitExecute(_)) = stupid::notes_copy(patch_commit.id(), commit_id)
            {
                return Err(e);
            };
            if merge_conflict {
                // In the case of a conflict, update() will be called after the
                // execute() performs the checkout. Setting the transaction head
                // here ensures that the real stack top will be checked-out.
                self.updated_head = Some(commit.clone());
            }

            if new_tree_id == new_parent.tree_id() {
                push_status = PushStatus::Empty;
            }

            self.patch_updates
                .insert(patchname.clone(), Some(PatchState { commit }));
        }

        if merge_conflict {
            // The final checkout at execute-time must allow these push conflicts.
            self.conflict_mode = ConflictMode::Allow;
        }

        if let Some(pos) = self.unapplied.iter().position(|pn| pn == patchname) {
            self.unapplied.remove(pos);
        } else if let Some(pos) = self.hidden.iter().position(|pn| pn == patchname) {
            self.hidden.remove(pos);
        }
        self.applied.push(patchname.clone());

        self.print_pushed(patchname, push_status, is_last)?;

        if merge_conflict {
            Err(Error::TransactionHalt(format!(
                "{} merge conflicts",
                self.conflicts.len()
            )))
        } else {
            Ok(())
        }
    }

    fn check_merged<'a, P>(
        &self,
        patchnames: &'a [P],
        temp_index: &mut git2::Index,
        temp_index_tree_id: &mut Option<git2::Oid>,
    ) -> Result<Vec<&'a PatchName>, Error>
    where
        P: AsRef<PatchName>,
    {
        let repo = self.stack.repo;
        let mut merged: Vec<&PatchName> = vec![];
        let temp_index_path = temp_index.path().unwrap();

        if temp_index_tree_id != &Some(self.stack.head.tree_id()) {
            stupid::read_tree(self.stack.head.tree_id(), temp_index_path)?;
            *temp_index_tree_id = Some(self.stack.head.tree_id());
        }

        for patchname in patchnames.iter().rev() {
            let patchname = patchname.as_ref();
            let patch_commit = self.get_patch_commit(patchname);
            let parent_commit = patch_commit.parent(0)?;

            if patch_commit.parent_count() == 1 && patch_commit.tree_id() == parent_commit.tree_id()
            {
                continue; // No change
            }

            if stupid::apply_treediff_to_index(
                patch_commit.tree_id(),
                parent_commit.tree_id(),
                repo.workdir().unwrap(),
                temp_index_path,
            )? {
                merged.push(patchname);
                *temp_index_tree_id = None;
            }
        }

        self.print_merged(&merged)?;

        Ok(merged)
    }

    fn print_merged(&self, merged_patches: &[&PatchName]) -> Result<(), Error> {
        let mut output = self.output.borrow_mut();
        write!(output, "Found ")?;
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, "{}", merged_patches.len())?;
        output.reset()?;
        let plural = if merged_patches.len() == 1 { "" } else { "es" };
        writeln!(output, " patch{} merged upstream", plural)?;
        Ok(())
    }

    fn print_rename(
        &self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
    ) -> Result<(), Error> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_dimmed(true))?;
        write!(output, "{}", old_patchname)?;
        color_spec.clear();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, " => ")?;
        output.reset()?;
        writeln!(output, "{}", new_patchname)?;
        Ok(())
    }

    fn print_deleted(&self, deleted: &[PatchName]) -> Result<(), Error> {
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
                write!(output, "{}", last)?;
            }
            output.reset()?;
            writeln!(output)?;
        }
        Ok(())
    }

    fn print_hidden(&self, hidden: &[PatchName]) -> Result<(), Error> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in hidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Red)))?;
            write!(output, "! ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true).set_italic(true))?;
            writeln!(output, "{}", patchname)?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    fn print_unhidden(&self, unhidden: &[PatchName]) -> Result<(), Error> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in unhidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(output, "- ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            writeln!(output, "{}", patchname)?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    fn print_popped(&self, popped: &[PatchName]) -> Result<(), Error> {
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
                write!(output, "{}", last)?;
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
    ) -> Result<(), Error> {
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
        write!(output, "{} ", sigil)?;
        color_spec.clear();
        output.set_color(color_spec.set_bold(is_last).set_intense(!is_last))?;
        write!(output, "{}", patchname)?;
        output.reset()?;

        let status_str = match status {
            PushStatus::New => " (new)",
            PushStatus::AlreadyMerged => " (merged)",
            PushStatus::Conflict => " (conflict)",
            PushStatus::Empty => " (empty)",
            PushStatus::Modified => " (modified)",
            PushStatus::Unmodified => "",
        };

        writeln!(output, "{}", status_str)?;
        if is_last {
            self.printed_top = true;
        }
        Ok(())
    }

    fn print_updated(&self, patchname: &PatchName) -> Result<(), Error> {
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
        writeln!(output, "{}", patchname)?;
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
}
