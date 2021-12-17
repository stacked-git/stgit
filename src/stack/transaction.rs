use std::collections::BTreeMap;
use std::ffi::OsString;
use std::io::Write;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

use git2::{Commit, Oid, RepositoryState};
use indexmap::IndexSet;
use termcolor::WriteColor;

use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;
use crate::stack::{PatchDescriptor, Stack};
use crate::wrap::repository::{commit_ex, with_temp_index};
use crate::wrap::signature;
use crate::wrap::CommitData;

use super::iter::AllPatches;

pub(crate) struct StackTransaction<'repo> {
    stack: Stack<'repo>,
    conflict_mode: ConflictMode,
    discard_changes: bool,
    use_index_and_worktree: bool,
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

enum PushStatus {
    AlreadyMerged,
    Conflict,
    Empty,
    Modified,
    Unmodified,
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
        if set_head {
            if transaction.use_index_and_worktree {
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
        use_index_and_worktree: bool,
    ) -> TransactionContext {
        let current_tree_id = stack.head.tree_id();
        let applied = stack.state.applied.clone();
        let unapplied = stack.state.unapplied.clone();
        let hidden = stack.state.hidden.clone();
        TransactionContext(Self {
            stack,
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

    #[allow(dead_code)]
    pub(crate) fn applied(&self) -> &[PatchName] {
        &self.applied
    }

    pub(crate) fn unapplied(&self) -> &[PatchName] {
        &self.unapplied
    }

    #[allow(dead_code)]
    pub(crate) fn hidden(&self) -> &[PatchName] {
        &self.hidden
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

    pub(crate) fn push_tree(
        &mut self,
        patchname: &PatchName,
        is_last: bool,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
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
        let config = self.stack.repo.config()?;
        let parent = patch_commit.parent(0)?;
        let is_empty = parent.tree_id() == patch_commit.tree_id();

        let push_status = if patch_commit.parent_id(0)? != self.top().id() {
            let default_committer = signature::default_committer(Some(&config))?;
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

            let commit = self.stack.repo.find_commit(new_commit_id)?;
            self.patch_updates
                .insert(patchname.clone(), Some(PatchDescriptor { commit }));

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

        self.print_pushed(patchname, push_status, is_last, stdout)
    }

    pub(crate) fn reorder_patches(
        &mut self,
        applied: Option<&[PatchName]>,
        unapplied: Option<&[PatchName]>,
        hidden: Option<&[PatchName]>,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        if let Some(applied) = applied {
            let num_common = self
                .applied
                .iter()
                .zip(applied)
                .take_while(|(old, new)| old == new)
                .count();

            let to_pop: IndexSet<PatchName> = self.applied[num_common..].iter().cloned().collect();
            self.pop_patches(|pn| to_pop.contains(pn), stdout)?;

            let to_push = &applied[num_common..];
            for (i, pn) in to_push.iter().enumerate() {
                let already_merged = false;
                let is_last = i + 1 == to_push.len();
                self.push_patch(pn, already_merged, is_last, stdout)?;
            }

            assert_eq!(self.applied, applied);

            if to_push.is_empty() {
                if let Some(last) = applied.last() {
                    self.print_pushed(last, PushStatus::Unmodified, true, stdout)?;
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

    fn print_rename(
        &self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        let mut color_spec = termcolor::ColorSpec::new();
        stdout.set_color(color_spec.set_dimmed(true))?;
        write!(stdout, "{}", old_patchname)?;
        color_spec.clear();
        stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(stdout, " => ")?;
        stdout.reset()?;
        writeln!(stdout, "{}", new_patchname)?;
        Ok(())
    }

    fn print_deleted(
        &self,
        deleted: &PatchName,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        let mut color_spec = termcolor::ColorSpec::new();
        stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
        write!(stdout, "* ")?;
        color_spec.set_fg(None);
        stdout.set_color(color_spec.set_dimmed(true))?;
        writeln!(stdout, "{}", deleted)?;
        stdout.reset()?;
        Ok(())
    }

    fn print_hidden(
        &self,
        hidden: &[PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in hidden {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Red)))?;
            write!(stdout, "! ")?;
            color_spec.set_fg(None);
            stdout.set_color(color_spec.set_dimmed(true).set_italic(true))?;
            writeln!(stdout, "{}", patchname)?;
            color_spec.clear();
            stdout.reset()?;
        }
        Ok(())
    }

    fn print_unhidden(
        &self,
        unhidden: &[PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in unhidden {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(stdout, "- ")?;
            color_spec.set_fg(None);
            stdout.set_color(color_spec.set_dimmed(true))?;
            writeln!(stdout, "{}", patchname)?;
            color_spec.clear();
            stdout.reset()?;
        }
        Ok(())
    }

    fn print_popped(
        &self,
        popped: &[PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        if !popped.is_empty() {
            let mut color_spec = termcolor::ColorSpec::new();
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(stdout, "- ")?;
            color_spec.set_fg(None);
            stdout.set_color(color_spec.set_dimmed(true))?;
            write!(stdout, "{}", popped[0])?;
            if popped.len() > 1 {
                stdout.set_color(color_spec.set_dimmed(false))?;
                write!(stdout, "..")?;
                stdout.set_color(color_spec.set_dimmed(true))?;
                let last = &popped[popped.len() - 1];
                write!(stdout, "{}", last)?;
            }
            stdout.reset()?;
            writeln!(stdout)?;
        }
        Ok(())
    }

    fn print_pushed(
        &self,
        patchname: &PatchName,
        status: PushStatus,
        is_last: bool,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        let sigil = if is_last { '>' } else { '+' };
        let mut color_spec = termcolor::ColorSpec::new();
        stdout.set_color(
            color_spec.set_fg(Some(if let PushStatus::Conflict = status {
                termcolor::Color::Red
            } else if is_last {
                termcolor::Color::Blue
            } else {
                termcolor::Color::Green
            })),
        )?;
        write!(stdout, "{} ", sigil)?;
        color_spec.clear();
        stdout.set_color(color_spec.set_bold(is_last).set_intense(!is_last))?;
        write!(stdout, "{}", patchname)?;
        stdout.reset()?;

        let status_str = match status {
            PushStatus::AlreadyMerged => " (merged)",
            PushStatus::Conflict => " (conflict)",
            PushStatus::Empty => " (empty)",
            PushStatus::Modified => " (modified)",
            PushStatus::Unmodified => "",
        };

        writeln!(stdout, "{}", status_str)?;
        Ok(())
    }

    pub(crate) fn hide_patches(
        &mut self,
        to_hide: &[PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
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

        self.reorder_patches(Some(&applied), Some(&unapplied), Some(&hidden), stdout)?;

        self.print_hidden(to_hide, stdout)
    }

    pub(crate) fn unhide_patches(
        &mut self,
        to_unhide: &[PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
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

        self.reorder_patches(None, Some(&unapplied), Some(&hidden), stdout)?;

        self.print_unhidden(to_unhide, stdout)
    }

    pub(crate) fn rename_patch(
        &mut self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<(), Error> {
        if self.stack.state.patches.contains_key(new_patchname)
            && self
                .patch_updates
                .get(new_patchname)
                .map_or(true, |maybe_desc| maybe_desc.is_some())
        {
            return Err(Error::PatchAlreadyExists(new_patchname.clone()));
        } else if !self.stack.state.patches.contains_key(old_patchname) {
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

        let patch_desc = self.stack.state.patches[old_patchname].clone();
        self.patch_updates.insert(old_patchname.clone(), None);
        self.patch_updates
            .insert(new_patchname.clone(), Some(patch_desc));

        self.print_rename(old_patchname, new_patchname, stdout)
    }

    pub(crate) fn delete_patches<F>(
        &mut self,
        should_delete: F,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<Vec<PatchName>, Error>
    where
        F: Fn(&PatchName) -> bool,
    {
        let all_popped = if let Some(first_pop_pos) = self.applied.iter().position(&should_delete) {
            self.applied.split_off(first_pop_pos)
        } else {
            vec![]
        };

        self.print_popped(&all_popped, stdout)?;

        let incidental: Vec<PatchName> = all_popped
            .iter()
            .filter(|pn| !should_delete(pn))
            .cloned()
            .collect();

        for patchname in all_popped.iter().filter(|pn| should_delete(pn)) {
            self.print_deleted(patchname, stdout)?;
            self.patch_updates.insert(patchname.clone(), None);
        }

        let unapplied_size = incidental.len() + self.unapplied.len();
        let unapplied = std::mem::replace(&mut self.unapplied, Vec::with_capacity(unapplied_size));
        self.unapplied.append(&mut incidental.clone());

        for patchname in unapplied {
            if should_delete(&patchname) {
                self.print_deleted(&patchname, stdout)?;
                self.patch_updates.insert(patchname, None);
            } else {
                self.unapplied.push(patchname);
            }
        }

        let mut i = 0;
        while i < self.hidden.len() {
            if should_delete(&self.hidden[i]) {
                let patchname = self.hidden.remove(i);
                self.print_deleted(&patchname, stdout)?;
                self.patch_updates.insert(patchname, None);
            } else {
                i += 1;
            }
        }

        Ok(incidental)
    }

    pub(crate) fn pop_patches<F>(
        &mut self,
        should_pop: F,
        stdout: &mut termcolor::StandardStream,
    ) -> Result<Vec<PatchName>, Error>
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

        self.print_popped(&all_popped, stdout)?;

        Ok(incidental)
    }

    pub(crate) fn push_patch(
        &mut self,
        patchname: &PatchName,
        already_merged: bool,
        is_last: bool,
        stdout: &mut termcolor::StandardStream,
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

        let mut push_status = if already_merged {
            PushStatus::AlreadyMerged
        } else {
            PushStatus::Unmodified
        };
        let mut merge_conflict = false;

        cd.tree_id = if already_merged {
            old_base_tree.id()
        } else {
            let base = old_base_tree;
            let ours = new_parent.tree()?;
            let theirs = patch_commit.tree()?;
            let mut merge_options = git2::MergeOptions::new();
            merge_options.patience(true);
            let mut temp_index = repo.merge_trees(&base, &ours, &theirs, Some(&merge_options))?;
            if temp_index.has_conflicts() {
                if !self.use_index_and_worktree {
                    return Err(Error::PatchApplicationUnclean(patchname.to_string()));
                }

                // TODO stgit.autoimerge
                // TODO compat: python version runs git merge-recursive in the worktree
                // It seems like the old temp index merge did not use `git apply
                // --3way`, which meant that it would fail in cases where the new
                // temp-index merge succeeds.  Not sure if there are cases where old
                // iw.merge() would succeed where new repo.merge_trees() (above) would
                // fail?
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
                push_status = PushStatus::Conflict;
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

        // TODO: want the "+ patch (modified)" status to indicate whether the patch's *diff*
        // from its new parent is different than from its old parent.
        let is_tree_modified = cd.tree_id != patch_commit.tree_id();

        if is_tree_modified {
            push_status = PushStatus::Modified;
        }

        if is_tree_modified || cd.parent_ids[0] != patch_commit.parent_id(0).unwrap() {
            let new_patch_commit_id = cd.commit(repo)?;
            let commit = repo.find_commit(new_patch_commit_id)?;
            if merge_conflict {
                // In the case of a conflict, update() will be called after the
                // execute() performs the checkout. Setting the transaction head
                // here ensures that the real stack top will be checked-out.
                self.updated_head = Some(commit.clone());
            }

            if commit
                .parent(0)
                .map(|parent| parent.tree_id() == commit.tree_id())
                .unwrap_or(false)
            {
                push_status = PushStatus::Empty;
            }

            self.patch_updates
                .insert(patchname.clone(), Some(PatchDescriptor { commit }));
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

        self.print_pushed(patchname, push_status, is_last, stdout)?;

        if merge_conflict {
            Err(Error::MergeConflicts(self.conflicts.len()))
        } else {
            Ok(())
        }
    }

    pub(crate) fn check_merged<'a>(
        &self,
        patches: &'a [PatchName],
        stdout: &mut termcolor::StandardStream,
    ) -> Result<Vec<&'a PatchName>, Error> {
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
                if let Ok(index) = repo.apply_to_tree(&head_tree, &diff, None) {
                    if !index.has_conflicts() {
                        merged.push(patchname);
                    }
                }
            }

            Ok(())
        })?;

        {
            write!(stdout, "Found ")?;
            let mut color_spec = termcolor::ColorSpec::new();
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
            write!(stdout, "{}", merged.len())?;
            stdout.reset()?;
            let plural = if merged.len() == 1 { "" } else { "es" };
            writeln!(stdout, " patch{} merged upstream", plural)?;
        }

        Ok(merged)
    }
}
