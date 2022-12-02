// SPDX-License-Identifier: GPL-2.0-only

//! High-level StGit stack representation.

use std::{collections::BTreeMap, str::FromStr};

use anyhow::{anyhow, Result};

use super::{
    state::StackState, transaction::TransactionBuilder, upgrade::stack_upgrade, PatchState,
    StackStateAccess,
};
use crate::{patchname::PatchName, repo::RepositoryExtended, stupid::Stupid};

/// StGit stack
///
/// This struct contains the underlying stack state as recorded in the git repo along
/// with other relevant branch state.
pub(crate) struct Stack<'repo> {
    pub(crate) repo: &'repo git2::Repository,
    pub(crate) branch_name: String,
    pub(crate) branch: git2::Branch<'repo>,
    pub(crate) branch_head: git2::Commit<'repo>,
    pub(crate) refname: String,
    base: git2::Commit<'repo>,
    state: StackState<'repo>,
}

impl<'repo> Stack<'repo> {
    /// Initialize a new StGit stack on an existing Git branch.
    ///
    /// The branch name is optional and defaults to the current branch.
    pub(crate) fn initialize(
        repo: &'repo git2::Repository,
        branch_name: Option<&str>,
    ) -> Result<Self> {
        let branch = repo.get_branch(branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let branch_head = branch.get().peel_to_commit()?;
        let base = branch_head.clone();
        let refname = state_refname_from_branch_name(&branch_name);

        stack_upgrade(repo, &branch_name)?;

        if repo.find_reference(&refname).is_ok() {
            return Err(anyhow!(
                "StGit stack already initialized for branch `{branch_name}`"
            ));
        }
        let state = StackState::new(branch_head.clone());
        state.commit(repo, Some(&refname), "initialize")?;
        ensure_patch_refs(repo, &branch_name, &state)?;

        Ok(Self {
            repo,
            branch_name,
            branch,
            branch_head,
            refname,
            base,
            state,
        })
    }

    /// Remove StGit stack state from the repository.
    ///
    /// This removes the reference to the stack state, i.e. `refs/stacks/<name>`, and
    /// references to the stacks patches found in `refs/patches/<name>/`. StGit specific
    /// configuration associated with the stack is also removed from the config.
    ///
    /// N.B. stack and patch commits that become unreferenced are subject to git's
    /// normal periodic garbage collection.
    pub(crate) fn deinitialize(self) -> Result<()> {
        let Self {
            repo,
            branch_name,
            refname,
            ..
        } = self;
        let mut state_ref = repo.find_reference(&refname)?;
        let patch_ref_glob = get_patch_refname(&branch_name, "*");
        for patch_reference in repo.references_glob(&patch_ref_glob)? {
            let mut patch_reference = patch_reference?;
            patch_reference.delete()?;
        }
        state_ref.delete()?;

        // It is ok if the StGit-specific config section does not exist.
        repo.stupid()
            .config_remove_section(&format!("branch.{branch_name}.stgit"))
            .ok();

        Ok(())
    }

    /// Get a stack from an existing branch.
    ///
    /// The current branch is used if the optional branch name is not provided.
    ///
    /// An error will be returned if there is no StGit stack associated with the branch.
    pub(crate) fn from_branch(
        repo: &'repo git2::Repository,
        branch_name: Option<&str>,
    ) -> Result<Self> {
        let branch = repo.get_branch(branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let branch_head = branch.get().peel_to_commit()?;

        stack_upgrade(repo, &branch_name)?;

        let refname = state_refname_from_branch_name(&branch_name);
        let state_ref = repo
            .find_reference(&refname)
            .map_err(|_| anyhow!("StGit stack not initialized for branch `{branch_name}`"))?;
        let stack_tree = state_ref.peel_to_tree()?;
        let state = StackState::from_tree(repo, &stack_tree)?;
        let base = if let Some(first_patchname) = state.applied.first() {
            state.patches[first_patchname].commit.parent(0)?
        } else {
            branch_head.clone()
        };
        ensure_patch_refs(repo, &branch_name, &state)?;
        Ok(Self {
            repo,
            branch_name,
            branch,
            branch_head,
            refname,
            base,
            state,
        })
    }

    /// Check whether the stack is marked as protected in the config.
    pub(crate) fn is_protected(&self, config: &git2::Config) -> bool {
        let name = &self.branch_name;
        let key = format!("branch.{name}.stgit.protect");
        config.get_bool(&key).unwrap_or(false)
    }

    /// Set the stack's protected state in the config.
    pub(crate) fn set_protected(&self, config: &mut git2::Config, protect: bool) -> Result<()> {
        let name = &self.branch_name;
        let key = format!("branch.{name}.stgit.protect");
        if protect {
            config.set_bool(&key, true)?;
            Ok(())
        } else {
            match config.remove(&key) {
                Ok(()) => Ok(()),
                Err(e)
                    if e.class() == git2::ErrorClass::Config
                        && e.code() == git2::ErrorCode::NotFound =>
                {
                    Ok(())
                }
                Err(e) => Err(e.into()),
            }
        }
    }

    /// Check whether the stack's recorded head matches the branch's head.
    pub(crate) fn is_head_top(&self) -> bool {
        self.state.head.id() == self.branch_head.id()
    }

    /// Return an error if the stack's recorded head differs from the branch's head.
    pub(crate) fn check_head_top_mismatch(&self) -> Result<()> {
        if self.state.applied.is_empty() || self.is_head_top() {
            Ok(())
        } else {
            Err(anyhow!(
                "HEAD and stack top are not the same. \
                 This can happen if you modify the branch with git. \
                 See `stg repair --help` for next steps to take."
            ))
        }
    }

    /// Re-commit stack state with updated branch head.
    pub(crate) fn log_external_mods(self, message: Option<&str>) -> Result<Self> {
        let state_ref = self.repo.find_reference(&self.refname)?;
        let prev_state_commit = state_ref.peel_to_commit()?;
        let prev_state_commit_id = prev_state_commit.id();
        let state = self
            .state
            .advance_head(self.branch_head.clone(), prev_state_commit);

        let message = message.unwrap_or(
            "external modifications\n\
             \n\
             Modifications by tools other than StGit (e.g. git).\n",
        );
        let reflog_msg = "external modifications";

        let state_commit_id = state.commit(self.repo, None, message)?;
        self.repo.reference_matching(
            &self.refname,
            state_commit_id,
            true,
            prev_state_commit_id,
            reflog_msg,
        )?;

        Ok(Self { state, ..self })
    }

    /// Start a transaction to modify the stack.
    pub(crate) fn setup_transaction(self) -> TransactionBuilder<'repo> {
        TransactionBuilder::new(self)
    }

    /// Clear the stack state history.
    pub(crate) fn clear_state_log(&mut self, reflog_msg: &str) -> Result<()> {
        self.state.prev = None;
        self.state
            .commit(self.repo, Some(&self.refname), reflog_msg)?;
        Ok(())
    }

    /// Update the branch and branch head commit.
    pub(super) fn update_head(&mut self, branch: git2::Branch<'repo>, commit: git2::Commit<'repo>) {
        self.branch = branch;
        self.branch_head = commit;
    }

    /// Get mutable reference to the stack state.
    pub(super) fn state_mut(&mut self) -> &mut StackState<'repo> {
        &mut self.state
    }

    /// Get reference name for a patch.
    pub(super) fn patch_refname(&self, patchname: &PatchName) -> String {
        self.patch_revspec(patchname.as_ref())
    }

    /// Get revision specification relative to this stack's patch reference root.
    ///
    /// I.e. `refs/patches/<branch>/<patch_spec>`.
    pub(crate) fn patch_revspec(&self, patch_spec: &str) -> String {
        get_patch_refname(&self.branch_name, patch_spec)
    }
}

impl<'repo> StackStateAccess<'repo> for Stack<'repo> {
    fn applied(&self) -> &[PatchName] {
        self.state.applied()
    }

    fn unapplied(&self) -> &[PatchName] {
        self.state.unapplied()
    }

    fn hidden(&self) -> &[PatchName] {
        self.state.hidden()
    }

    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo> {
        self.state.get_patch(patchname)
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        self.state.has_patch(patchname)
    }

    fn top(&self) -> &git2::Commit<'repo> {
        self.state.top()
    }

    fn head(&self) -> &git2::Commit<'repo> {
        self.state.head()
    }

    fn base(&self) -> &git2::Commit<'repo> {
        &self.base
    }
}

/// Get reference name for StGit stack state for the given branch name.
pub(crate) fn state_refname_from_branch_name(branch_name: &str) -> String {
    format!("refs/stacks/{branch_name}")
}

/// Get reference name for a patch in the given branch.
fn get_patch_refname(branch_name: &str, patch_spec: &str) -> String {
    format!("refs/patches/{branch_name}/{patch_spec}")
}

/// Get name of branch with custom error mapping for non-UTF8 branch names.
pub(crate) fn get_branch_name(branch: &git2::Branch<'_>) -> Result<String> {
    let name_bytes = branch.name_bytes()?;
    Ok(std::str::from_utf8(name_bytes)
        .map_err(|_| {
            anyhow!(
                "Branch name `{}` is not valid UTF-8",
                String::from_utf8_lossy(name_bytes)
            )
        })?
        .to_string())
}

/// Fix-up stack's patch references.
///
/// Ensures that each patch in the stack has a valid patch reference and that there are
/// no references for non-existing patches in this stack's patch ref namespace.
///
/// This is done when instantiating a [`Stack`] to guard against external modifications
/// to the stack's patch refs.
fn ensure_patch_refs(repo: &git2::Repository, branch_name: &str, state: &StackState) -> Result<()> {
    let patch_ref_prefix = get_patch_refname(branch_name, "");
    let patch_ref_glob = get_patch_refname(branch_name, "*");
    let mut state_patches: BTreeMap<&PatchName, &PatchState> = state.patches.iter().collect();

    for existing_ref in repo.references_glob(&patch_ref_glob)? {
        let mut existing_ref = existing_ref?;
        if let Some(existing_refname) = existing_ref.name() {
            let existing_patchname = existing_refname.strip_prefix(&patch_ref_prefix).unwrap();
            if let Ok(existing_patchname) = PatchName::from_str(existing_patchname) {
                if let Some(patchdesc) = state_patches.remove(&existing_patchname) {
                    if let Some(existing_id) = existing_ref.target() {
                        if existing_id == patchdesc.commit.id() {
                            // Patch ref is good. Do nothing.
                        } else {
                            existing_ref
                                .set_target(patchdesc.commit.id(), "fixup broken patch ref")?;
                        }
                    } else {
                        // Existing ref seems to be symbolic, and not direct.
                        repo.reference(
                            existing_refname,
                            patchdesc.commit.id(),
                            true,
                            "fixup sybolic patch ref",
                        )?;
                    }
                } else {
                    // Existing ref does not map to known/current patch.
                    existing_ref.delete()?;
                }
            } else {
                // Existing ref does not have a valid patch name.
                existing_ref.delete()?;
            }
        } else {
            // The existing ref name is not valid UTF-8, so is not a valid patch ref.
            existing_ref.delete()?;
        }
    }

    // At this point state_patches only contains patches that did not overlap with the
    // existing patch refs found in the repository.
    for (patchname, patchdesc) in state_patches {
        repo.reference(
            &get_patch_refname(branch_name, patchname.as_ref()),
            patchdesc.commit.id(),
            false,
            "fixup missing patch ref",
        )?;
    }

    Ok(())
}
