// SPDX-License-Identifier: GPL-2.0-only

//! High-level StGit stack representation.

use std::{collections::BTreeMap, rc::Rc, str::FromStr};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;

use super::{
    state::StackState, transaction::TransactionBuilder, upgrade::stack_upgrade, PatchState,
    StackAccess, StackStateAccess,
};
use crate::{
    branchloc::BranchLocator,
    ext::RepositoryExtended,
    patch::PatchName,
    wrap::{Branch, PartialRefName},
};

/// StGit stack
///
/// This struct contains the underlying stack state as recorded in the git repo along
/// with other relevant branch state.
pub(crate) struct Stack<'repo> {
    pub(crate) repo: &'repo gix::Repository,
    branch_name: String,
    branch: Branch<'repo>,
    branch_head: Rc<gix::Commit<'repo>>,
    stack_refname: String,
    base: Rc<gix::Commit<'repo>>,
    state: StackState<'repo>,
    is_initialized: bool,
}

/// Policy for stack initialization when opening/discovering a stack for a branch.
pub(crate) enum InitializationPolicy {
    /// The stack will be initialized if it is not yet initialized.
    AutoInitialize,

    /// The stack must be initialized and thus must *not* already be initialized.
    MustInitialize,

    /// The stack will be re-initialized even if the branch has an existing stack
    /// metadata reference.
    ForceInitialize,

    /// The stack must already be initialized.
    RequireInitialized,

    /// An uninitialized stack is allowed in which case an empty [`Stack`] will be
    /// provided.
    ///
    /// Stack transactions are prohibited on such [`Stack`] instances.
    AllowUninitialized,
}

impl<'repo> Stack<'repo> {
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
            stack_refname,
            ..
        } = self;
        let state_ref = repo.find_reference(&stack_refname)?;
        let patch_ref_prefix = get_patch_refname(&branch_name, "");
        for patch_reference in
            repo.references()?
                .all()?
                .filter_map(Result::ok)
                .filter(|reference| {
                    reference
                        .name()
                        .as_bstr()
                        .starts_with(patch_ref_prefix.as_bytes())
                })
        {
            patch_reference.delete()?;
        }
        state_ref.delete()?;

        // It is ok if the StGit-specific config section does not exist.
        let mut local_config_file = repo.local_config_file()?;
        let section = format!("{branch_name}.stgit");
        if local_config_file
            .remove_section("branch", Some(section.as_bytes().into()))
            .is_some()
        {
            repo.write_local_config(local_config_file)
                .context("writing local config")?;
        }

        Ok(())
    }

    pub(crate) fn current(
        repo: &'repo gix::Repository,
        init_policy: InitializationPolicy,
    ) -> Result<Self> {
        let branch = repo.get_current_branch()?;
        Stack::from_branch(repo, branch, init_policy)
    }

    pub(crate) fn from_branch_name(
        repo: &'repo gix::Repository,
        branch_name: &PartialRefName,
        init_policy: InitializationPolicy,
    ) -> Result<Self> {
        let branch = repo.get_branch(branch_name)?;
        Stack::from_branch(repo, branch, init_policy)
    }

    pub(crate) fn from_branch_locator(
        repo: &'repo gix::Repository,
        branch_loc: Option<&BranchLocator>,
        init_policy: InitializationPolicy,
    ) -> Result<Self> {
        let branch = if let Some(loc) = branch_loc {
            loc.resolve(repo)?
        } else {
            repo.get_current_branch()?
        };
        Stack::from_branch(repo, branch, init_policy)
    }

    /// Get a stack from an existing branch.
    ///
    /// The current branch is used if the optional branch name is not provided.
    ///
    /// An error will be returned if there is no StGit stack associated with the branch.
    pub(crate) fn from_branch(
        repo: &'repo gix::Repository,
        branch: Branch<'repo>,
        init_policy: InitializationPolicy,
    ) -> Result<Self> {
        let branch_name = branch.get_branch_name()?.to_string();
        let branch_head = Rc::new(branch.get_commit()?);
        let stack_refname = state_refname_from_branch_name(&branch_name);
        let is_initialized;

        stack_upgrade(repo, &branch_name)?;

        let maybe_state_ref = repo.find_reference(&stack_refname).ok();

        let state_and_base_from_ref =
            |state_ref: gix::Reference<'repo>| -> Result<(StackState<'repo>, Rc<gix::Commit<'repo>>)> {
                let stack_tree = state_ref.id().object()?.try_into_commit()?.tree()?;
                let state = StackState::from_tree(repo, stack_tree)?;
                let base = if let Some(first_patchname) = state.applied.first() {
                    Rc::new(
                        repo.find_object(
                            state.patches[first_patchname]
                                .commit
                                .parent_ids()
                                .next()
                                .unwrap(),
                        )?
                        .try_into_commit()?,
                    )
                } else {
                    branch_head.clone()
                };
                Ok((state, base))
            };

        let initialize_state_and_base =
            || -> Result<(StackState<'repo>, Rc<gix::Commit<'repo>>)> {
                let state = StackState::new(branch_head.clone());
                let base = branch_head.clone();
                state.commit(repo, Some(&stack_refname), "initialize")?;
                Ok((state, base))
            };

        let (state, base) = match init_policy {
            InitializationPolicy::AutoInitialize => {
                is_initialized = true;
                if let Some(state_ref) = maybe_state_ref {
                    state_and_base_from_ref(state_ref)?
                } else {
                    initialize_state_and_base()?
                }
            }
            InitializationPolicy::MustInitialize => {
                if maybe_state_ref.is_some() {
                    return Err(anyhow!(
                        "StGit stack already initialized for branch `{branch_name}`"
                    ));
                }
                is_initialized = true;
                initialize_state_and_base()?
            }
            InitializationPolicy::ForceInitialize => {
                is_initialized = true;
                initialize_state_and_base()?
            }
            InitializationPolicy::RequireInitialized => {
                let state_ref = maybe_state_ref.ok_or_else(|| {
                    anyhow!("StGit stack not initialized for branch `{branch_name}`")
                })?;
                is_initialized = true;
                state_and_base_from_ref(state_ref)?
            }
            InitializationPolicy::AllowUninitialized => {
                if let Some(state_ref) = maybe_state_ref {
                    is_initialized = true;
                    state_and_base_from_ref(state_ref)?
                } else {
                    is_initialized = false;
                    let state = StackState::new(branch_head.clone());
                    let base = branch_head.clone();
                    (state, base)
                }
            }
        };

        ensure_patch_refs(repo, &branch_name, &state)?;
        Ok(Self {
            repo,
            branch_name,
            branch,
            branch_head,
            stack_refname,
            base,
            state,
            is_initialized,
        })
    }

    /// Check whether the stack is marked as protected in the config.
    pub(crate) fn is_protected(&self, config: &gix::config::Snapshot) -> bool {
        config
            .plumbing()
            .boolean_by(
                "branch",
                Some(format!("{}.stgit", self.branch_name).as_str().into()),
                "protect",
            )
            .unwrap_or(Ok(false))
            .unwrap_or(false)
    }

    /// Set the stack's protected state in the config.
    pub(crate) fn set_protected(&self, protect: bool) -> Result<()> {
        let section = "branch";
        let subsection = format!("{}.stgit", self.branch_name);
        let subsection = subsection.as_str();

        let mut local_config_file = self.repo.local_config_file()?;

        if protect {
            local_config_file.set_raw_value_by(
                section,
                Some(subsection.into()),
                "protect",
                "true",
            )?;
        } else {
            if let Ok(mut value) =
                local_config_file.raw_value_mut_by(section, Some(subsection.into()), "protect")
            {
                value.delete();
            }
            if let Ok(section) = local_config_file
                .section_by_key(format!("{section}.{subsection}").as_bytes().as_bstr())
            {
                if section.num_values() == 0 {
                    local_config_file.remove_section_by_id(section.id());
                }
            }
        }

        self.repo.write_local_config(local_config_file)?;
        Ok(())
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
        assert!(
            self.is_initialized,
            "Attempt to log stack state when uninitialized"
        );

        let prev_state_commit = self
            .repo
            .find_reference(&self.stack_refname)?
            .into_fully_peeled_id()?
            .object()?
            .try_into_commit()?;
        let prev_state_commit_id = prev_state_commit.id;
        let state = self
            .state
            .advance_head(self.branch_head.clone(), Rc::new(prev_state_commit));

        let message = message.unwrap_or(
            "external modifications\n\
             \n\
             Modifications by tools other than StGit (e.g. git).\n",
        );
        let reflog_msg = "external modifications";

        let state_commit_id = state.commit(self.repo, None, message)?;

        self.repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: reflog_msg.into(),
                },
                expected: gix::refs::transaction::PreviousValue::ExistingMustMatch(
                    gix::refs::Target::Peeled(prev_state_commit_id),
                ),
                new: gix::refs::Target::Peeled(state_commit_id),
            },
            name: gix::refs::FullName::try_from(self.stack_refname.as_str())?,
            deref: false,
        })?;

        Ok(Self { state, ..self })
    }

    /// Start a transaction to modify the stack.
    pub(crate) fn setup_transaction(self) -> TransactionBuilder<'repo> {
        assert!(
            self.is_initialized,
            "Attempt transaction with uninitialized stack state"
        );
        TransactionBuilder::new(self)
    }

    /// Clear the stack state history.
    pub(crate) fn clear_state_log(&mut self, reflog_msg: &str) -> Result<()> {
        self.state.prev = None;
        self.state
            .commit(self.repo, Some(&self.stack_refname), reflog_msg)?;
        Ok(())
    }

    /// Update the branch and branch head commit.
    pub(super) fn update_head(&mut self, branch: Branch<'repo>, commit: Rc<gix::Commit<'repo>>) {
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

impl<'repo> StackAccess<'repo> for Stack<'repo> {
    fn get_branch_name(&self) -> &str {
        &self.branch_name
    }

    fn get_branch_refname(&self) -> &gix::refs::FullNameRef {
        self.branch.get_reference_name()
    }

    fn get_stack_refname(&self) -> &str {
        &self.stack_refname
    }

    fn get_branch_head(&self) -> &Rc<gix::Commit<'repo>> {
        &self.branch_head
    }

    fn base(&self) -> &Rc<gix::Commit<'repo>> {
        &self.base
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

    fn top(&self) -> &Rc<gix::Commit<'repo>> {
        self.state.top()
    }

    fn head(&self) -> &Rc<gix::Commit<'repo>> {
        self.state.head()
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

/// Fix-up stack's patch references.
///
/// Ensures that each patch in the stack has a valid patch reference and that there are
/// no references for non-existing patches in this stack's patch ref namespace.
///
/// This is done when instantiating a [`Stack`] to guard against external modifications
/// to the stack's patch refs.
fn ensure_patch_refs(repo: &gix::Repository, branch_name: &str, state: &StackState) -> Result<()> {
    let patch_ref_prefix = get_patch_refname(branch_name, "");
    let mut state_patches: BTreeMap<&PatchName, &PatchState> = state.patches.iter().collect();

    for mut existing_ref in repo
        .references()?
        .all()?
        .filter_map(Result::ok)
        .filter(|reference| {
            reference
                .name()
                .as_bstr()
                .starts_with(patch_ref_prefix.as_bytes())
        })
    {
        if let Ok(existing_refname) = existing_ref.name().as_bstr().to_str() {
            let patchname_str = existing_refname
                .strip_prefix(&patch_ref_prefix)
                .expect("did starts_with above");
            if let Ok(existing_patchname) = PatchName::from_str(patchname_str) {
                if let Some(patchdesc) = state_patches.remove(&existing_patchname) {
                    if let Some(existing_id) = existing_ref.target().try_id() {
                        if existing_id == patchdesc.commit.id {
                            // Patch ref is good. Do nothing.
                        } else {
                            existing_ref
                                .set_target_id(patchdesc.commit.id, "fixup broken patch ref")?;
                        }
                    } else {
                        // Existing ref seems to be symbolic, and not direct.
                        repo.edit_reference(gix::refs::transaction::RefEdit {
                            change: gix::refs::transaction::Change::Update {
                                log: gix::refs::transaction::LogChange {
                                    mode: gix::refs::transaction::RefLog::AndReference,
                                    force_create_reflog: false,
                                    message: "fixup symbolic patch ref".into(),
                                },
                                expected: gix::refs::transaction::PreviousValue::ExistingMustMatch(
                                    existing_ref.target().into_owned(),
                                ),
                                new: gix::refs::Target::Peeled(patchdesc.commit.id),
                            },
                            name: existing_ref.name().into(),
                            deref: false,
                        })?;
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
        repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: "fixup missing patch ref".into(),
                },
                expected: gix::refs::transaction::PreviousValue::MustNotExist,
                new: gix::refs::Target::Peeled(patchdesc.commit.id),
            },
            name: gix::refs::FullName::try_from(get_patch_refname(
                branch_name,
                patchname.as_ref(),
            ))?,
            deref: false,
        })?;
    }

    Ok(())
}
