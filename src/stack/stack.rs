use std::collections::BTreeMap;
use std::str::FromStr;

use anyhow::{anyhow, Context, Result};
use git2::{Branch, Commit, RepositoryState};

use super::{
    error::{repo_state_to_str, Error},
    state::{StackState, StackStateAccess},
    transaction::TransactionBuilder,
    upgrade::stack_upgrade,
    PatchState,
};
use crate::patchname::PatchName;

pub(crate) struct Stack<'repo> {
    pub(crate) repo: &'repo git2::Repository,
    pub(crate) branch_name: String,
    pub(crate) branch: Branch<'repo>,
    pub(crate) branch_head: Commit<'repo>,
    pub(crate) refname: String,
    base: Commit<'repo>,
    state: StackState<'repo>,
}

impl<'repo> Stack<'repo> {
    pub(crate) fn initialize(
        repo: &'repo git2::Repository,
        branch_name: Option<&str>,
    ) -> Result<Self> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let branch_head = branch.get().peel_to_commit()?;
        let base = branch_head.clone();
        let refname = state_refname_from_branch_name(&branch_name);

        stack_upgrade(repo, &branch_name)?;

        if repo.find_reference(&refname).is_ok() {
            return Err(anyhow!("branch `{branch_name}` already initialized"));
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

    pub fn from_branch(repo: &'repo git2::Repository, branch_name: Option<&str>) -> Result<Self> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let branch_head = branch.get().peel_to_commit()?;

        stack_upgrade(repo, &branch_name)?;

        let refname = state_refname_from_branch_name(&branch_name);
        let state_ref = repo
            .find_reference(&refname)
            .map_err(|_| anyhow!("branch `{branch_name}` not initialized"))?;
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

    pub fn check_repository_state(&self, conflicts_okay: bool) -> Result<()> {
        if self.repo.index()?.has_conflicts() {
            if conflicts_okay {
                Ok(())
            } else {
                Err(Error::OutstandingConflicts.into())
            }
        } else {
            match self.repo.state() {
                RepositoryState::Clean => Ok(()),
                RepositoryState::Merge => {
                    if conflicts_okay {
                        Ok(())
                    } else {
                        Err(Error::OutstandingConflicts.into())
                    }
                }
                state => {
                    Err(Error::ActiveRepositoryState(repo_state_to_str(state).to_string()).into())
                }
            }
        }
    }

    pub fn is_head_top(&self) -> bool {
        self.state.applied.is_empty() || self.state.head.id() == self.branch_head.id()
    }

    pub fn check_head_top_mismatch(&self) -> Result<()> {
        if self.is_head_top() {
            Ok(())
        } else {
            Err(anyhow!(
                "HEAD and stack top are not the same. \
                 This can happen if you modify the branch with git. \
                 See `stg repair --help` for next steps to take."
            ))
        }
    }

    pub fn check_index_clean(&self) -> Result<()> {
        let mut status_options = git2::StatusOptions::new();
        status_options.show(git2::StatusShow::Index);
        if self.repo.statuses(Some(&mut status_options))?.is_empty() {
            Ok(())
        } else {
            Err(anyhow!("Index not clean. Use `refresh` or `reset --hard`"))
        }
    }

    pub fn check_worktree_clean(&self) -> Result<()> {
        let mut status_options = git2::StatusOptions::new();
        status_options.show(git2::StatusShow::Workdir);
        status_options.exclude_submodules(true);
        if self.repo.statuses(Some(&mut status_options))?.is_empty() {
            Ok(())
        } else {
            Err(anyhow!(
                "Worktree not clean. Use `refresh` or `reset --hard`"
            ))
        }
    }

    pub fn log_external_mods(self) -> Result<Self> {
        let state_ref = self.repo.find_reference(&self.refname)?;
        let prev_state_commit = state_ref.peel_to_commit()?;
        let prev_state_commit_id = prev_state_commit.id();
        let state = self
            .state
            .advance_head(self.branch_head.clone(), prev_state_commit);

        let message = "external modifications\n\
                       \n\
                       Modifications by tools other than StGit (e.g. git).\n";
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

    pub(crate) fn setup_transaction(self) -> TransactionBuilder<'repo> {
        TransactionBuilder::new(self)
    }

    pub(crate) fn update_head(&mut self, branch: git2::Branch<'repo>, commit: git2::Commit<'repo>) {
        self.branch = branch;
        self.branch_head = commit;
    }

    pub(super) fn state_mut(&mut self) -> &mut StackState<'repo> {
        &mut self.state
    }

    pub(crate) fn patch_refname(&self, patchname: &PatchName) -> String {
        get_patch_refname(&self.branch_name, patchname.as_ref())
    }

    pub(crate) fn patch_revspec(&self, patch_spec: &str) -> String {
        get_patch_refname(&self.branch_name, patch_spec)
    }
}

impl<'repo> StackStateAccess<'repo> for Stack<'repo> {
    fn applied(&self) -> &[PatchName] {
        &self.state.applied
    }

    fn unapplied(&self) -> &[PatchName] {
        &self.state.unapplied
    }

    fn hidden(&self) -> &[PatchName] {
        &self.state.hidden
    }

    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo> {
        &self.state.patches[patchname]
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        self.state.patches.contains_key(patchname)
    }

    fn top(&self) -> &Commit<'repo> {
        if let Some(patchname) = self.applied().last() {
            &self.state.patches[patchname].commit
        } else {
            &self.state.head
        }
    }

    fn head(&self) -> &Commit<'repo> {
        &self.state.head
    }

    fn base(&self) -> &Commit<'repo> {
        &self.base
    }
}

fn state_refname_from_branch_name(branch_shorthand: &str) -> String {
    format!("refs/stacks/{branch_shorthand}")
}

fn get_patch_refname(branch_name: &str, patch_spec: &str) -> String {
    format!("refs/patches/{branch_name}/{patch_spec}")
}

fn get_branch_name(branch: &Branch<'_>) -> Result<String> {
    let name_bytes = branch.name_bytes()?;
    Ok(std::str::from_utf8(name_bytes)
        .map_err(|_| {
            anyhow!(
                "non-UTF-8 branch name `{}`",
                String::from_utf8_lossy(name_bytes)
            )
        })?
        .to_string())
}

fn get_branch<'repo>(
    repo: &'repo git2::Repository,
    branch_name: Option<&str>,
) -> Result<git2::Branch<'repo>> {
    if let Some(name) = branch_name {
        let branch =
            repo.find_branch(name, git2::BranchType::Local)
                .map_err(|e| -> anyhow::Error {
                    if e.class() == git2::ErrorClass::Reference {
                        match e.code() {
                            git2::ErrorCode::NotFound => {
                                anyhow!("branch `{name}` not found")
                            }
                            git2::ErrorCode::InvalidSpec => {
                                anyhow!("invalid branch name `{name}`")
                            }
                            git2::ErrorCode::UnbornBranch => {
                                anyhow!("unborn branch `{name}`")
                            }
                            _ => e.into(),
                        }
                    } else {
                        e.into()
                    }
                })?;
        Ok(branch)
    } else if repo.head_detached()? {
        Err(anyhow!("not on branch, HEAD is detached"))
    } else {
        let head = repo.head().context("failed to get HEAD reference")?;
        if head.is_branch() {
            Ok(git2::Branch::wrap(head))
        } else {
            Err(anyhow!(
                "not on branch, HEAD points at `{}`",
                String::from_utf8_lossy(head.name_bytes())
            ))
        }
    }
}

fn ensure_patch_refs<'repo>(
    repo: &'repo git2::Repository,
    branch_name: &str,
    state: &StackState,
) -> Result<()> {
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
