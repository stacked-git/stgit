use std::collections::BTreeMap;
use std::str::FromStr;

use git2::{Branch, Commit, Oid, Reference, RepositoryState, Tree};

use super::iter::AllPatches;
use super::state::StackState;
use super::PatchDescriptor;
use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;
use crate::wrap::repository::get_branch;

type PatchMap<'repo> = BTreeMap<PatchName, Commit<'repo>>;

pub(crate) struct Stack<'repo> {
    pub(crate) repo: &'repo git2::Repository,
    pub(crate) branch_name: String,
    pub(crate) branch: Branch<'repo>,
    pub(crate) head_commit: Commit<'repo>,
    pub(crate) head_tree: Tree<'repo>,
    pub(crate) state_ref: Reference<'repo>,
    pub(crate) state: StackState,
    pub(crate) patchmap: PatchMap<'repo>,
}

impl<'repo> Stack<'repo> {
    pub(crate) fn initialize(
        repo: &'repo git2::Repository,
        branch_name: Option<&str>,
    ) -> Result<Self, Error> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let head_commit = branch.get().peel_to_commit()?;
        let head_tree = branch.get().peel_to_tree()?;
        let state_refname = state_refname_from_branch_name(&branch_name);

        if repo.find_reference(&state_refname).is_ok() {
            return Err(Error::StackAlreadyInitialized(branch_name));
        }
        let state = StackState::new(head_commit.id());
        state.commit(repo, Some(&state_refname), "initialize")?;
        ensure_patch_refs(repo, &branch_name, &state)?;
        let patchmap = PatchMap::new();
        let state_ref = repo.find_reference(&state_refname)?;

        Ok(Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
            patchmap,
        })
    }

    pub fn from_branch(
        repo: &'repo git2::Repository,
        branch_name: Option<&str>,
    ) -> Result<Self, Error> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let head_commit = branch.get().peel_to_commit()?;
        let head_tree = branch.get().peel_to_tree()?;
        let stack_refname = state_refname_from_branch_name(&branch_name);
        let state_ref = repo
            .find_reference(&stack_refname)
            .map_err(|_| Error::StackNotInitialized(branch_name.to_string()))?;
        let stack_tree = state_ref.peel_to_tree()?;
        let state = StackState::from_tree(repo, &stack_tree)?;
        ensure_patch_refs(repo, &branch_name, &state)?;
        let mut patchmap = PatchMap::new();
        for (patchname, patchdesc) in &state.patches {
            patchmap.insert(patchname.clone(), repo.find_commit(patchdesc.oid)?);
        }
        Ok(Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
            patchmap,
        })
    }

    pub fn base_commit(&self) -> Result<Commit<'repo>, Error> {
        if let Some(first_patchname) = self.state.applied.first() {
            let first_patch_id = &self.state.patches[first_patchname].oid;
            Ok(self.repo.find_commit(*first_patch_id)?.parent(0)?)
        } else {
            Ok(self.head_commit.clone())
        }
    }

    pub fn all_patches(&self) -> AllPatches<'_> {
        self.state.all_patches()
    }

    pub fn check_repository_state(&self, conflicts_okay: bool) -> Result<(), Error> {
        match self.repo.state() {
            RepositoryState::Clean => Ok(()),
            RepositoryState::Merge => {
                if conflicts_okay {
                    Ok(())
                } else {
                    Err(Error::OutstandingConflicts)
                }
            }
            state => Err(Error::ActiveRepositoryState(
                repo_state_to_str(state).to_string(),
            )),
        }
    }

    pub fn is_head_top(&self) -> bool {
        self.state.applied.is_empty() || self.state.head == self.head_commit.id()
    }

    pub fn check_head_top_mismatch(&self) -> Result<(), Error> {
        if self.is_head_top() {
            Ok(())
        } else {
            Err(Error::StackTopHeadMismatch)
        }
    }

    #[allow(dead_code)]
    pub fn check_index_clean(&self) -> Result<(), Error> {
        if self.repo.index()?.is_empty() {
            Ok(())
        } else {
            Err(Error::DirtyIndex)
        }
    }

    #[allow(dead_code)]
    pub fn check_worktree_clean(&self) -> Result<(), Error> {
        let mut status_options = git2::StatusOptions::new();
        status_options
            .show(git2::StatusShow::Workdir)
            .update_index(true);
        if self.repo.statuses(Some(&mut status_options))?.is_empty() {
            Ok(())
        } else {
            Err(Error::DirtyWorktree)
        }
    }

    pub fn advance_state(
        self,
        new_head: Oid,
        prev_state: Oid,
        message: &str,
        reflog_msg: Option<&str>,
    ) -> Result<Self, Error> {
        let Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
            patchmap,
        } = self;
        let state = state.advance_head(new_head, prev_state);
        let state_commit_oid = state.commit(repo, None, message)?;
        let reflog_msg = if let Some(reflog_msg) = reflog_msg {
            reflog_msg
        } else {
            message
        };
        let state_ref = self.repo.reference_matching(
            state_ref.name().unwrap(),
            state_commit_oid,
            true,
            prev_state,
            reflog_msg,
        )?;
        Ok(Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
            patchmap,
        })
    }

    pub(crate) fn patch_refname(&self, patchname: &PatchName) -> String {
        get_patch_refname(&self.branch_name, patchname.as_ref())
    }

    pub(crate) fn patch_revspec(&self, patch_spec: &str) -> String {
        get_patch_refname(&self.branch_name, patch_spec)
    }
}

fn state_refname_from_branch_name(branch_shorthand: &str) -> String {
    format!("refs/stacks/{}", branch_shorthand)
}

fn get_patch_refname(branch_name: &str, patch_spec: &str) -> String {
    format!("refs/patches/{}/{}", &branch_name, patch_spec)
}

fn get_branch_name(branch: &Branch<'_>) -> Result<String, Error> {
    let name_bytes = branch.name_bytes()?;
    Ok(std::str::from_utf8(name_bytes)
        .map_err(|_| Error::NonUtf8BranchName(String::from_utf8_lossy(name_bytes).to_string()))?
        .to_string())
}

fn ensure_patch_refs<'repo>(
    repo: &'repo git2::Repository,
    branch_name: &str,
    state: &StackState,
) -> Result<(), Error> {
    let patch_ref_prefix = get_patch_refname(branch_name, "");
    let patch_ref_glob = get_patch_refname(branch_name, "*");
    let mut state_patches: BTreeMap<&PatchName, &PatchDescriptor> = state.patches.iter().collect();

    for existing_ref in repo.references_glob(&patch_ref_glob)? {
        let mut existing_ref = existing_ref?;
        if let Some(existing_refname) = existing_ref.name() {
            let existing_patchname = existing_refname.strip_prefix(&patch_ref_prefix).unwrap();
            if let Ok(existing_patchname) = PatchName::from_str(existing_patchname) {
                if let Some(patchdesc) = state_patches.remove(&existing_patchname) {
                    if let Some(existing_id) = existing_ref.target() {
                        if existing_id == patchdesc.oid {
                            // Patch ref is good. Do nothing.
                        } else {
                            existing_ref.set_target(patchdesc.oid, "fixup broken patch ref")?;
                        }
                    } else {
                        // Existing ref seems to be symbolic, and not direct.
                        repo.reference(
                            existing_refname,
                            patchdesc.oid,
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
            patchdesc.oid,
            false,
            "fixup missing patch ref",
        )?;
    }

    Ok(())
}
