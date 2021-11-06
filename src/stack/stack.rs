use git2::{Branch, Commit, Oid, Reference, Repository, RepositoryState, Tree};

use super::iter::AllPatches;
use super::state::StackState;
use crate::error::{repo_state_to_str, Error};
use crate::patchname::PatchName;

pub(crate) struct Stack<'repo> {
    pub(crate) repo: &'repo Repository,
    pub(crate) branch_name: String,
    pub(crate) branch: Branch<'repo>,
    pub(crate) head_commit: Commit<'repo>,
    pub(crate) head_tree: Tree<'repo>,
    pub(crate) state_ref: Reference<'repo>,
    pub(crate) state: StackState,
}

impl<'repo> Stack<'repo> {
    pub(crate) fn initialize(
        repo: &'repo Repository,
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
        let state_ref = repo.find_reference(&state_refname)?;

        Ok(Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
        })
    }

    pub fn from_branch(repo: &'repo Repository, branch_name: Option<&str>) -> Result<Self, Error> {
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
        Ok(Self {
            repo,
            branch_name,
            branch,
            head_commit,
            head_tree,
            state_ref,
            state,
        })
    }

    pub fn base_id(&self) -> Oid {
        if let Some(base_patchname) = self.state.applied.first() {
            self.state.patches[base_patchname].oid
        } else {
            self.head_commit.id()
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
            state @ _ => Err(Error::ActiveRepositoryState(
                repo_state_to_str(state).to_string(),
            )),
        }
    }

    pub fn is_head_top(&self) -> bool {
        self.state.head == self.head_commit.id()
    }

    pub fn check_head_top_mismatch(&self) -> Result<(), Error> {
        if self.is_head_top() {
            Ok(())
        } else {
            Err(Error::StackTopHeadMismatch)
        }
    }

    pub fn check_index_clean(&self) -> Result<(), Error> {
        if self.repo.index()?.is_empty() {
            Ok(())
        } else {
            Err(Error::DirtyIndex)
        }
    }

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
        })
    }

    pub(crate) fn patch_refname(&self, patchname: &PatchName) -> String {
        format!("refs/patches/{}/{}", &self.branch_name, patchname)
    }

    pub(crate) fn patch_revspec(&self, patch_spec: &str) -> String {
        format!("refs/patches/{}/{}", &self.branch_name, patch_spec)
    }
}

fn state_refname_from_branch_name(branch_shorthand: &str) -> String {
    format!("refs/stacks/{}", branch_shorthand)
}

fn get_branch<'repo>(
    repo: &'repo Repository,
    branch_name: Option<&str>,
) -> Result<Branch<'repo>, Error> {
    if let Some(name) = branch_name {
        let branch = repo
            .find_branch(name, git2::BranchType::Local)
            .map_err(|e| {
                if e.class() == git2::ErrorClass::Reference {
                    if e.code() == git2::ErrorCode::NotFound {
                        Error::BranchNotFound(name.to_string())
                    } else if e.code() == git2::ErrorCode::InvalidSpec {
                        Error::InvalidBranchName(name.to_string())
                    } else {
                        e.into()
                    }
                } else {
                    e.into()
                }
            })?;
        Ok(branch)
    } else if repo.head_detached()? {
        Err(Error::HeadDetached)
    } else {
        let head = repo.head()?;
        if head.is_branch() {
            Ok(Branch::wrap(head))
        } else {
            Err(Error::HeadNotBranch(
                String::from_utf8_lossy(head.name_bytes()).to_string(),
            ))
        }
    }
}

fn get_branch_name<'repo>(branch: &'repo Branch<'_>) -> Result<String, Error> {
    let name_bytes = branch.name_bytes()?;
    Ok(std::str::from_utf8(name_bytes)
        .map_err(|_| Error::NonUtf8BranchName(String::from_utf8_lossy(name_bytes).to_string()))?
        .to_string())
}
