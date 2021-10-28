mod iter;
mod state;

use std::str;

use git2::{Branch, Repository, RepositoryState};

use crate::error::Error;
use iter::AllPatches;
pub(crate) use state::PatchDescriptor;
use state::StackState;

pub(crate) struct Stack<'repo> {
    branch_name: String,
    branch: Branch<'repo>,
    repo: &'repo Repository,
    pub state: StackState,
}

impl<'repo> Stack<'repo> {
    pub(crate) fn initialize(
        repo: &'repo Repository,
        branch_name: Option<&str>,
    ) -> Result<Self, Error> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let stack_refname = stack_refname_from_branch_name(&branch_name);

        if repo.find_reference(&stack_refname).is_ok() {
            return Err(Error::StackAlreadyInitialized(branch_name));
        }
        let state = StackState::new(repo.head()?.peel_to_commit()?.id());
        state.commit(repo, Some(&stack_refname), "initialize")?;

        Ok(Self {
            branch_name,
            branch,
            repo,
            state,
        })
    }

    pub fn from_branch(repo: &'repo Repository, branch_name: Option<&str>) -> Result<Self, Error> {
        let branch = get_branch(repo, branch_name)?;
        let branch_name = get_branch_name(&branch)?;
        let stack_refname = stack_refname_from_branch_name(&branch_name);
        let stack_ref = repo
            .find_reference(&stack_refname)
            .map_err(|_| Error::StackNotInitialized(branch_name.to_string()))?;
        let stack_tree = stack_ref.peel_to_tree()?;
        let state = StackState::from_tree(repo, &stack_tree)?;
        Ok(Self {
            branch_name,
            branch,
            repo,
            state,
        })
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

    pub fn check_head_top_mismatch(&self) -> Result<(), Error> {
        let head_oid = self.branch.get().peel_to_commit()?.id();
        let head_oid2 = self.repo.head()?.peel_to_commit()?.id();
        assert_eq!(head_oid, head_oid2);
        if head_oid != self.state.top() {
            Err(Error::StackTopHeadMismatch)
        } else {
            Ok(())
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
}

fn stack_refname_from_branch_name(branch_shorthand: &str) -> String {
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
    Ok(str::from_utf8(name_bytes)
        .map_err(|_| Error::NonUtf8BranchName(String::from_utf8_lossy(name_bytes).to_string()))?
        .to_string())
}

fn repo_state_to_str(state: RepositoryState) -> &'static str {
    match state {
        RepositoryState::Clean => "clean",
        RepositoryState::Merge => "merge",
        RepositoryState::Revert | RepositoryState::RevertSequence => "revert",
        RepositoryState::CherryPick | RepositoryState::CherryPickSequence => "cherry-pick",
        RepositoryState::Bisect => "bisect",
        RepositoryState::Rebase => "rebase",
        RepositoryState::RebaseInteractive => "interactive rebase",
        RepositoryState::RebaseMerge => "rebase merge",
        RepositoryState::ApplyMailbox => "apply mailbox",
        RepositoryState::ApplyMailboxOrRebase => "rebase or apply mailbox",
    }
}
