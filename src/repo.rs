// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git2::Repository`].

use anyhow::{anyhow, Context, Result};

/// Extends [`git2::Repository`] with additional methods.
pub(crate) trait RepositoryExtended {
    /// Determine whether the repository is in a clean state.
    ///
    /// A clean repository is not in the middle of any of a variety of stateful operations such
    /// as merge, rebase, cherrypick, etc.; see [`git2::RepositoryState`].
    fn check_repository_state(&self) -> Result<()>;

    /// Get [`git2::Branch`], with StGit-specific error messaging.
    ///
    /// Gets the current branch if the provided `branch_name` is `None`,
    fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>>;
}

impl RepositoryExtended for git2::Repository {
    fn check_repository_state(&self) -> Result<()> {
        match self.state() {
            git2::RepositoryState::Clean => Ok(()),
            state => {
                let state_str = match state {
                    git2::RepositoryState::Clean => "clean",
                    git2::RepositoryState::Merge => "merge",
                    git2::RepositoryState::Revert | git2::RepositoryState::RevertSequence => {
                        "revert"
                    }
                    git2::RepositoryState::CherryPick
                    | git2::RepositoryState::CherryPickSequence => "cherry-pick",
                    git2::RepositoryState::Bisect => "bisect",
                    git2::RepositoryState::Rebase => "rebase",
                    git2::RepositoryState::RebaseInteractive => "interactive rebase",
                    git2::RepositoryState::RebaseMerge => "rebase merge",
                    git2::RepositoryState::ApplyMailbox => "apply mailbox",
                    git2::RepositoryState::ApplyMailboxOrRebase => "rebase or apply mailbox",
                };
                Err(anyhow!(
                    "Complete the in-progress `{state_str}` before trying again",
                ))
            }
        }
    }

    fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>> {
        if let Some(name) = branch_name {
            self.find_branch(name, git2::BranchType::Local)
                .or_else(|e| match (e.class(), e.code()) {
                    (git2::ErrorClass::Reference, git2::ErrorCode::NotFound) => {
                        if let Ok(reference) = self.find_reference(name) {
                            if reference.is_branch() {
                                Ok(git2::Branch::wrap(reference))
                            } else {
                                Err(anyhow!("Reference `{name}` is not a branch"))
                            }
                        } else {
                            Err(anyhow!("Branch `{name}` not found"))
                        }
                    }
                    (git2::ErrorClass::Reference, git2::ErrorCode::InvalidSpec) => {
                        Err(anyhow!("Invalid branch name `{name}`"))
                    }
                    (git2::ErrorClass::Reference, git2::ErrorCode::UnbornBranch) => {
                        Err(anyhow!("Unborn branch `{name}`"))
                    }
                    _ => Err(e.into()),
                })
        } else if self.head_detached()? {
            Err(anyhow!("Not on branch, HEAD is detached"))
        } else {
            let head = self.head().context("getting HEAD reference")?;
            if head.is_branch() {
                Ok(git2::Branch::wrap(head))
            } else {
                Err(anyhow!(
                    "Not on branch, HEAD points at `{}`",
                    String::from_utf8_lossy(head.name_bytes())
                ))
            }
        }
    }
}
