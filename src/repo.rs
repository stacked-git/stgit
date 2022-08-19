// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git2::Repository`].

use anyhow::{anyhow, Context, Result};

use crate::stack::Error;

/// Extends [`git2::Repository`] with additional methods.
pub(crate) trait RepositoryExtended {
    /// Check whether there are any files with conflicts in the index.
    fn check_conflicts(&self) -> Result<()>;

    /// Determine whether the repository's default index is clean.
    ///
    /// A clean index is one that does not record and differences from the `HEAD` tree.
    fn check_index_clean(&self) -> Result<()>;

    /// Determine whether the work tree is clean.
    ///
    /// A clean work tree does not have any git-managed files changed relative to the
    /// `HEAD` tree.
    fn check_worktree_clean(&self) -> Result<()>;

    /// Determine whether both the default index and current work tree are clean.
    fn check_index_and_worktree_clean(&self) -> Result<()>;

    /// Determine whether the repository is in a clean state.
    ///
    /// A clean repository is not in the middle of any of a variety of stateful operations such
    /// as merge, rebase, cherrypick, etc.; see [`git2::RepositoryState`].
    fn check_repository_state(&self) -> Result<()>;

    /// Get [`git2::Branch`], with StGit-specific error messaging.
    ///
    /// Gets the current branch if the provided `branch_name` is `None`,
    fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>>;

    /// [`git2::Repository::checkout_tree()`] with StGit-specfic error mapping.
    fn checkout_tree_ex(
        &self,
        treeish: &git2::Object<'_>,
        opts: Option<&mut git2::build::CheckoutBuilder<'_>>,
    ) -> Result<()>;
}

impl RepositoryExtended for git2::Repository {
    fn check_conflicts(&self) -> Result<()> {
        if self
            .index()
            .context("checking for conflicts")?
            .has_conflicts()
        {
            Err(Error::OutstandingConflicts.into())
        } else {
            Ok(())
        }
    }

    fn check_index_clean(&self) -> Result<()> {
        let mut status_options = git2::StatusOptions::new();
        status_options.show(git2::StatusShow::Index);
        if self.statuses(Some(&mut status_options))?.is_empty() {
            Ok(())
        } else {
            Err(anyhow!("Index not clean. Use `refresh` or `reset --hard`"))
        }
    }

    fn check_worktree_clean(&self) -> Result<()> {
        let mut status_options = git2::StatusOptions::new();
        status_options.show(git2::StatusShow::Workdir);
        status_options.exclude_submodules(true);
        if self.statuses(Some(&mut status_options))?.is_empty() {
            Ok(())
        } else {
            Err(anyhow!(
                "Worktree not clean. Use `refresh` or `reset --hard`"
            ))
        }
    }

    fn check_index_and_worktree_clean(&self) -> Result<()> {
        let mut options = git2::StatusOptions::new();
        options.show(git2::StatusShow::IndexAndWorkdir);
        options.exclude_submodules(true);
        let statuses = self.statuses(Some(&mut options))?;
        if statuses.is_empty() {
            Ok(())
        } else {
            let index_dirty_bits = git2::Status::INDEX_NEW.bits()
                | git2::Status::INDEX_DELETED.bits()
                | git2::Status::INDEX_RENAMED.bits()
                | git2::Status::INDEX_MODIFIED.bits()
                | git2::Status::INDEX_TYPECHANGE.bits()
                | git2::Status::CONFLICTED.bits();
            let wt_dirty_bits = git2::Status::WT_NEW.bits()
                | git2::Status::WT_DELETED.bits()
                | git2::Status::WT_RENAMED.bits()
                | git2::Status::WT_MODIFIED.bits()
                | git2::Status::WT_TYPECHANGE.bits();
            let index_dirty = statuses
                .iter()
                .any(|entry| entry.status().bits() & index_dirty_bits != 0);
            let wt_dirty = statuses
                .iter()
                .any(|entry| entry.status().bits() & wt_dirty_bits != 0);

            if index_dirty && wt_dirty {
                Err(anyhow!(
                    "Index and worktree not clean. Use `refresh` or `reset --hard`"
                ))
            } else if index_dirty {
                Err(anyhow!("Index not clean. Use `refresh` or `reset --hard`"))
            } else if wt_dirty {
                Err(anyhow!(
                    "Worktree not clean. Use `refresh` or `reset --hard`"
                ))
            } else {
                panic!("expected either/both worktree or index to be dirty")
            }
        }
    }

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
            let branch =
                self.find_branch(name, git2::BranchType::Local)
                    .map_err(|e| -> anyhow::Error {
                        if e.class() == git2::ErrorClass::Reference {
                            match e.code() {
                                git2::ErrorCode::NotFound => {
                                    anyhow!("Branch `{name}` not found")
                                }
                                git2::ErrorCode::InvalidSpec => {
                                    anyhow!("Invalid branch name `{name}`")
                                }
                                git2::ErrorCode::UnbornBranch => {
                                    anyhow!("Unborn branch `{name}`")
                                }
                                _ => e.into(),
                            }
                        } else {
                            e.into()
                        }
                    })?;
            Ok(branch)
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

    fn checkout_tree_ex(
        &self,
        treeish: &git2::Object<'_>,
        opts: Option<&mut git2::build::CheckoutBuilder<'_>>,
    ) -> Result<()> {
        self.checkout_tree(treeish, opts)
            .map_err(|e| -> anyhow::Error {
                if e.class() == git2::ErrorClass::Checkout && e.code() == git2::ErrorCode::Conflict
                {
                    Error::CheckoutConflicts(e.message().to_string()).into()
                } else {
                    e.into()
                }
            })
    }
}
