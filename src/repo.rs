use anyhow::{anyhow, Context, Result};

use crate::stack::Error;

pub(crate) trait RepositoryExtended {
    fn check_index_clean(&self) -> Result<()>;
    fn check_worktree_clean(&self) -> Result<()>;
    fn check_index_and_worktree_clean(&self) -> Result<()>;
    fn check_repository_state(&self, conflicts_okay: bool) -> Result<()>;
    fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>>;
}

impl RepositoryExtended for git2::Repository {
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

    fn check_repository_state(&self, conflicts_okay: bool) -> Result<()> {
        if self.index()?.has_conflicts() {
            if conflicts_okay {
                Ok(())
            } else {
                Err(Error::OutstandingConflicts.into())
            }
        } else {
            match self.state() {
                git2::RepositoryState::Clean => Ok(()),
                git2::RepositoryState::Merge => {
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

    fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>> {
        if let Some(name) = branch_name {
            let branch =
                self.find_branch(name, git2::BranchType::Local)
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
        } else if self.head_detached()? {
            Err(anyhow!("not on branch, HEAD is detached"))
        } else {
            let head = self.head().context("getting HEAD reference")?;
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
}

pub(crate) fn repo_state_to_str(state: git2::RepositoryState) -> &'static str {
    match state {
        git2::RepositoryState::Clean => "clean",
        git2::RepositoryState::Merge => "merge",
        git2::RepositoryState::Revert | git2::RepositoryState::RevertSequence => "revert",
        git2::RepositoryState::CherryPick | git2::RepositoryState::CherryPickSequence => {
            "cherry-pick"
        }
        git2::RepositoryState::Bisect => "bisect",
        git2::RepositoryState::Rebase => "rebase",
        git2::RepositoryState::RebaseInteractive => "interactive rebase",
        git2::RepositoryState::RebaseMerge => "rebase merge",
        git2::RepositoryState::ApplyMailbox => "apply mailbox",
        git2::RepositoryState::ApplyMailboxOrRebase => "rebase or apply mailbox",
    }
}
