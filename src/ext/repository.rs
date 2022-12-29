// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git2::Repository`].

use anyhow::{anyhow, Context, Result};

use crate::{stupid::Stupid, wrap::Message};

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

    /// Create a new commit object in the repository, with extended features.
    ///
    /// The extended features versus [`git2::Repository::commit()`] include:
    ///
    /// - Respecting `i18n.commitEncoding` for commit messages.
    /// - Respecting `commit.gpgSign` and creating signed commits when enabled.
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &Message,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid>;

    /// Create a new commit object in the repository.
    ///
    /// The provided [`CommitOptions`] gives finer-grained control versus
    /// [`RepositoryExtended::commit_ex()`].
    fn commit_with_options(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &Message,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
        options: &CommitOptions,
    ) -> Result<git2::Oid>;
}

/// Options for creating a git commit object.
pub(crate) struct CommitOptions<'a> {
    /// The target encoding for the commit message.
    pub(crate) commit_encoding: Option<&'a str>,

    /// Determine whether the commit object should be signed with GPG.
    pub(crate) gpgsign: bool,
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

    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &Message,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid> {
        let config = self.config()?;
        let commit_encoding = config.get_string("i18n.commitencoding").ok();
        let commit_encoding = commit_encoding.as_deref();
        let gpgsign = config.get_bool("commit.gpgsign").unwrap_or(false);
        self.commit_with_options(
            author,
            committer,
            message,
            tree_id,
            parent_ids,
            &CommitOptions {
                commit_encoding,
                gpgsign,
            },
        )
    }

    fn commit_with_options(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &Message,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
        options: &CommitOptions<'_>,
    ) -> Result<git2::Oid> {
        let commit_encoding = match options.commit_encoding {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s.as_bytes())
                    .ok_or_else(|| anyhow!("Unhandled i18n.commitEncoding `{s}`"))?;
                Some(encoding)
            }
            None => None,
        };

        if options.gpgsign
            || (commit_encoding.is_some() && commit_encoding != Some(encoding_rs::UTF_8))
        {
            // Use git for any commit that needs to be signed
            self.stupid().commit_tree(
                author,
                committer,
                &message.encode_with(commit_encoding)?,
                tree_id,
                parent_ids,
                options.gpgsign,
            )
        } else {
            // Use git2 for all other occasions
            let decoded_message = message.decode()?;
            let tree = self.find_tree(tree_id)?;
            let mut parents: Vec<git2::Commit<'_>> = Vec::new();
            for parent_id in parent_ids {
                parents.push(self.find_commit(parent_id)?);
            }
            let parents: Vec<&git2::Commit<'_>> = parents.iter().collect();

            Ok(self.commit(None, author, committer, &decoded_message, &tree, &parents)?)
        }
    }
}
