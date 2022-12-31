// SPDX-License-Identifier: GPL-2.0-only

use std::borrow::Cow;

use anyhow::{anyhow, Result};
use bstr::BStr;

use crate::{
    stupid::Stupid,
    wrap::{Branch, Message},
};

/// Extends [`git_repository::Repository`] with additional methods.
pub(crate) trait RepositoryExtended {
    /// Open git repository based on current directory and any environment overrides.
    fn open() -> Result<git_repository::Repository> {
        Ok(git_repository::ThreadSafeRepository::discover_with_environment_overrides(".")?.into())
    }

    /// Determine whether the repository is in a clean state.
    ///
    /// A clean repository is not in the middle of any of a variety of stateful operations such
    /// as merge, rebase, cherrypick, etc.; see [`git_repository::state::InProgress`].
    fn check_repository_state(&self) -> Result<()>;

    /// Get [`Branch`], with StGit-specific error messaging.
    ///
    /// Gets the current branch if the provided `branch_name` is `None`,
    fn get_branch(&self, branch_name: Option<&str>) -> Result<Branch<'_>>;

    /// Get repository-local config file which can be used to change local configuration.
    fn local_config_file(&self) -> Result<git_repository::config::File<'static>>;

    /// Write repository-local config file.
    fn write_local_config(&self, file: git_repository::config::File) -> Result<()>;

    /// Find [`git_repository::Tree`] by its object id.
    ///
    /// The provided object id must point to a tree object. I.e. this will not peel a
    /// commit to a tree.
    fn find_tree(
        &self,
        id: impl Into<git_repository::ObjectId>,
    ) -> Result<git_repository::Tree<'_>>;

    /// Find [`git_repository::Commit`] by its object id.
    ///
    /// The provided object id must point to a commit object. An id pointing to a tag
    /// will not be peeled to a commit.
    fn find_commit(
        &self,
        id: impl Into<git_repository::ObjectId>,
    ) -> Result<git_repository::Commit<'_>>;

    /// Create a new commit object in the repository, with extended features.
    ///
    /// The extended features versus [`git_repository::Repository::commit()`] include:
    ///
    /// - Respecting `i18n.commitEncoding` for commit messages.
    /// - Respecting `commit.gpgSign` and creating signed commits when enabled.
    fn commit_ex<'a>(
        &self,
        author: impl Into<git_repository::actor::SignatureRef<'a>>,
        committer: impl Into<git_repository::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: git_repository::ObjectId,
        parent_ids: impl IntoIterator<Item = git_repository::ObjectId>,
    ) -> Result<git_repository::ObjectId>;

    /// Create a new commit object in the repository.
    ///
    /// The provided [`CommitOptions`] gives finer-grained control versus
    /// [`RepositoryExtended::commit_ex()`].
    fn commit_with_options<'a>(
        &self,
        author: impl Into<git_repository::actor::SignatureRef<'a>>,
        committer: impl Into<git_repository::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: git_repository::ObjectId,
        parent_ids: impl IntoIterator<Item = git_repository::ObjectId>,
        options: &CommitOptions<'_>,
    ) -> Result<git_repository::ObjectId>;
}

/// Options for creating a git commit object.
pub(crate) struct CommitOptions<'a> {
    /// The target encoding for the commit message.
    pub(crate) commit_encoding: Option<Cow<'a, BStr>>,

    /// Determine whether the commit object should be signed with GPG.
    pub(crate) gpgsign: bool,
}

impl RepositoryExtended for git_repository::Repository {
    fn check_repository_state(&self) -> Result<()> {
        use git_repository::state::InProgress;
        if let Some(state) = self.state() {
            let state_str = match state {
                InProgress::ApplyMailbox => "apply mailbox",
                InProgress::ApplyMailboxRebase => "rebase or apply mailbox",
                InProgress::Bisect => "bisect",
                InProgress::CherryPick | InProgress::CherryPickSequence => "cherry-pick",
                InProgress::Merge => "merge",
                InProgress::Rebase => "rebase",
                InProgress::RebaseInteractive => "interactive rebase",
                InProgress::Revert | InProgress::RevertSequence => "revert",
            };
            Err(anyhow!(
                "Complete the in-progress `{state_str}` before trying again",
            ))
        } else {
            Ok(())
        }
    }

    fn get_branch(&self, branch_name: Option<&str>) -> Result<Branch<'_>> {
        use git_repository::{head::Kind, refs::Category};

        if let Some(name) = branch_name {
            let reference = self.find_reference(name).map_err(|e| match e {
                git_repository::reference::find::existing::Error::Find(inner) => {
                    anyhow!("Invalid branch name `{name}`: {inner}")
                }
                git_repository::reference::find::existing::Error::NotFound => {
                    anyhow!("Branch `{name}` not found")
                }
            })?;

            if matches!(reference.name().category(), Some(Category::LocalBranch),) {
                Ok(Branch::wrap(reference))
            } else {
                Err(anyhow!("Reference `{name}` is not a local branch"))
            }
        } else {
            match self.head()?.kind {
                Kind::Symbolic(inner_reference) => {
                    if matches!(inner_reference.name.category(), Some(Category::LocalBranch)) {
                        let reference = self
                            .find_reference(inner_reference.name.as_bstr())
                            .expect("inner reference is known to be valid");
                        Ok(Branch::wrap(reference))
                    } else {
                        Err(anyhow!(
                            "HEAD points to `{}` which is not a local branch",
                            inner_reference.name
                        ))
                    }
                }
                Kind::Unborn(full_name) => Err(anyhow!("branch `{full_name}` is unborn")),
                Kind::Detached {
                    target: _,
                    peeled: _,
                } => Err(anyhow!("Not on branch, HEAD is detached")),
            }
        }
    }

    fn local_config_file(&self) -> Result<git_repository::config::File<'static>> {
        let source = git_repository::config::Source::Local;

        let local_config_path = self.git_dir().join(
            source
                .storage_location(&mut |n| std::env::var_os(n))
                .expect("know repo-local config path"),
        );

        Ok(git_repository::config::File::from_path_no_includes(
            local_config_path,
            source,
        )?)
    }

    fn write_local_config(&self, file: git_repository::config::File) -> Result<()> {
        let local_config_path = self.git_dir().join(
            git_repository::config::Source::Local
                .storage_location(&mut |n| std::env::var_os(n))
                .expect("know repo-local config path"),
        );

        file.write_to(
            std::fs::File::options()
                .truncate(true)
                .write(true)
                .open(local_config_path)?,
        )?;
        Ok(())
    }

    fn find_tree(
        &self,
        id: impl Into<git_repository::ObjectId>,
    ) -> Result<git_repository::Tree<'_>> {
        Ok(self.find_object(id)?.try_into_tree()?)
    }

    fn find_commit(
        &self,
        id: impl Into<git_repository::ObjectId>,
    ) -> Result<git_repository::Commit<'_>> {
        Ok(self.find_object(id)?.try_into_commit()?)
    }

    fn commit_ex<'a>(
        &self,
        author: impl Into<git_repository::actor::SignatureRef<'a>>,
        committer: impl Into<git_repository::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: git_repository::ObjectId,
        parent_ids: impl IntoIterator<Item = git_repository::ObjectId>,
    ) -> Result<git_repository::ObjectId> {
        let config = self.config_snapshot();
        let commit_encoding = config.string("i18n.commitencoding");
        let gpgsign = config.boolean("commit.gpgsign").unwrap_or(false);
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

    fn commit_with_options<'a>(
        &self,
        author: impl Into<git_repository::actor::SignatureRef<'a>>,
        committer: impl Into<git_repository::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: git_repository::ObjectId,
        parent_ids: impl IntoIterator<Item = git_repository::ObjectId>,
        options: &CommitOptions<'_>,
    ) -> Result<git_repository::ObjectId> {
        let author = author.into();
        let committer = committer.into();
        let commit_encoding = match &options.commit_encoding {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s)
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
            // Use gitoxide for all other occasions
            let commit_id = self.write_object(&git_repository::objs::Commit {
                tree: tree_id,
                parents: parent_ids.into_iter().collect(),
                author: author.to_owned(),
                committer: committer.to_owned(),
                encoding: commit_encoding.map(|enc| enc.name().into()),
                message: message.raw_bytes().into(),
                extra_headers: vec![],
            })?;
            Ok(commit_id.detach())
        }
    }
}
