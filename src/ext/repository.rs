// SPDX-License-Identifier: GPL-2.0-only

use std::borrow::Cow;

use anyhow::{anyhow, Result};
use bstr::BStr;

use crate::{
    stupid::Stupid,
    wrap::{Branch, Message, PartialRefName},
};

/// Extends [`gix::Repository`] with additional methods.
pub(crate) trait RepositoryExtended {
    /// Open git repository based on current directory and any environment overrides.
    fn open() -> Result<gix::Repository> {
        Ok(gix::ThreadSafeRepository::discover_with_environment_overrides(".")?.into())
    }

    /// Determine whether the repository is in a clean state.
    ///
    /// A clean repository is not in the middle of any of a variety of stateful
    /// operations such as merge, rebase, cherry-pick, etc.; see
    /// [`gix::state::InProgress`].
    fn check_repository_state(&self) -> Result<()>;

    /// Get the author signature or error if it is unavailable.
    fn get_author(&self) -> Result<gix::actor::SignatureRef<'_>>;

    /// Get the committer signature or error if it is unavailable.
    fn get_committer(&self) -> Result<gix::actor::SignatureRef<'_>>;

    /// Get [`Branch`], with StGit-specific error messaging.
    ///
    /// Gets the current branch if the provided `branch_name` is `None`,
    fn get_branch(&self, branch_name: &PartialRefName) -> Result<Branch<'_>>;

    /// Get the current branch.
    ///
    /// Returns an error if the head is detached or unborn.
    fn get_current_branch(&self) -> Result<Branch<'_>>;

    /// Get repository-local config file which can be used to change local
    /// configuration.
    fn local_config_file(&self) -> Result<gix::config::File<'static>>;

    /// Write repository-local config file.
    fn write_local_config(&self, file: gix::config::File) -> Result<()>;

    /// Create a new commit object in the repository, with extended features.
    ///
    /// The extended features versus [`gix::Repository::commit()`] include:
    ///
    /// - Respecting `i18n.commitEncoding` for commit messages.
    /// - Respecting `commit.gpgSign` and creating signed commits when enabled.
    fn commit_ex<'a>(
        &self,
        author: impl Into<gix::actor::SignatureRef<'a>>,
        committer: impl Into<gix::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
    ) -> Result<gix::ObjectId>;

    /// Create a new commit object in the repository.
    ///
    /// The provided [`CommitOptions`] gives finer-grained control versus
    /// [`RepositoryExtended::commit_ex()`].
    fn commit_with_options<'a>(
        &self,
        author: impl Into<gix::actor::SignatureRef<'a>>,
        committer: impl Into<gix::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
        options: &CommitOptions<'_>,
    ) -> Result<gix::ObjectId>;

    /// [`gix::Repository::rev_parse_single()`] with StGit-specific error mapping.
    fn rev_parse_single_ex(&self, spec: &str) -> Result<gix::Id<'_>>;
}

/// Options for creating a git commit object.
pub(crate) struct CommitOptions<'a> {
    /// The target encoding for the commit message.
    pub(crate) commit_encoding: Option<Cow<'a, BStr>>,

    /// Determine whether the commit object should be signed with GPG.
    pub(crate) gpgsign: bool,
}

impl RepositoryExtended for gix::Repository {
    fn check_repository_state(&self) -> Result<()> {
        use gix::state::InProgress;
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
                "complete the in-progress `{state_str}` before trying again",
            ))
        } else {
            Ok(())
        }
    }

    fn get_author(&self) -> Result<gix::actor::SignatureRef<'_>> {
        Ok(self.author().ok_or_else(|| {
            anyhow!("author identity unknown; please configure `user.name` and `user.email`.")
        })??)
    }

    fn get_committer(&self) -> Result<gix::actor::SignatureRef<'_>> {
        Ok(self.committer().ok_or_else(|| {
            anyhow!("committer identity unknown; please configure `user.name` and `user.email`.")
        })??)
    }

    fn get_branch(&self, branch_name: &PartialRefName) -> Result<Branch<'_>> {
        use gix::refs::Category;

        let gix_name: gix::refs::PartialName = branch_name.into();
        let reference = self.find_reference(&gix_name).map_err(|e| match e {
            gix::reference::find::existing::Error::Find(inner) => {
                anyhow!("invalid branch name `{branch_name}`: {inner}")
            }
            gix::reference::find::existing::Error::NotFound { name } => {
                anyhow!("branch `{}` not found", name.as_ref().as_bstr())
            }
        })?;

        if matches!(reference.name().category(), Some(Category::LocalBranch),) {
            Ok(Branch::wrap(reference))
        } else {
            Err(anyhow!("reference `{branch_name}` is not a local branch"))
        }
    }

    fn get_current_branch(&self) -> Result<Branch<'_>> {
        use gix::{head::Kind, refs::Category};
        match self.head()?.kind {
            Kind::Symbolic(inner_reference) => {
                if matches!(inner_reference.name.category(), Some(Category::LocalBranch)) {
                    let reference = self
                        .find_reference(&inner_reference.name)
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
            Kind::Detached { .. } => Err(anyhow!("not on branch, HEAD is detached")),
        }
    }

    fn local_config_file(&self) -> Result<gix::config::File<'static>> {
        let source = gix::config::Source::Local;

        let local_config_path = self.common_dir().join(
            source
                .storage_location(&mut |n| std::env::var_os(n))
                .expect("know repo-local config path"),
        );

        Ok(gix::config::File::from_path_no_includes(
            local_config_path,
            source,
        )?)
    }

    fn write_local_config(&self, file: gix::config::File) -> Result<()> {
        let local_config_path = self.common_dir().join(
            gix::config::Source::Local
                .storage_location(&mut |n| std::env::var_os(n))
                .expect("know repo-local config path"),
        );

        file.write_to(
            &mut std::fs::File::options()
                .truncate(true)
                .write(true)
                .open(local_config_path)?,
        )?;
        Ok(())
    }

    fn commit_ex<'a>(
        &self,
        author: impl Into<gix::actor::SignatureRef<'a>>,
        committer: impl Into<gix::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
    ) -> Result<gix::ObjectId> {
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
        author: impl Into<gix::actor::SignatureRef<'a>>,
        committer: impl Into<gix::actor::SignatureRef<'a>>,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
        options: &CommitOptions<'_>,
    ) -> Result<gix::ObjectId> {
        let author = author.into();
        let committer = committer.into();
        let commit_encoding = match &options.commit_encoding {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s)
                    .ok_or_else(|| anyhow!("unhandled i18n.commitEncoding `{s}`"))?;
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
            let commit_id = self.write_object(&gix::objs::Commit {
                tree: tree_id,
                parents: parent_ids.into_iter().collect(),
                author: author.to_owned()?,
                committer: committer.to_owned()?,
                encoding: commit_encoding.map(|enc| enc.name().into()),
                message: message.raw_bytes().into(),
                extra_headers: vec![],
            })?;
            Ok(commit_id.detach())
        }
    }

    fn rev_parse_single_ex(&self, spec: &str) -> Result<gix::Id<'_>> {
        use gix::{
            refs::file::find::existing::Error as FindError,
            revision::spec::parse::{single::Error as SingleError, Error as SpecParseError},
        };

        use crate::patch::revspec::Error;

        // Catch revspec ranges early because gitoxide will only flag this as an error if
        // both specs in the range are valid.
        if spec.contains("..") {
            return Err(Error::InvalidRevision(
                spec.to_string(),
                "revspec must resolve to a single object".to_string(),
            )
            .into());
        }

        self.rev_parse_single(spec)
            .map_err(|single_err| -> anyhow::Error {
                match single_err {
                    SingleError::Parse(inner) => {
                        let mut spec_parse_err = &inner;
                        loop {
                            match spec_parse_err {
                                SpecParseError::FindReference(find_err) => match find_err {
                                    e @ FindError::Find(_) => {
                                        break Error::InvalidRevision(
                                            spec.to_string(),
                                            e.to_string(),
                                        )
                                        .into()
                                    }
                                    FindError::NotFound { name: _ } => {
                                        break Error::RevisionNotFound(spec.to_string()).into()
                                    }
                                },
                                SpecParseError::Multi { current, next } => {
                                    if let Some(next) = next {
                                        spec_parse_err = next
                                            .downcast_ref::<SpecParseError>()
                                            .expect("next error is SpecParseError");
                                    } else {
                                        spec_parse_err = current
                                            .downcast_ref::<SpecParseError>()
                                            .expect("current error is SpecParseError");
                                    }
                                }
                                SpecParseError::SingleNotFound => {
                                    break Error::RevisionNotFound(spec.to_string()).into()
                                }
                                e => {
                                    break Error::InvalidRevision(spec.to_string(), e.to_string())
                                        .into()
                                }
                            }
                        }
                    }
                    e @ SingleError::RangedRev { spec: _ } => {
                        Error::InvalidRevision(spec.to_string(), e.to_string()).into()
                    }
                }
            })
    }
}
