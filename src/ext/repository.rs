// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{anyhow, Result};
use bstr::{BString, ByteSlice};

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
    fn local_config_file(&self) -> Result<gix::config::File>;

    /// Write repository-local config file.
    fn write_local_config(&self, file: gix::config::File) -> Result<()>;

    /// Create a new commit object in the repository, with extended features.
    ///
    /// The extended features versus [`gix::Repository::commit()`] include:
    ///
    /// - Respecting `i18n.commitEncoding` for commit messages.
    /// - Respecting `commit.gpgSign` and creating signed commits when enabled.
    fn commit_ex(
        &self,
        author: &gix::actor::Signature,
        committer: &gix::actor::Signature,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
    ) -> Result<gix::ObjectId>;

    /// Create a new commit object in the repository.
    ///
    /// The provided [`CommitOptions`] gives finer-grained control versus
    /// [`RepositoryExtended::commit_ex()`].
    fn commit_with_options(
        &self,
        author: &gix::actor::Signature,
        committer: &gix::actor::Signature,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
        options: &CommitOptions,
    ) -> Result<gix::ObjectId>;

    /// [`gix::Repository::rev_parse_single()`] with StGit-specific error mapping.
    fn rev_parse_single_ex(&self, spec: &str) -> Result<gix::Id<'_>>;

    /// Get the comment symbol from git config.
    ///
    /// Returns the configured `core.commentChar` value, defaulting to "#" if not set.
    /// Git's default is '#' and it also supports 'auto' which we treat as '#'.
    /// Note: git allows this to be a string, not just a single character.
    fn get_comment_symbol(&self) -> String;

    /// Determine whether `git notes copy` might find a note to copy from `commit_id`.
    ///
    /// Returns `false` only when git would certainly copy nothing: the notes ref git
    /// would use is outside `refs/notes/`, does not exist (or is a dangling symbolic
    /// ref), or its tree has no entry that could be the note for `commit_id`. Any
    /// doubt (lookup errors, unsupported configuration sources, etc.) yields `true` so
    /// the caller defers to git.
    fn may_have_note(&self, commit_id: gix::ObjectId) -> bool;
}

/// Options for creating a git commit object.
pub(crate) struct CommitOptions {
    /// The target encoding for the commit message.
    pub(crate) commit_encoding: Option<BString>,

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
        let reference = self.find_reference(&gix_name).map_err(|e| {
            if e.is_not_found() {
                anyhow!("branch `{branch_name}` not found")
            } else {
                anyhow!("invalid branch name `{branch_name}`: {e}")
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

    fn local_config_file(&self) -> Result<gix::config::File> {
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

    fn commit_ex(
        &self,
        author: &gix::actor::Signature,
        committer: &gix::actor::Signature,
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

    fn commit_with_options(
        &self,
        author: &gix::actor::Signature,
        committer: &gix::actor::Signature,
        message: &Message,
        tree_id: gix::ObjectId,
        parent_ids: impl IntoIterator<Item = gix::ObjectId>,
        options: &CommitOptions,
    ) -> Result<gix::ObjectId> {
        let commit_encoding = match &options.commit_encoding {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s.as_ref())
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
                author: author.clone(),
                committer: committer.to_owned(),
                encoding: commit_encoding.map(|enc| enc.name().into()),
                message: message.raw_bytes().into(),
                extra_headers: vec![],
            })?;
            Ok(commit_id.detach())
        }
    }

    fn rev_parse_single_ex(&self, spec: &str) -> Result<gix::Id<'_>> {
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

        self.rev_parse_single(spec).map_err(|err| -> anyhow::Error {
            // Distinguish "the spec parsed but resolved to nothing" from "the spec
            // is malformed".
            if err.is_not_found() {
                Error::RevisionNotFound(spec.to_string()).into()
            } else {
                Error::InvalidRevision(spec.to_string(), err.to_string()).into()
            }
        })
    }

    fn get_comment_symbol(&self) -> String {
        let config = self.config_snapshot();
        config
            // fetch the comment symbol from the config
            .string("core.commentChar")
            // convert the value to a string and trim it
            .and_then(|value| value.to_str().ok().map(|s| s.trim().to_string()))
            // filter out empty values and "auto"
            .filter(|value_str| !value_str.is_empty() && value_str != "auto")
            // if no value is found, return "#"
            .unwrap_or("#".to_string())
    }

    fn may_have_note(&self, commit_id: gix::ObjectId) -> bool {
        // gix does not apply `git -c` config passed via GIT_CONFIG_PARAMETERS, which
        // git would honor when selecting the notes ref.
        if std::env::var_os("GIT_CONFIG_PARAMETERS").is_some_and(|v| !v.is_empty()) {
            return true;
        }

        // Select the notes ref the same way as git: GIT_NOTES_REF, even if empty, takes
        // precedence over core.notesRef. Neither is expanded if not fully qualified.
        let notes_ref: BString = if let Some(value) = std::env::var_os("GIT_NOTES_REF") {
            match gix::path::os_string_into_bstring(value) {
                Ok(value) => value,
                Err(_) => return true,
            }
        } else if let Some(value) = self.config_snapshot().string("core.notesRef") {
            value
        } else {
            BString::from("refs/notes/commits")
        };

        // git refuses to copy notes in refs outside of refs/notes/.
        if !notes_ref.starts_with(b"refs/notes/") {
            return false;
        }

        notes_ref_may_have_note(self, notes_ref.as_bstr(), commit_id).unwrap_or(true)
    }
}

/// Determine whether the tree of `notes_ref` might contain a note for `commit_id`.
fn notes_ref_may_have_note(
    repo: &gix::Repository,
    notes_ref: &bstr::BStr,
    commit_id: gix::ObjectId,
) -> Result<bool> {
    // Git's SYMREF_MAXDEPTH (5 ref reads) follows at most 4 symbolic hops. Following
    // up to 5 hops before deferring to git errs toward finding a note.
    const MAX_SYMREF_DEPTH: usize = 5;

    let name: &gix::refs::FullNameRef = notes_ref.try_into()?;
    let Some(mut reference) = repo.try_find_reference(name)? else {
        return Ok(false);
    };

    let mut depth = 0;
    let target_id = loop {
        match reference.target() {
            gix::refs::TargetRef::Object(id) => break id.to_owned(),
            gix::refs::TargetRef::Symbolic(target_name) => {
                depth += 1;
                if depth > MAX_SYMREF_DEPTH {
                    return Ok(true);
                }
                let target_name = target_name.to_owned();
                match repo.try_find_reference(target_name.as_ref())? {
                    Some(target) => reference = target,
                    // Dangling symbolic ref; git finds no notes.
                    None => return Ok(false),
                }
            }
        }
    };

    let tree_id = repo
        .find_object(target_id)?
        .peel_to_kind(gix::object::Kind::Tree)?
        .id;
    let hex = commit_id.to_hex().to_string();
    notes_tree_may_have_note(repo, tree_id, hex.as_bytes())
}

/// Search a notes tree for any entry that could be the note for the object whose
/// (remaining) hex name is `hex`.
///
/// Notes trees may be fanned out, i.e. the note for `abcdef...` may be at `abcdef...`,
/// `ab/cdef...`, `ab/cd/ef...`, etc. Matching is case-insensitive, like git's hex
/// parsing. Any full-length match is treated as a possible note regardless of mode, so
/// this errs toward `true`.
fn notes_tree_may_have_note(
    repo: &gix::Repository,
    tree_id: gix::ObjectId,
    hex: &[u8],
) -> Result<bool> {
    let tree = repo.find_tree(tree_id)?;
    for entry in tree.decode()?.entries {
        let name = entry.filename.as_bytes();
        if name.is_empty()
            || name.len() > hex.len()
            || !name.eq_ignore_ascii_case(&hex[..name.len()])
        {
            continue;
        }
        if name.len() == hex.len()
            || (entry.mode.is_tree()
                && notes_tree_may_have_note(repo, entry.oid.to_owned(), &hex[name.len()..])?)
        {
            return Ok(true);
        }
    }
    Ok(false)
}
