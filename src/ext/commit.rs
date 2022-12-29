// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{anyhow, Result};

use crate::wrap::Message;

/// Extension trait for [`git2::Commit`].
pub(crate) trait CommitExtended<'a> {
    /// Get author signature, strictly.
    ///
    /// The author signature, in an arbitrary git commit object, may (should? must?) be
    /// encoded with the commit encoding. However, libgit2 does not perform any decoding
    /// when it parses commit objects, thus a `git2::Signature` from a `git2::Commit` is
    /// not decoded in the general case, and thus `git2::Signature.name()` and `email()`
    /// are only available as decoded UTF-8 strs iff the commit happens to be using the
    /// UTF-8 encoding.
    ///
    /// This method takes into account the commit's encoding and attempts to decode the
    /// author name and email into UTF-8. The signature returned by this method is
    /// guaranteed to have valid UTF-8 name and email strs.
    fn author_strict(&self) -> Result<git2::Signature<'static>>;

    /// Get committer signature, strictly.
    ///
    /// See [`CommitExtended::author_strict()`].
    fn committer_strict(&self) -> Result<git2::Signature<'static>>;

    /// Get commit message with extended capabilities.
    fn message_ex(&self) -> Message;

    /// Determine whether the commit has the same tree as its parent.
    fn is_no_change(&self) -> Result<bool>;
}

impl<'a> CommitExtended<'a> for git2::Commit<'a> {
    fn author_strict(&self) -> Result<git2::Signature<'static>> {
        let sig = self.author();
        let encoding = if let Some(encoding_name) = self.message_encoding() {
            encoding_rs::Encoding::for_label(encoding_name.as_bytes()).ok_or_else(|| {
                anyhow!(
                    "Unhandled commit encoding `{}` in commit `{}`",
                    encoding_name,
                    self.id()
                )
            })?
        } else {
            encoding_rs::UTF_8
        };

        if let Some(name) =
            encoding.decode_without_bom_handling_and_without_replacement(sig.name_bytes())
        {
            if let Some(email) =
                encoding.decode_without_bom_handling_and_without_replacement(sig.email_bytes())
            {
                Ok(git2::Signature::new(&name, &email, &sig.when())?)
            } else {
                Err(anyhow!(
                    "Could not decode author email as `{}` for commit `{}`",
                    encoding.name(),
                    self.id(),
                ))
            }
        } else {
            Err(anyhow!(
                "Could not decode author name as `{}` for commit `{}`",
                encoding.name(),
                self.id(),
            ))
        }
    }

    fn committer_strict(&self) -> Result<git2::Signature<'static>> {
        let sig = self.committer();
        let encoding = if let Some(encoding_name) = self.message_encoding() {
            encoding_rs::Encoding::for_label(encoding_name.as_bytes()).ok_or_else(|| {
                anyhow!(
                    "Unhandled commit encoding `{}` in commit `{}`",
                    encoding_name,
                    self.id()
                )
            })?
        } else {
            encoding_rs::UTF_8
        };

        if let Some(name) =
            encoding.decode_without_bom_handling_and_without_replacement(sig.name_bytes())
        {
            if let Some(email) =
                encoding.decode_without_bom_handling_and_without_replacement(sig.email_bytes())
            {
                Ok(git2::Signature::new(&name, &email, &sig.when())?)
            } else {
                Err(anyhow!(
                    "Could not decode committer email as `{}` for commit `{}`",
                    encoding.name(),
                    self.id(),
                ))
            }
        } else {
            Err(anyhow!(
                "Could not decode committer name as `{}` for commit `{}`",
                encoding.name(),
                self.id(),
            ))
        }
    }

    fn message_ex(&self) -> Message {
        if let Some(message) = self.message_raw() {
            Message::Str(message)
        } else {
            Message::Raw {
                bytes: self.message_raw_bytes(),
                encoding: self.message_encoding(),
            }
        }
    }

    fn is_no_change(&self) -> Result<bool> {
        let no_change = self.parent_count() == 1 && self.parent(0)?.tree_id() == self.tree_id();
        Ok(no_change)
    }
}
