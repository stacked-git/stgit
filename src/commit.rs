use std::borrow::Cow;

use crate::{error::Error, stupid};

pub(crate) enum CommitMessage<'a> {
    Str(&'a str),
    String(String),
    Raw {
        bytes: &'a [u8],
        encoding: Option<&'a str>,
    },
}

impl<'a> CommitMessage<'a> {
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            CommitMessage::Str(s) => s.is_empty(),
            CommitMessage::String(s) => s.is_empty(),
            CommitMessage::Raw { bytes, encoding: _ } => bytes.is_empty(),
        }
    }

    pub(crate) fn encoding(&self) -> Result<&'static encoding_rs::Encoding, Error> {
        match self {
            Self::Str(_) => Ok(encoding_rs::UTF_8),
            Self::String(_) => Ok(encoding_rs::UTF_8),
            Self::Raw { bytes: _, encoding } => {
                if let Some(encoding_str) = encoding {
                    let encoding = encoding_rs::Encoding::for_label(encoding_str.as_bytes())
                        .ok_or_else(|| {
                            Error::Generic(format!("Unhandled message encoding `{}`", encoding_str))
                        })?;
                    if encoding.is_single_byte() {
                        Ok(encoding)
                    } else {
                        Err(Error::Generic(format!(
                            "Message encoding `{}` is not single-byte",
                            encoding.name()
                        )))
                    }
                } else {
                    // Fallback to latin1.
                    // TODO: Is this a terrible idea?
                    // TODO: Does using this encoding give a different outcome than using
                    // encoding_rs::mem::decode_latin1(bytes)?
                    Ok(encoding_rs::Encoding::for_label(b"iso-8859-1").unwrap())
                }
            }
        }
    }

    pub(crate) fn decode(&'a self) -> Result<Cow<'a, str>, Error> {
        match self {
            Self::Str(s) => Ok(Cow::Borrowed(s)),
            Self::String(s) => Ok(Cow::Borrowed(s)),
            Self::Raw { bytes, encoding: _ } => {
                let encoding = self.encoding()?;
                if let Some(message) =
                    encoding.decode_without_bom_handling_and_without_replacement(bytes)
                {
                    Ok(message)
                } else {
                    Err(Error::Generic(format!(
                        "Replacements while decoding message with `{}`",
                        encoding.name()
                    )))
                }
            }
        }
    }

    pub(crate) fn encode_with(
        &'a self,
        target_encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<Cow<'a, [u8]>, Error> {
        let is_encoding_specified = target_encoding.is_some();
        let target_encoding = target_encoding.unwrap_or(encoding_rs::UTF_8);
        match self {
            CommitMessage::Str(s) => {
                if target_encoding == encoding_rs::UTF_8 {
                    Ok(Cow::Borrowed(s.as_bytes()))
                } else {
                    let (encoded, _actual_encoding, any_replacements) = target_encoding.encode(s);

                    if any_replacements {
                        Err(Error::Generic(format!(
                            "Failed to encode commit message with `{}`",
                            target_encoding.name(),
                        )))
                    } else {
                        Ok(encoded)
                    }
                }
            }
            CommitMessage::String(s) => {
                if target_encoding == encoding_rs::UTF_8 {
                    Ok(Cow::Borrowed(s.as_bytes()))
                } else {
                    let (encoded, _actual_encoding, any_replacements) = target_encoding.encode(s);

                    if any_replacements {
                        Err(Error::Generic(format!(
                            "Failed to encode commit message with `{}`",
                            target_encoding.name(),
                        )))
                    } else {
                        Ok(encoded)
                    }
                }
            }
            CommitMessage::Raw { bytes, encoding } => {
                if let Some(encoding_str) = encoding {
                    if let Some(current_encoding) =
                        encoding_rs::Encoding::for_label(encoding_str.as_bytes())
                    {
                        if current_encoding == target_encoding {
                            Ok(Cow::Borrowed(bytes))
                        } else if let Some(message) = current_encoding
                            .decode_without_bom_handling_and_without_replacement(bytes)
                        {
                            let (encoded, _actual_encoding, any_replacements) =
                                target_encoding.encode(&message);
                            if any_replacements {
                                Err(Error::Generic(format!(
                                    "Failed to reencode commit message from `{}` to `{}`",
                                    current_encoding.name(),
                                    target_encoding.name(),
                                )))
                            } else {
                                Ok(Cow::Owned(encoded.to_vec()))
                            }
                        } else {
                            Err(Error::Generic(format!(
                                "Failed to decode commit message with `{}`",
                                current_encoding.name(),
                            )))
                        }
                    } else {
                        Err(Error::Generic(format!(
                            "Unhandled commit message encoding `{}`",
                            encoding_str
                        )))
                    }
                } else if !is_encoding_specified {
                    // No current encoding.
                    //
                    // No target encoding either. Assume current mystery encoding is
                    // same as target mystery encoding.
                    //
                    // This CommitMessage comes from a commit that both (a) did not have
                    // an explicit "encoding" header; and (b) did not cleanly decode as
                    // utf-8.
                    Ok(Cow::Borrowed(bytes))
                } else {
                    // No current encoding, but have a target encoding.
                    // Guess that current encoding is same as target encoding.
                    if target_encoding
                        .decode_without_bom_handling_and_without_replacement(bytes)
                        .is_some()
                    {
                        Ok(Cow::Borrowed(bytes))
                    } else {
                        Err(Error::Generic(format!(
                            "Cannot encode commit message with unknown encoding to `{}`",
                            target_encoding.name(),
                        )))
                    }
                }
            }
        }
    }

    // pub(crate) fn encode(&'a self, label: Option<&str>) -> Result<Cow<'a, [u8]>, Error> {
    //     let target_encoding = if let Some(label) = label {
    //         if let Some(encoding) = encoding_rs::Encoding::for_label(label.as_bytes()) {
    //             Some(encoding)
    //         } else {
    //             return Err(Error::Generic(format!(
    //                 "Unhandled commit message encoding `{}`",
    //                 label
    //             )));
    //         }
    //     } else {
    //         None
    //     };
    //     self.encode_with(target_encoding)
    // }

    pub(crate) fn raw_bytes(&self) -> &[u8] {
        match self {
            Self::Str(s) => s.as_bytes(),
            Self::String(s) => s.as_bytes(),
            Self::Raw { bytes, encoding: _ } => *bytes,
        }
    }
}

impl<'a> From<&'a str> for CommitMessage<'a> {
    fn from(s: &'a str) -> Self {
        Self::Str(s)
    }
}

impl<'a> From<String> for CommitMessage<'a> {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl<'a> Default for CommitMessage<'a> {
    fn default() -> Self {
        Self::Str("")
    }
}

impl<'a> PartialEq for CommitMessage<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_bytes() == other.raw_bytes()
    }
}

pub(crate) trait CommitExtended<'a> {
    fn author_strict(&self) -> Result<git2::Signature<'static>, Error>;
    fn committer_strict(&self) -> Result<git2::Signature<'static>, Error>;
    fn message_ex(&self) -> CommitMessage;
}

impl<'a> CommitExtended<'a> for git2::Commit<'a> {
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
    fn author_strict(&self) -> Result<git2::Signature<'static>, Error> {
        let sig = self.author();
        let encoding = if let Some(encoding_name) = self.message_encoding() {
            encoding_rs::Encoding::for_label(encoding_name.as_bytes()).ok_or_else(|| {
                Error::Generic(format!(
                    "Unhandled commit encoding `{}` in commit `{}`",
                    encoding_name,
                    self.id()
                ))
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
                Err(Error::Generic(format!(
                    "could not decode author email as `{}` for commit `{}`",
                    encoding.name(),
                    self.id(),
                )))
            }
        } else {
            Err(Error::Generic(format!(
                "could not decode author name as `{}` for commit `{}`",
                encoding.name(),
                self.id(),
            )))
        }
    }

    /// Get committer signature, strictly.
    fn committer_strict(&self) -> Result<git2::Signature<'static>, Error> {
        let sig = self.committer();
        let encoding = if let Some(encoding_name) = self.message_encoding() {
            encoding_rs::Encoding::for_label(encoding_name.as_bytes()).ok_or_else(|| {
                Error::Generic(format!(
                    "Unhandled commit encoding `{}` in commit `{}`",
                    encoding_name,
                    self.id()
                ))
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
                Err(Error::Generic(format!(
                    "could not decode committer email as `{}` for commit `{}`",
                    encoding.name(),
                    self.id(),
                )))
            }
        } else {
            Err(Error::Generic(format!(
                "could not decode committer name as `{}` for commit `{}`",
                encoding.name(),
                self.id(),
            )))
        }
    }

    fn message_ex(&self) -> CommitMessage {
        if let Some(message) = self.message_raw() {
            CommitMessage::Str(message)
        } else {
            CommitMessage::Raw {
                bytes: self.message_raw_bytes(),
                encoding: self.message_encoding(),
            }
        }
    }
}

pub(crate) struct CommitOptions<'a> {
    pub(crate) commit_encoding: Option<&'a str>,
    pub(crate) gpgsign: bool,
}

pub(crate) trait RepositoryCommitExtended {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error>;

    fn commit_with_options(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
        options: &CommitOptions,
    ) -> Result<git2::Oid, Error>;
}

impl RepositoryCommitExtended for git2::Repository {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error> {
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
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
        options: &CommitOptions<'_>,
    ) -> Result<git2::Oid, Error> {
        let commit_encoding = match options.commit_encoding {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s.as_bytes()).ok_or_else(|| {
                    Error::Generic(format!("Unhandled i18n.commitEncoding `{}`", s))
                })?;
                Some(encoding)
            }
            None => None,
        };

        if options.gpgsign
            || (commit_encoding.is_some() && commit_encoding != Some(encoding_rs::UTF_8))
        {
            // Use git for any commit that needs to be signed
            stupid::commit_tree(
                self.path(),
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
