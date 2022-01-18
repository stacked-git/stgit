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

    pub(crate) fn encode(&'a self, label: Option<&str>) -> Result<Cow<'a, [u8]>, Error> {
        let target_encoding = if let Some(label) = label {
            if let Some(encoding) = encoding_rs::Encoding::for_label(label.as_bytes()) {
                encoding
            } else {
                return Err(Error::Generic(format!(
                    "Unhandled commit message encoding `{}`",
                    label
                )));
            }
        } else {
            encoding_rs::UTF_8
        };

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
                } else if label.is_none() {
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

pub(crate) trait CommitMessageExtended<'a> {
    fn message_ex(&self) -> CommitMessage;
}

impl<'a> CommitMessageExtended<'a> for git2::Commit<'a> {
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

pub(crate) trait CommitExtended {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error>;
}

impl CommitExtended for git2::Repository {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &CommitMessage,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error> {
        let config = self.config()?;
        let commit_encoding_str = config.get_string("i18n.commitencoding").ok();
        let commit_encoding_str = commit_encoding_str.as_deref();
        let commit_encoding = match commit_encoding_str {
            Some(s) => {
                let encoding = encoding_rs::Encoding::for_label(s.as_bytes()).ok_or_else(|| {
                    Error::Generic(format!("Unhandled i18n.commitEncoding `{}`", s))
                })?;
                encoding
            }
            None => encoding_rs::UTF_8,
        };

        let gpgsign = config.get_bool("commit.gpgsign").unwrap_or(false);

        if gpgsign || commit_encoding != encoding_rs::UTF_8 {
            // Use git for any commit that needs to be signed
            stupid::commit_tree(
                self.path(),
                author,
                committer,
                &message.encode(commit_encoding_str)?,
                tree_id,
                parent_ids,
                gpgsign,
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
