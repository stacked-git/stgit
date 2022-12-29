// SPDX-License-Identifier: GPL-2.0-only

use std::borrow::Cow;

use anyhow::{anyhow, Result};

/// Enum for handling commit messages in various forms.
///
/// Git does not enforce commit message encodings. Although commit messages are
/// typically UTF-8 encoded, git permits any "extended ASCII" encoding. Furthermore,
/// while commit objects may have an optional encoding header that specifies the commit
/// message encoding, that header is not always present or correct in commits found in
/// the wild. And yet another layer of complication emerges due to [`git2`] (and the
/// underlying `libgit2`) not having any capability of its own to decode, encode, or
/// re-encode commit messages. With [`git2`], it entirely up to the application to make
/// sense of the raw commit message bytes when any non-UTF-8 encoding comes into play.
///
/// StGit aims to always create commit objects will correct/correctly-identified
/// encodings. In the easy cases, UTF-8 encoded commit messages map cleanly to/from Rust
/// `str`s and `String`s. But there remain several edge and otherwise not so easy cases
/// to accommodate, including:
///
/// - Decoding commit messages with explicit non-UTF-8 encodings.
/// - Decoding commit messages that are neither valid UTF-8 nor have an explicit
///   encoding.
/// - Encoding commit messages based on the user's `i18n.commitEncoding` configuration.
///
/// This enum's `&str`, [`String`], and raw byte slice (`&[u8]`) variants are meant to
/// cover all relevant commit message forms to handle both easy and not-so-easy cases.
pub(crate) enum Message<'a> {
    Str(&'a str),
    String(String),
    Raw {
        bytes: &'a [u8],
        encoding: Option<&'a str>,
    },
}

impl<'a> Message<'a> {
    /// Determine whether the commit message has any content.
    ///
    /// For the [`Message::Raw`] variant, emptiness is determined simply by
    /// the presence or absensce of bytes, independent of the nominal encoding.
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Message::Str(s) => s.is_empty(),
            Message::String(s) => s.is_empty(),
            Message::Raw { bytes, encoding: _ } => bytes.is_empty(),
        }
    }

    /// Get the commit message's [`encoding_rs::Encoding`].
    pub(crate) fn encoding(&self) -> Result<&'static encoding_rs::Encoding> {
        match self {
            Self::Str(_) | Self::String(_) => Ok(encoding_rs::UTF_8),
            Self::Raw { bytes: _, encoding } => {
                if let Some(encoding_str) = encoding {
                    let encoding = encoding_rs::Encoding::for_label(encoding_str.as_bytes())
                        .ok_or_else(|| anyhow!("Unhandled message encoding `{encoding_str}`"))?;
                    if encoding.is_single_byte() {
                        Ok(encoding)
                    } else {
                        Err(anyhow!(
                            "Message encoding `{}` is not single-byte",
                            encoding.name()
                        ))
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

    /// Decode commit message to Rust-native UTF-8 [`str`].
    pub(crate) fn decode(&'a self) -> Result<Cow<'a, str>> {
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
                    Err(anyhow!(
                        "Replacements while decoding message with `{}`",
                        encoding.name()
                    ))
                }
            }
        }
    }

    /// Encode commit message with target encoding.
    ///
    /// If the provided `target_encoding` is `None`, UTF-8 is used.
    ///
    /// In cases where the current encoding matches the target encoding, no decoding or
    /// encoding is actually performed. This both saves compute cycles and, perhaps more
    /// importantly, may allow commit objects with mal-encoded messages to be handled by
    /// StGit as long as no modifications to the message are required. In other words,
    /// by deferring/avoiding decoding/recoding commit messages, StGit may be able to
    /// get its job done without realizing underlying encoding errors in commits it has
    /// to deal with.
    pub(crate) fn encode_with(
        &'a self,
        target_encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<Cow<'a, [u8]>> {
        let is_encoding_specified = target_encoding.is_some();
        let target_encoding = target_encoding.unwrap_or(encoding_rs::UTF_8);
        match self {
            Message::Str(s) => {
                if target_encoding == encoding_rs::UTF_8 {
                    Ok(Cow::Borrowed(s.as_bytes()))
                } else {
                    let (encoded, _actual_encoding, any_replacements) = target_encoding.encode(s);

                    if any_replacements {
                        Err(anyhow!(
                            "Failed to encode commit message with `{}`",
                            target_encoding.name(),
                        ))
                    } else {
                        Ok(encoded)
                    }
                }
            }
            Message::String(s) => {
                if target_encoding == encoding_rs::UTF_8 {
                    Ok(Cow::Borrowed(s.as_bytes()))
                } else {
                    let (encoded, _actual_encoding, any_replacements) = target_encoding.encode(s);

                    if any_replacements {
                        Err(anyhow!(
                            "Failed to encode commit message with `{}`",
                            target_encoding.name(),
                        ))
                    } else {
                        Ok(encoded)
                    }
                }
            }
            Message::Raw { bytes, encoding } => {
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
                                Err(anyhow!(
                                    "Failed to re-encode commit message from `{}` to `{}`",
                                    current_encoding.name(),
                                    target_encoding.name(),
                                ))
                            } else {
                                Ok(Cow::Owned(encoded.to_vec()))
                            }
                        } else {
                            Err(anyhow!(
                                "Failed to decode commit message with `{}`",
                                current_encoding.name(),
                            ))
                        }
                    } else {
                        Err(anyhow!(
                            "Unhandled commit message encoding `{encoding_str}`"
                        ))
                    }
                } else if !is_encoding_specified {
                    // No current encoding.
                    //
                    // No target encoding either. Assume current mystery encoding is
                    // same as target mystery encoding.
                    //
                    // This Message comes from a commit that both (a) did not have an
                    // explicit "encoding" header; and (b) did not cleanly decode as
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
                        Err(anyhow!(
                            "Cannot encode commit message with unknown encoding to `{}`",
                            target_encoding.name(),
                        ))
                    }
                }
            }
        }
    }

    // pub(crate) fn encode(&'a self, label: Option<&str>) -> Result<Cow<'a, [u8]>> {
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

    /// Get commit message as raw byte slice in the message's nominal encoding.
    pub(crate) fn raw_bytes(&self) -> &[u8] {
        match self {
            Self::Str(s) => s.as_bytes(),
            Self::String(s) => s.as_bytes(),
            Self::Raw { bytes, encoding: _ } => bytes,
        }
    }
}

impl<'a> From<&'a str> for Message<'a> {
    fn from(s: &'a str) -> Self {
        Self::Str(s)
    }
}

impl<'a> From<String> for Message<'a> {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl<'a> Default for Message<'a> {
    fn default() -> Self {
        Self::Str("")
    }
}

impl<'a> PartialEq for Message<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_bytes() == other.raw_bytes()
    }
}
