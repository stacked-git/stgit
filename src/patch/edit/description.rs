// SPDX-License-Identifier: GPL-2.0-only

//! Interactively editable patch description format.

use std::io::Write;

use anyhow::{anyhow, Context, Result};
use bstr::{BStr, BString, ByteSlice};

use crate::{ext::TimeExtended, patch::PatchName};

const CUT_LINE: &str = "------------------------ >8 ------------------------";

#[derive(Clone, PartialEq, Eq)]
pub(super) struct DiffBuffer(pub(super) BString);

impl AsRef<[u8]> for DiffBuffer {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

/// Patch details presented to user when interactively editing a patch.
///
/// After the user edits the patch description file, the details are read back into the
/// complementary [`EditedPatchDescription`] struct.
#[derive(Default)]
pub(super) struct EditablePatchDescription {
    /// Patch name.
    ///
    /// Should be the original or default patch name when setting up an interactive
    /// edit, but may be `None` after interactive edit (i.e. if the user removes the
    /// name or the entire `Patch:` header).
    pub patchname: Option<PatchName>,

    /// Patch author.
    ///
    /// Should be setup with the existing or default author. May be `None` after
    /// interactive edit if the user removes the `Author:` header.
    pub author: Option<gix::actor::Signature>,

    /// Patch commit message.
    ///
    /// May be blank/empty before and/or after edit.
    pub message: String,

    /// Instruction string presented to the user in the editable patch description file.
    ///
    /// Will be `None` when the description is read-back after user edits.
    pub instruction: Option<String>,

    /// Instructions for user regarding what can/cannot be done with the diff.
    ///
    /// This instruction is only needed/presented when the optional diff is provided.
    /// This field will be `None` when the description is read-back after user edits.
    pub diff_instruction: Option<String>,

    /// Optional diff to present to the user in the editable patch description.
    ///
    /// Unlike all the other fields, the diff *does not* have to be valid UTF-8.
    pub diff: Option<DiffBuffer>,

    /// Comment symbol to use for comment lines in the patch description.
    ///
    /// This is typically read from git's `core.commentChar` config.
    pub comment_symbol: String,
}

impl EditablePatchDescription {
    /// Write user-editable patch description to the provided stream with custom comment string.
    pub(super) fn write_with_comment_symbol<S: Write>(
        &self,
        stream: &mut S,
        comment_symbol: &str,
    ) -> Result<()> {
        let patchname = if let Some(patchname) = &self.patchname {
            patchname.as_ref()
        } else {
            ""
        };

        writeln!(stream, "Patch:  {patchname}")?;
        if let Some(author) = self.author.as_ref() {
            let authdate = author.time.format(gix::date::time::format::ISO8601);
            write!(stream, "Author: ")?;
            stream.write_all(author.name.as_bstr())?;
            write!(stream, " <")?;
            stream.write_all(author.email.as_bstr())?;
            write!(stream, ">\nDate:   {authdate}\n",)?;
        } else {
            writeln!(stream, "Author: ")?;
            writeln!(stream, "Date:   ")?;
        }
        let message = self.message.trim_end_matches('\n');
        write!(stream, "\n{message}\n")?;
        if let Some(instruction) = &self.instruction {
            writeln!(stream)?;
            for line in instruction.lines() {
                writeln!(stream, "{} {}", comment_symbol, line)?;
            }
        } else {
            writeln!(stream)?;
        }
        if let Some(diff) = self.diff.as_ref() {
            if let Some(diff_instruction) = &self.diff_instruction {
                for line in diff_instruction.lines() {
                    writeln!(stream, "{} {}", comment_symbol, line)?;
                }
            }
            writeln!(stream, "{} {}", comment_symbol, CUT_LINE)?;
            writeln!(
                stream,
                "{} Do not modify or remove the line above.",
                comment_symbol
            )?;
            stream.write_all(diff.as_ref())?;
        }
        Ok(())
    }

    /// Write user-editable patch description to the provided stream.
    ///
    /// Uses the comment symbol stored in the struct.
    pub(super) fn write<S: Write>(&self, stream: &mut S) -> Result<()> {
        self.write_with_comment_symbol(stream, &self.comment_symbol)
    }
}

/// Patch details read-back after the user interactively edits a
/// [`EditablePatchDescription`].
#[derive(Default)]
pub(super) struct EditedPatchDescription {
    /// Patch name.
    ///
    /// The outer option indicates whether the "Patch:" header was present in the
    /// user-edited buffer.
    ///
    /// | `Patch:` header    | `patchname` value       |
    /// |--------------------|-------------------------|
    /// | `Patch: name`      | `Some(Some(patchname))` |
    /// | `Patch: `          | `Some(None)`            |
    /// | absent             | `None`                  |
    pub patchname: Option<Option<PatchName>>,

    /// Patch author.
    ///
    /// The outer option indicates whether the "Author:" header was present in the
    /// user-edited buffer.
    ///
    /// | `Author:` header      | `author` value          |
    /// |-----------------------|-------------------------|
    /// | `Author: name <mail>` | `Some(Some(signature))` |
    /// | `Author: `            | `Some(None)`            |
    /// | absent                | `None`                  |
    pub author: Option<Option<gix::actor::Signature>>,

    /// Patch commit message.
    ///
    /// May be blank/empty after edit.
    pub message: String,

    /// Optional diff.
    ///
    /// This diff may have been modified by the user.
    ///
    /// Unlike all the other fields, the diff *does not* have to be valid UTF-8.
    pub diff: Option<DiffBuffer>,
}

impl EditedPatchDescription {
    /// Attempt to parse user-edited patch description.
    ///
    /// Any lines starting with '#' (or the comment_symbol) are treated as comments
    /// and discarded, except for the cut line which separates the headers and
    /// message from the diff content.
    ///
    /// The "Patch", "Author", and "Date" headers, if present, must be the first
    /// three lines of the message. This rigidity is done to allow the message,
    /// which follows these headers, to potentially contain strings such as
    /// "Patch:".
    ///
    /// If all headers are absent and the trimmed message is empty, an error is
    /// returned. Blanking-out the headers and message is thus a mechanism for the
    /// user to abort the interactive edit.
    pub(super) fn parse_with_comment_symbol(buf: &[u8], comment_symbol: &str) -> Result<Self> {
        Self::parse_internal(buf, comment_symbol)
    }

    fn parse_internal(buf: &[u8], comment_symbol: &str) -> Result<Self> {
        let comment_bytes = comment_symbol.as_bytes();
        let cut_line = format!("{} {}", comment_symbol, CUT_LINE);
        let cut_line_bytes = cut_line.as_bytes();
        let mut raw_patchname: Option<Option<String>> = None;
        let mut raw_author: Option<Option<String>> = None;
        let mut raw_authdate: Option<Option<String>> = None;
        let mut consume_diff: bool = false;
        let mut consuming_message: bool = false;
        let mut consecutive_empty: usize = 0;
        let mut message = String::new();
        let mut pos: usize = 0;

        for (line_num, line) in buf
            .split_inclusive(|&b| b == b'\n')
            .map(BStr::new)
            .enumerate()
        {
            pos += line.len();
            if line.starts_with(cut_line_bytes) {
                consume_diff = true;
                break;
            } else if line.starts_with(comment_bytes) {
                continue;
            }

            // Every line before the diff must be valid utf8
            let line = line
                .to_str()
                .map_err(|_| anyhow!("patch description contains non-UTF-8 data"))?;
            let trimmed = line.trim_end();

            if consuming_message {
                if trimmed.is_empty() {
                    if consecutive_empty == 0 {
                        message.push('\n');
                    }
                    consecutive_empty += 1;
                } else {
                    consecutive_empty = 0;
                    message.push_str(trimmed);
                    message.push('\n');
                }
            } else {
                if let Some((key, value)) = trimmed.split_once(':') {
                    let raw_value = value.trim();
                    if line_num == 0 && key == "Patch" {
                        raw_patchname = Some(if raw_value.is_empty() {
                            None
                        } else {
                            Some(raw_value.to_string())
                        });
                        continue;
                    } else if line_num == 1 && key == "Author" {
                        raw_author = Some(if raw_value.is_empty() {
                            None
                        } else {
                            Some(raw_value.to_string())
                        });
                        continue;
                    } else if line_num == 2 && key == "Date" {
                        raw_authdate = Some(if raw_value.is_empty() {
                            None
                        } else {
                            Some(raw_value.to_string())
                        });
                        continue;
                    }
                }

                // Swallow a single blank line after the headers
                if trimmed.is_empty() {
                    consecutive_empty += 1;
                } else {
                    message.push_str(trimmed);
                    message.push('\n');
                }
                consuming_message = true;
            }
        }

        if raw_patchname.is_none()
            && raw_author.is_none()
            && raw_authdate.is_none()
            && message.trim().is_empty()
        {
            return Err(anyhow!("aborting due to empty patch description"));
        }

        let patchname = if let Some(maybe_patchname) = raw_patchname {
            Some(if let Some(patchname) = maybe_patchname {
                Some(patchname.parse::<PatchName>()?)
            } else {
                None
            })
        } else {
            None
        };

        let author = if let Some(maybe_author) = raw_author {
            Some(if let Some(name_email) = maybe_author {
                let (name, email) = super::parse::parse_name_email(&name_email)?;
                let time = if let Some(Some(date_str)) = raw_authdate {
                    gix::date::Time::parse_time(&date_str).context("parsing author date")?
                } else {
                    gix::date::Time::now_local_or_utc()
                };
                Some(gix::actor::Signature {
                    name: BString::from(name),
                    email: BString::from(email),
                    time,
                })
            } else {
                None
            })
        } else {
            None
        };

        if message.ends_with("\n\n") {
            message.pop();
        }

        if message.trim().is_empty() {
            message.clear();
        }

        let diff = if consume_diff {
            // Skip any comment lines after the cut line.
            for line in buf[pos..].split_inclusive(|&b| b == b'\n') {
                if line.starts_with(comment_bytes) {
                    pos += line.len();
                } else {
                    break;
                }
            }

            let diff_slice = buf[pos..].as_bstr();
            if diff_slice.iter().all(u8::is_ascii_whitespace) {
                None
            } else {
                Some(DiffBuffer(diff_slice.to_owned()))
            }
        } else {
            None
        };

        Ok(Self {
            patchname,
            author,
            message,
            diff,
        })
    }
}

#[cfg(test)]
mod tests {
    use bstr::ByteSlice;

    use super::*;

    fn compare_patch_descs(edited: &EditedPatchDescription, editable: &EditablePatchDescription) {
        if let Some(edited_patchname) = edited.patchname.as_ref() {
            assert_eq!(edited_patchname.as_ref(), editable.patchname.as_ref());
        } else {
            assert!(editable.patchname.is_none());
        }

        if let Some(maybe_author0) = edited.author.as_ref() {
            if let Some(author0) = maybe_author0.as_ref() {
                let author1 = editable.author.as_ref().expect("should also be some");
                assert_eq!(author0.name, author1.name);
                assert_eq!(author0.email, author1.email);
                assert_eq!(author0.time.seconds, author1.time.seconds);
                assert_eq!(author0.time.offset, author1.time.offset,);
            } else {
                assert!(editable.author.is_none());
            }
        } else {
            assert!(editable.author.is_none());
        }

        assert_eq!(edited.message, editable.message);
        if let (Some(diff0), Some(diff1)) = (&edited.diff, &editable.diff) {
            assert_eq!(diff0.0.to_str().unwrap(), diff1.0.to_str().unwrap(),);
        } else {
            assert!(
                edited.diff == editable.diff,
                "diffs differ edited is {} editable is {}",
                if edited.diff.is_some() {
                    "Some"
                } else {
                    "None"
                },
                if editable.diff.is_some() {
                    "Some"
                } else {
                    "None"
                },
            );
        }
    }

    fn compare_edited_descs(desc0: &EditedPatchDescription, desc1: &EditedPatchDescription) {
        assert_eq!(
            desc0.patchname.as_ref().as_ref(),
            desc1.patchname.as_ref().as_ref()
        );

        if let (Some(Some(author0)), Some(Some(author1))) = (
            desc0.author.as_ref().as_ref(),
            desc1.author.as_ref().as_ref(),
        ) {
            assert_eq!(author0.name, author1.name);
            assert_eq!(author0.email, author1.email);
            assert_eq!(author0.time.seconds, author1.time.seconds);
            assert_eq!(author0.time.offset, author1.time.offset);
        } else if let Some(None) = desc0.author.as_ref() {
            assert!(desc1.author.as_ref().unwrap().is_none());
        } else {
            assert!(desc1.author.is_none());
        }

        assert_eq!(desc0.message, desc1.message);
        if let (Some(diff0), Some(diff1)) = (desc0.diff.as_ref(), desc1.diff.as_ref()) {
            assert_eq!(diff0.0.to_str(), diff1.0.to_str());
        } else {
            assert!(
                desc0.diff == desc1.diff,
                "diffs differ desc0 is {} desc1 is {}",
                if desc0.diff.is_some() { "Some" } else { "None" },
                if desc1.diff.is_some() { "Some" } else { "None" },
            );
        }
    }

    #[test]
    fn round_trip_no_message_no_diff() {
        let editable = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, -3600),
            }),
            message: "".to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: None,
            diff: None,
            comment_symbol: "#".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        editable.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 03:25:21 -0100\n\
             \n\
             \n\
             \n\
             # Instruction\n",
        );

        let edited =
            EditedPatchDescription::parse_with_comment_symbol(buf.as_slice(), "#").unwrap();

        compare_patch_descs(&edited, &editable);
    }

    #[test]
    fn round_trip_one_line_message() {
        let patch_desc = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            }),
            message: "Subject\n".to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: None,
            diff: None,
            comment_symbol: "#".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             # Instruction\n",
        );

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(buf.as_slice(), "#").unwrap();

        compare_patch_descs(&edited_desc, &patch_desc);
    }

    #[test]
    fn round_trip_multi_line_message() {
        let patch_desc = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            }),
            message: "Subject\n\
                      \n\
                      Body of message.\n\
                      More body of message.\n\
                      \n\
                      With-a-trailer: yes\n\
                      "
            .to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: None,
            diff: None,
            comment_symbol: "#".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             Body of message.\n\
             More body of message.\n\
             \n\
             With-a-trailer: yes\n\
             \n\
             # Instruction\n",
        );

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(buf.as_slice(), "#").unwrap();

        compare_patch_descs(&edited_desc, &patch_desc);
    }

    #[test]
    fn with_diff() {
        let pd = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            }),
            message: "Subject\n".to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: Some("Diff instruction\n".to_string()),
            diff: Some(DiffBuffer(BString::from(
                "\n\
                 Some stuff before first diff --git\n\
                 \n\
                 diff --git a/foo.txt b/foo.txt\n\
                 index ce01362..a21e91b 100644\n\
                 --- a/foo.txt\n\
                 +++ b/foo.txt\n\
                 @@ -1 +1 @@\n\
                 -hello\n\
                 +goodbye\n\
                 \\ No newline at end of file\n",
            ))),
            comment_symbol: "#".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        pd.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             # Instruction\n\
             # Diff instruction\n\
             # ------------------------ >8 ------------------------\n\
             # Do not modify or remove the line above.\n\
             \n\
             Some stuff before first diff --git\n\
             \n\
             diff --git a/foo.txt b/foo.txt\n\
             index ce01362..a21e91b 100644\n\
             --- a/foo.txt\n\
             +++ b/foo.txt\n\
             @@ -1 +1 @@\n\
             -hello\n\
             +goodbye\n\
             \\ No newline at end of file\n",
        );

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(buf.as_slice(), "#").unwrap();

        compare_patch_descs(&edited_desc, &pd);
    }

    #[test]
    fn with_extra_comments() {
        let patch_desc = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            }),
            message: "Subject\n\
                      \n\
                      Body of message.\n   # Indented: not a comment.\n\
                      \n\
                      With-a-trailer: yes\n\
                      "
            .to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: None,
            diff: None,
            comment_symbol: "#".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             Body of message.\n   # Indented: not a comment.\n\
             \n\
             With-a-trailer: yes\n\
             \n\
             # Instruction\n",
        );

        let edited = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        # Next line must be blank.\n\
        \n\
        # Subject is below\n\
        Subject\n\
        \n\
        Body of message.\n   # Indented: not a comment.\n\
        \n\
        # Trailer is below\n\
        With-a-trailer: yes\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), "#").unwrap();

        compare_patch_descs(&edited_desc, &patch_desc);
    }

    #[test]
    fn missing_patch_header() {
        let description = b"\
        # Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        let expected = EditedPatchDescription {
            patchname: None,
            author: Some(Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            })),
            message: "Subject\n".to_string(),
            diff: None,
        };

        compare_edited_descs(&edited_desc, &expected);
    }

    #[test]
    fn missing_date_header() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        # Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        let commented_time = gix::date::Time::new(987654321, 21600);
        assert!(edited_desc.author.is_some());
        assert!(edited_desc.author.as_ref().unwrap().is_some());
        // Author date should be "now" if Author is present and Date is missing.
        // We just check that the commented-out time is not used.
        assert_ne!(
            edited_desc.author.as_ref().unwrap().as_ref().unwrap().time,
            commented_time
        );

        let expected = EditedPatchDescription {
            patchname: Some(Some("patch".parse::<PatchName>().unwrap())),
            author: Some(Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: edited_desc.author.as_ref().unwrap().as_ref().unwrap().time,
            })),
            message: "Subject\n".to_string(),
            diff: None,
        };

        compare_edited_descs(&edited_desc, &expected);
    }

    #[test]
    fn extra_header() {
        let description = b"\
        Patch:  patch\n\
        Extra:  nope\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        let expected = EditedPatchDescription {
            patchname: Some(Some("patch".parse::<PatchName>().unwrap())),
            author: None,
            message: "Extra:  nope\n\
                      Author: The Author <author@example.com>\n\
                      Date:   2001-04-19 10:25:21 +0600\n\
                      \n\
                      Subject\n"
                .to_string(),
            diff: None,
        };

        compare_edited_descs(&expected, &edited_desc);
    }

    #[test]
    fn invalid_date() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001/04/19 10:25:21 +0600\n\
        \n\
        Subject\n\
        \n\
        # Instruction\n";

        assert!(
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").is_err()
        );
    }

    #[test]
    fn no_blank_before_message() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        Subject\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();
        assert_eq!(edited_desc.message, "Subject\n");
    }

    #[test]
    fn extra_blanks_before_message() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        \n\
        \n\
        Subject\n\
        \n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        assert_eq!(edited_desc.message, "Subject\n");
    }

    #[test]
    fn no_blank_before_end() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        # Instruction\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        assert_eq!(edited_desc.message, "Subject\n");
    }

    #[test]
    fn no_eol_message() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        assert_eq!(edited_desc.message, "Subject\n");
    }

    #[test]
    fn extra_blanks_in_message() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        \n\
        \n\
        body\n\
        \n\
        \n  \
        \n\
        more body\n\
        \n\t\
        \n\
        # Instruction\n\
        \n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(description.as_slice(), "#").unwrap();

        assert_eq!(
            edited_desc.message,
            "Subject\n\
             \n\
             body\n\
             \n\
             more body\n"
        );
    }

    #[test]
    fn empty_diff() {
        for description in [
            b"# ------------------------ >8 ------------------------\n\
              # Do not modify or remove the line above.\n"
                .as_slice(),
            b"# ------------------------ >8 ------------------------\n",
            b"# ------------------------ >8 ------------------------",
            b"# ------------------------ >8 ------------------------    ",
            b"# ------------------------ >8 ------------------------  \n",
            b"# ------------------------ >8 ------------------------\n  \n",
        ] {
            let result = EditedPatchDescription::parse_with_comment_symbol(description, "#");
            assert!(result.is_err());
        }
    }

    #[test]
    fn no_headers() {
        let edited = b"\
        Subject\n\
        \n\
        Body1.\n\
        Body2.\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), "#").unwrap();

        assert!(edited_desc.patchname.is_none());
        assert!(edited_desc.author.is_none());
        assert_eq!(edited_desc.message.as_str(), "Subject\n\nBody1.\nBody2.\n");
        assert!(edited_desc.diff.is_none());
    }

    #[test]
    fn empty_headers() {
        let edited = b"\
        Patch:     \n\
        Author:    \n\
        Date:      \n\
        \n\
        Subject\n\
        \n\
        Body1.\n\
        Body2.\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), "#").unwrap();

        assert!(edited_desc.patchname.unwrap().is_none());
        assert!(edited_desc.author.unwrap().is_none());
        assert_eq!(edited_desc.message.as_str(), "Subject\n\nBody1.\nBody2.\n");
        assert!(edited_desc.diff.is_none());
    }

    #[test]
    fn message_with_dashed_separator_no_headers() {
        let edited = b"\
        Subject\n\
        \n\
        Body1.\n\
        Body2.\n\
        ---\n\
        Extra.\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), "#").unwrap();

        assert!(edited_desc.patchname.is_none());
        assert!(edited_desc.author.is_none());
        assert_eq!(
            edited_desc.message.as_str(),
            "Subject\n\
             \n\
             Body1.\n\
             Body2.\n\
             ---\n\
             Extra.\n"
        );
        assert!(edited_desc.diff.is_none());
    }

    #[test]
    fn message_with_dashed_separator_empty_headers() {
        let edited = b"\
        Patch:     \n\
        Author:    \n\
        Date:      \n\
        \n\
        Subject\n\
        \n\
        Body1.\n\
        Body2.\n\
        ---\n\
        Extra.\n";

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), "#").unwrap();

        assert!(edited_desc.patchname.unwrap().is_none());
        assert!(edited_desc.author.unwrap().is_none());
        assert_eq!(
            edited_desc.message.as_str(),
            "Subject\n\
             \n\
             Body1.\n\
             Body2.\n\
             ---\n\
             Extra.\n"
        );
        assert!(edited_desc.diff.is_none());
    }

    #[test]
    fn parse_with_custom_comment_symbol() {
        let edited = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject\n\
        %% Comment\n\
        Body 1.\n\
        %% Instruction\n";

        let comment_symbol = "%%";
        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(edited.as_slice(), comment_symbol)
                .unwrap();

        assert_eq!(edited_desc.message, "Subject\nBody 1.\n");
    }
    #[test]
    fn write_with_custom_comment_symbol() {
        let pd = EditablePatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: Some(gix::actor::Signature {
                name: BString::from("The Author"),
                email: BString::from("author@example.com"),
                time: gix::date::Time::new(987654321, 21600),
            }),
            message: "Subject\n".to_string(),
            instruction: Some("Instruction\n".to_string()),
            diff_instruction: Some("Diff instruction\n".to_string()),
            diff: Some(DiffBuffer(BString::from(
                "\n\
                 Some stuff before first diff --git\n\
                 \n\
                 diff --git a/foo.txt b/foo.txt\n\
                 index ce01362..a21e91b 100644\n\
                 --- a/foo.txt\n\
                 +++ b/foo.txt\n\
                 @@ -1 +1 @@\n\
                 -hello\n\
                 +goodbye\n\
                 \\ No newline at end of file\n",
            ))),
            comment_symbol: "%%".to_string(),
        };

        let mut buf: Vec<u8> = vec![];
        pd.write(&mut buf).unwrap();

        assert_eq!(
            buf.to_str().unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             %% Instruction\n\
             %% Diff instruction\n\
             %% ------------------------ >8 ------------------------\n\
             %% Do not modify or remove the line above.\n\
             \n\
             Some stuff before first diff --git\n\
             \n\
             diff --git a/foo.txt b/foo.txt\n\
             index ce01362..a21e91b 100644\n\
             --- a/foo.txt\n\
             +++ b/foo.txt\n\
             @@ -1 +1 @@\n\
             -hello\n\
             +goodbye\n\
             \\ No newline at end of file\n",
        );

        let edited_desc =
            EditedPatchDescription::parse_with_comment_symbol(buf.as_slice(), "%%").unwrap();

        compare_patch_descs(&edited_desc, &pd);
    }
}
