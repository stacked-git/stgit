use std::io::Write;

use chrono::DateTime;

use crate::error::Error;
use crate::patchname::PatchName;
use crate::signature::{self, TimeExtended};

pub(crate) struct DiffBuffer(Vec<u8>);

impl DiffBuffer {
    pub(crate) fn from_bytes(buf: &[u8]) -> Option<Self> {
        let needle = b"\ndiff --git";
        if buf.starts_with(&needle[1..]) {
            Some(Self(buf.to_vec()))
        } else if let Some(pos) = find(buf, needle) {
            Some(Self(buf[pos + 1..].to_vec()))
        } else {
            None
        }
    }
}

impl AsRef<[u8]> for DiffBuffer {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

pub(crate) struct PatchDescription {
    pub patchname: Option<PatchName>,
    pub author: git2::Signature<'static>,
    pub message: String,
    pub diff: Option<DiffBuffer>,
}

impl PatchDescription {
    pub(crate) fn write<S: Write>(
        &self,
        stream: &mut S,
        instruction: Option<&str>,
    ) -> Result<(), Error> {
        self.write_header_and_message(stream)?;
        if let Some(instruction) = instruction {
            writeln!(stream, "\n{}", instruction.trim_end())?;
        } else {
            writeln!(stream)?;
        }
        self.write_diff(stream)?;
        Ok(())
    }

    fn write_header<S: Write>(&self, stream: &mut S) -> Result<(), Error> {
        let patchname = if let Some(patchname) = &self.patchname {
            patchname.as_ref()
        } else {
            ""
        };
        let authdate = self.author.datetime().format("%Y-%m-%d %H:%M:%S %z");

        write!(
            stream,
            "Patch:  {}\n\
             Author: ",
            patchname,
        )?;
        stream.write_all(self.author.name_bytes())?;
        write!(stream, " <")?;
        stream.write_all(self.author.email_bytes())?;
        write!(
            stream,
            ">\n\
             Date:   {}\n",
            authdate,
        )?;
        Ok(())
    }

    fn write_header_and_message<S: Write>(&self, stream: &mut S) -> Result<(), Error> {
        self.write_header(stream)?;
        let message = self.message.trim_end_matches('\n');
        write!(stream, "\n{}\n", message)?;
        Ok(())
    }

    fn write_diff<S: Write>(&self, stream: &mut S) -> Result<(), Error> {
        if let Some(diff) = self.diff.as_ref() {
            stream.write_all(b"---\n")?;
            stream.write_all(diff.as_ref())?;
        }
        Ok(())
    }
}

impl TryFrom<Vec<u8>> for PatchDescription {
    type Error = Error;

    fn try_from(buf: Vec<u8>) -> Result<Self, Self::Error> {
        let mut buf = buf;
        let diff_marker = b"\n---\n";
        let marker_pos = find(&buf, diff_marker);
        let (header, diff) = if let Some(pos) = marker_pos {
            let maybe_diff_buf = DiffBuffer::from_bytes(&buf[pos + diff_marker.len()..]);
            buf.truncate(pos);
            (buf, maybe_diff_buf)
        } else {
            (buf, None)
        };
        let header = String::from_utf8(header).map_err(|_| Error::NonUtf8PatchDescription)?;

        let mut raw_patchname: Option<String> = None;
        let mut raw_author: Option<String> = None;
        let mut raw_authdate: Option<String> = None;
        let mut consuming_message: bool = false;
        let mut consecutive_empty: usize = 0;
        let mut message = if let Some(pos) = marker_pos {
            String::with_capacity(pos)
        } else {
            String::new()
        };

        for line in header.split_inclusive('\n') {
            if line.starts_with('#') {
                continue;
            } else if consuming_message {
                let trimmed = line.trim_end();
                if trimmed.is_empty() {
                    if consecutive_empty == 0 {
                        message.push('\n');
                    }
                    consecutive_empty += 1;
                } else {
                    consecutive_empty = 0;
                    message.push_str(trimmed);
                    message.push('\n')
                }
            } else if raw_patchname.is_some() && raw_author.is_some() && raw_authdate.is_some() {
                let trimmed = line.trim_end();
                if !trimmed.is_empty() {
                    consuming_message = true;
                    message.push_str(trimmed);
                    message.push('\n')
                }
            } else if let Some((key, value)) = line.split_once(':') {
                match key {
                    "Patch" => {
                        if raw_patchname.is_none() {
                            raw_patchname = Some(value.trim().to_string());
                        } else {
                            return Err(Error::ParsePatchDescription(
                                "duplicate `Patch` header".to_string(),
                            ));
                        }
                    }
                    "Author" => {
                        if raw_author.is_none() {
                            raw_author = Some(value.trim().to_string())
                        } else {
                            return Err(Error::ParsePatchDescription(
                                "duplicate `Author` header".to_string(),
                            ));
                        }
                    }
                    "Date" => {
                        if raw_authdate.is_none() {
                            raw_authdate = Some(value.trim().to_string())
                        } else {
                            return Err(Error::ParsePatchDescription(
                                "duplicate `Date` header".to_string(),
                            ));
                        }
                    }
                    _ => {
                        return Err(Error::ParsePatchDescription(format!(
                            "invalid header key `{}`",
                            key
                        )))
                    }
                }
            } else {
                return Err(Error::ParsePatchDescription(format!(
                    "could not parse `{}`",
                    line.trim()
                )));
            }
        }

        let patchname: Option<PatchName> = if let Some(patchname) = raw_patchname {
            if !patchname.is_empty() {
                Some(patchname.parse::<PatchName>()?)
            } else {
                None
            }
        } else {
            return Err(Error::ParsePatchDescription(
                "`Patch:` header is missing".to_string(),
            ));
        };

        let (authname, authemail) = if let Some(author) = raw_author {
            let (name, email) = signature::parse_name_email(&author)?;
            (name.to_string(), email.to_string())
        } else {
            return Err(Error::ParsePatchDescription(
                "`Author:` header is missing".to_string(),
            ));
        };

        let authdate = if let Some(authdate) = raw_authdate {
            let dt = DateTime::parse_from_str(&authdate, "%Y-%m-%d %H:%M:%S %z")
                .map_err(|_| Error::InvalidDate(authdate, "patch description".to_string()))?;
            git2::Time::new(dt.timestamp(), dt.offset().local_minus_utc() / 60)
        } else {
            return Err(Error::ParsePatchDescription(
                "`Date:` header is missing".to_string(),
            ));
        };

        let author = git2::Signature::new(&authname, &authemail, &authdate)?;

        if message.ends_with("\n\n") {
            message.pop();
        }

        if message.trim().is_empty() {
            message.clear();
        }

        Ok(Self {
            patchname,
            author,
            message,
            diff,
        })
    }
}

fn find(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if haystack.len() >= needle.len() {
        for i in 0..haystack.len() - needle.len() + 1 {
            if haystack[i..i + needle.len()] == needle[..] {
                return Some(i);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compare_patch_descs(pd0: &PatchDescription, pd1: &PatchDescription) {
        assert_eq!(pd0.patchname, pd1.patchname);
        assert_eq!(pd0.author.name(), pd1.author.name());
        assert_eq!(pd0.author.email(), pd1.author.email());
        assert_eq!(pd0.author.when().seconds(), pd1.author.when().seconds());
        assert_eq!(
            pd0.author.when().offset_minutes(),
            pd1.author.when().offset_minutes()
        );
        assert_eq!(pd0.message, pd1.message);

        let mut diff0: Vec<u8> = vec![];
        pd0.write_diff(&mut diff0).unwrap();
        let mut diff1: Vec<u8> = vec![];
        pd1.write_diff(&mut diff1).unwrap();
        assert_eq!(diff0, diff1);
    }

    #[test]
    fn round_trip_no_message_no_diff() {
        let patch_desc = PatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: git2::Signature::new(
                "The Author",
                "author@example.com",
                &git2::Time::new(987654321, -60),
            )
            .unwrap(),
            message: "".to_string(),
            diff: None,
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf, Some("# Instruction\n")).unwrap();

        assert_eq!(
            std::str::from_utf8(buf.as_slice()).unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 03:25:21 -0100\n\
             \n\
             \n\
             \n\
             # Instruction\n",
        );

        let new_pd = PatchDescription::try_from(buf).unwrap();

        compare_patch_descs(&new_pd, &patch_desc);
    }

    #[test]
    fn round_trip_one_line_message() {
        let patch_desc = PatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: git2::Signature::new(
                "The Author",
                "author@example.com",
                &git2::Time::new(987654321, 360),
            )
            .unwrap(),
            message: "Subject\n".to_string(),
            diff: None,
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf, Some("# Instruction\n")).unwrap();

        assert_eq!(
            std::str::from_utf8(buf.as_slice()).unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             # Instruction\n",
        );

        let new_pd = PatchDescription::try_from(buf).unwrap();

        compare_patch_descs(&new_pd, &patch_desc);
    }

    #[test]
    fn round_trip_multi_line_message() {
        let patch_desc = PatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: git2::Signature::new(
                "The Author",
                "author@example.com",
                &git2::Time::new(987654321, 360),
            )
            .unwrap(),
            message: "Subject\n\
                      \n\
                      Body of message.\n\
                      More body of message.\n\
                      \n\
                      With-a-trailer: yes\n\
                      "
            .to_string(),
            diff: None,
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf, Some("# Instruction\n")).unwrap();

        assert_eq!(
            std::str::from_utf8(buf.as_slice()).unwrap(),
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

        let new_pd = PatchDescription::try_from(buf).unwrap();

        compare_patch_descs(&new_pd, &patch_desc);
    }

    #[test]
    fn with_diff() {
        let patch_desc = PatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: git2::Signature::new(
                "The Author",
                "author@example.com",
                &git2::Time::new(987654321, 360),
            )
            .unwrap(),
            message: "Subject\n".to_string(),
            diff: DiffBuffer::from_bytes(
                b"\n\
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
            ),
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf, Some("# Instruction\n")).unwrap();

        assert_eq!(
            std::str::from_utf8(buf.as_slice()).unwrap(),
            "Patch:  patch\n\
             Author: The Author <author@example.com>\n\
             Date:   2001-04-19 10:25:21 +0600\n\
             \n\
             Subject\n\
             \n\
             # Instruction\n\
             ---\n\
             diff --git a/foo.txt b/foo.txt\n\
             index ce01362..a21e91b 100644\n\
             --- a/foo.txt\n\
             +++ b/foo.txt\n\
             @@ -1 +1 @@\n\
             -hello\n\
             +goodbye\n\
             \\ No newline at end of file\n",
        );

        let new_pd = PatchDescription::try_from(buf).unwrap();

        compare_patch_descs(&new_pd, &patch_desc);
    }

    #[test]
    fn with_extra_comments() {
        let patch_desc = PatchDescription {
            patchname: Some("patch".parse::<PatchName>().unwrap()),
            author: git2::Signature::new(
                "The Author",
                "author@example.com",
                &git2::Time::new(987654321, 360),
            )
            .unwrap(),
            message: "Subject\n\
                      \n\
                      Body of message.\n   # Indented: not a comment.\n\
                      \n\
                      With-a-trailer: yes\n\
                      "
            .to_string(),
            diff: None,
        };

        let mut buf: Vec<u8> = vec![];
        patch_desc.write(&mut buf, Some("# Instruction\n")).unwrap();

        assert_eq!(
            std::str::from_utf8(buf.as_slice()).unwrap(),
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

        let updated = b"# These are headers\n\
                        Patch:  patch\n\
                        # The author signature is below\n\
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

        let new_pd = PatchDescription::try_from(updated.to_vec()).unwrap();

        compare_patch_descs(&new_pd, &patch_desc);
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

        assert!(PatchDescription::try_from(description.to_vec()).is_err());
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

        assert!(PatchDescription::try_from(description.to_vec()).is_err());
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

        assert!(PatchDescription::try_from(description.to_vec()).is_err());
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

        assert!(PatchDescription::try_from(description.to_vec()).is_err());
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

        let pd = PatchDescription::try_from(description.to_vec()).unwrap();
        assert_eq!(pd.message, "Subject\n");
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

        let pd = PatchDescription::try_from(description.to_vec()).unwrap();

        assert_eq!(pd.message, "Subject\n");
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

        let pd = PatchDescription::try_from(description.to_vec()).unwrap();

        assert_eq!(pd.message, "Subject\n");
    }

    #[test]
    fn no_eol_message() {
        let description = b"\
        Patch:  patch\n\
        Author: The Author <author@example.com>\n\
        Date:   2001-04-19 10:25:21 +0600\n\
        \n\
        Subject";

        let pd = PatchDescription::try_from(description.to_vec()).unwrap();

        assert_eq!(pd.message, "Subject\n");
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

        let pd = PatchDescription::try_from(description.to_vec()).unwrap();

        assert_eq!(
            pd.message,
            "Subject\n\
             \n\
             body\n\
             \n\
             more body\n"
        );
    }
}
