// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

use winnow::{
    error::{ContextError, ErrMode, ErrorKind, ParserError},
    stream::Stream,
    PResult, Parser,
};

/// A partial git reference name.
///
/// A partial reference name does not have to include the "refs/" prefix which a
/// complete reference name would require. A partial ref name may thus be used to, for
/// example, capture a short branch name such as "main" (which is short for
/// "refs/heads/main").
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub(crate) struct PartialRefName(pub(crate) String);

impl From<&PartialRefName> for gix::refs::PartialName {
    fn from(value: &PartialRefName) -> Self {
        gix::refs::PartialName::try_from(value.0.as_str())
            .expect("PartialRefName was already validated")
    }
}

impl<'a> From<&'a PartialRefName> for &'a bstr::BStr {
    fn from(value: &'a PartialRefName) -> Self {
        bstr::BStr::new(value.0.as_str())
    }
}

impl AsRef<str> for PartialRefName {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl std::fmt::Display for PartialRefName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for PartialRefName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        partial_ref_name
            .parse(s)
            .map_err(|e| anyhow::format_err!("{e}"))
    }
}

pub(crate) fn partial_ref_name(input: &mut &str) -> PResult<PartialRefName> {
    let mut iter = input.iter_offsets().peekable();
    let mut start_index = 0;
    let mut prev = None;

    let mut split_offset = loop {
        if let Some((i, c)) = iter.next() {
            let mut peek_next = || iter.peek().map(|(_, c)| *c);
            if c == '\\' {
                if i == 0 && peek_next() == Some('-') {
                    start_index = 1;
                } else {
                    break i;
                }
            } else if c.is_control()
                || matches!(c, ' ' | '~' | '^' | ':' | '?' | '[' | '*' | '\x7f')
                || (c == '/' && (i == 0 || matches!(peek_next(), Some('/') | None)))
                || (c == '.'
                    && (i == 0
                        || matches!(peek_next(), Some('.') | None)
                        || (prev == Some('/') && peek_next() == Some('/'))))
                || (c == '@' && peek_next() == Some('{'))
            {
                // Some characters are never allowed.
                // Branch name may not start or end with '.'.
                // Sequence of ".." is not allowed.
                // Branch name may not start or end with '/'.
                // Sequence of "//" and "/./" are not allowed.
                // Sequence of "@{" is not allowed.
                break i;
            } else {
                prev = Some(c);
            }
        } else {
            break input.len();
        }
    };

    if start_index == 1 {
        input.next_token();
        split_offset -= 1;
    }
    if input[..split_offset].ends_with(['.', '/']) {
        split_offset -= 1;
    }

    let name = input.next_slice(split_offset);

    if name.is_empty() {
        Err(ErrMode::Backtrack(ContextError::from_error_kind(
            input,
            ErrorKind::Eof,
        )))
    } else if name == "-" {
        Err(ErrMode::Backtrack(ContextError::from_error_kind(
            input,
            ErrorKind::Verify,
        )))
    } else if name.ends_with(".lock") {
        // Names ending with ".lock" are invalid and there is no recovery.
        Err(ErrMode::Cut(ContextError::from_error_kind(
            input,
            ErrorKind::Verify,
        )))
    } else {
        gix::refs::PartialName::try_from(name).expect("parser only allows valid names");
        Ok(PartialRefName(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use winnow::Parser;

    use super::*;

    #[test]
    fn test_partial_ref_name() {
        assert!(partial_ref_name.parse_peek("abc.lock").is_err());
        assert_eq!(
            partial_ref_name.parse_peek("abc"),
            Ok(("", PartialRefName(String::from("abc"))))
        );
        assert_eq!(
            partial_ref_name.parse_peek("abc^^^"),
            Ok(("^^^", PartialRefName(String::from("abc"))))
        );
        assert_eq!(
            partial_ref_name.parse_peek("abc."),
            Ok((".", PartialRefName(String::from("abc"))))
        );
        assert_eq!(
            partial_ref_name.parse_peek("abc.def"),
            Ok(("", PartialRefName(String::from("abc.def"))))
        );
        assert_eq!(
            partial_ref_name.parse_peek("abc.def/"),
            Ok(("/", PartialRefName(String::from("abc.def"))))
        );
    }
}
