// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

/// A partial git reference name.
///
/// A partial reference name does not have to include the "refs/" prefix which a
/// complete reference name would require. A partial ref name may thus be used to, for
/// example, capture a short branch name such as "main" (which is short for
/// "refs/heads/main").
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PartialRefName(pub(crate) String);

impl From<&PartialRefName> for gix::refs::PartialName {
    fn from(value: &PartialRefName) -> Self {
        gix::refs::PartialName::try_from(value.0.as_str())
            .expect("PartialRefName was already validated")
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
        f.write_str(self.0.as_str())
    }
}

impl FromStr for PartialRefName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(partial_ref_name))(s)
            .map(|(_, partial_name_ref)| partial_name_ref)
            .map_err(|e| anyhow::anyhow!("{e}"))
    }
}

pub(crate) fn partial_ref_name(input: &str) -> nom::IResult<&str, PartialRefName> {
    use nom::error::{Error, ErrorKind};

    let mut iter = input.char_indices().peekable();
    let mut start_index = 0;
    let mut prev = None;

    let split_index = loop {
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

    let (name, rest) = (&input[start_index..split_index], &input[split_index..]);
    let (name, rest) = name
        .ends_with(|c| matches!(c, '.' | '/'))
        .then(|| {
            (
                &input[start_index..split_index - 1],
                &input[split_index - 1..],
            )
        })
        .unwrap_or((name, rest));

    if name.is_empty() {
        Err(nom::Err::Error(Error {
            input,
            code: ErrorKind::Eof,
        }))
    } else if name.ends_with(".lock") {
        // Names ending with ".lock" are invalid and there is no recovery.
        Err(nom::Err::Failure(Error {
            input,
            code: ErrorKind::Verify,
        }))
    } else {
        gix::refs::PartialName::try_from(name).expect("parser only allows valid names");
        Ok((rest, PartialRefName(name.to_string())))
    }
}
