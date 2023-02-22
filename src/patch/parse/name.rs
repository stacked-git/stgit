// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchName`].

use nom::error::{Error, ErrorKind};

use crate::patch::PatchName;

pub(super) fn patch_name(input: &str) -> nom::IResult<&str, PatchName> {
    let mut iter = input.char_indices().peekable();
    let mut start_index = 0;

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
                || matches!(c, ' ' | '~' | '^' | ':' | '/' | '?' | '[' | '*' | '\x7f')
                || (c == '.' && (i == 0 || matches!(peek_next(), Some('.') | None)))
                || (c == '@' && peek_next() == Some('{'))
            {
                // Some characters are never allowed.
                // Patch name may not start or end with '.'.
                // Sequence of ".." is not allowed.
                // Sequence of "@{" is not allowed.
                break i;
            }
        } else {
            break input.len();
        }
    };

    let (name, rest) = (&input[start_index..split_index], &input[split_index..]);
    let (name, rest) = name
        .ends_with('.')
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
    } else if name == "@" || name == "{base}" {
        Err(nom::Err::Error(Error {
            input,
            code: ErrorKind::Verify,
        }))
    } else {
        // This should be detected above.
        debug_assert!(!name.ends_with('.'));
        Ok((rest, PatchName(String::from(name))))
    }
}
