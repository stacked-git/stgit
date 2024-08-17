// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchName`].

use winnow::{
    error::{ContextError, ErrMode, ErrorKind, ParserError},
    stream::Stream,
    PResult,
};

use crate::patch::PatchName;

pub(super) fn patch_name(input: &mut &str) -> PResult<PatchName> {
    let mut iter = input.char_indices().peekable();
    let mut start_index = 0;

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

    if start_index == 1 {
        input.next_token();
        split_offset -= 1;
    }
    if input[..split_offset].ends_with('.') {
        split_offset -= 1;
    }

    let name = input.next_slice(split_offset);

    if name.is_empty() {
        Err(ErrMode::Backtrack(ContextError::from_error_kind(
            input,
            ErrorKind::Eof,
        )))
    } else if name.ends_with(".lock") {
        // Names ending with ".lock" are invalid and there is no recovery.
        Err(ErrMode::Cut(ContextError::from_error_kind(
            input,
            ErrorKind::Verify,
        )))
    } else if name == "@" || name == "{base}" {
        Err(ErrMode::Backtrack(ContextError::from_error_kind(
            input,
            ErrorKind::Verify,
        )))
    } else {
        // This should be detected above.
        debug_assert!(!name.ends_with('.'));
        Ok(PatchName(String::from(name)))
    }
}
