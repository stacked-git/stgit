// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for numbers.

use winnow::{
    ascii::digit1,
    combinator::{alt, opt},
    PResult, Parser,
};

use super::Sign;

/// Parse a sign character, i.e. `-` or `+`.
pub(super) fn sign(input: &mut &str) -> PResult<Sign> {
    alt(('-'.map(|_| Sign::Minus), '+'.map(|_| Sign::Plus))).parse_next(input)
}

/// Parse unsigned int.
///
/// Although the returned value is a [`usize`], it is validated as an [`isize`]. This
/// ensures that potential sign conversions are more likely to succeed.
pub(super) fn unsigned_int(input: &mut &str) -> PResult<usize> {
    digit1
        .try_map(|s: &str| s.parse::<isize>().map(|n| n as usize))
        .parse_next(input)
}

/// Parse a negative int. I.e. an int with a leading `-` sign.
pub(super) fn negative_int(input: &mut &str) -> PResult<isize> {
    ('-', digit1)
        .take()
        .try_map(|s: &str| s.parse::<isize>())
        .parse_next(input)
}

/// Parse a positive int with a leading `+` sign.
pub(super) fn plusative_int(input: &mut &str) -> PResult<isize> {
    ('+', digit1)
        .take()
        .try_map(|s: &str| s.parse::<isize>())
        .parse_next(input)
}

/// Parse a signed int, but disallow an explicit `+` sign.
pub(super) fn nonplussed_int(input: &mut &str) -> PResult<isize> {
    (opt('-'), digit1)
        .take()
        .try_map(|s: &str| s.parse::<isize>())
        .parse_next(input)
}
