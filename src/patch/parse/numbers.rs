// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for numbers.

use nom::{
    branch::alt,
    character::complete::{char as the_char, digit1},
    combinator::{map, map_res, opt, recognize},
    sequence::tuple,
};

use super::Sign;

/// Parse a sign character, i.e. '-' or '+'.
pub(super) fn sign(input: &str) -> nom::IResult<&str, Sign> {
    alt((
        map(the_char('-'), |_| Sign::Minus),
        map(the_char('+'), |_| Sign::Plus),
    ))(input)
}

/// Parse unsigned int.
///
/// Although the returned value is a [`usize`], it is validated as an [`isize`]. This
/// ensures that potential sign conversions are more likely to succeed.
pub(super) fn unsigned_int(input: &str) -> nom::IResult<&str, usize> {
    map_res(digit1, |s: &str| s.parse::<isize>().map(|n| n as usize))(input)
}

/// Parse a negative int. I.e. an int with a leading '-' sign.
pub(super) fn negative_int(input: &str) -> nom::IResult<&str, isize> {
    map_res(recognize(tuple((the_char('-'), digit1))), |s: &str| {
        s.parse::<isize>()
    })(input)
}

/// Parse a positive int with a leading '+' sign.
pub(super) fn plusative_int(input: &str) -> nom::IResult<&str, isize> {
    map_res(recognize(tuple((the_char('+'), digit1))), |s: &str| {
        s.parse::<isize>()
    })(input)
}

/// Parse a signed int, but disallow an explicit '+' sign.
pub(super) fn nonplussed_int(input: &str) -> nom::IResult<&str, isize> {
    map_res(recognize(tuple((opt(the_char('-')), digit1))), |s: &str| {
        s.parse::<isize>()
    })(input)
}
