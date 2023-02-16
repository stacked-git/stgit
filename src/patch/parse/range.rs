// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchRange`] and [`PatchRangeBounds`].

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::separated_pair,
};

use crate::patch::{PatchRange, PatchRangeBounds};

use super::patch_locator;

pub(in super::super) fn patch_range(input: &str) -> nom::IResult<&str, PatchRange> {
    alt((
        map(patch_range_bounds, PatchRange::Range),
        map(patch_locator, PatchRange::Single),
    ))(input)
}

pub(super) fn patch_range_bounds(input: &str) -> nom::IResult<&str, PatchRangeBounds> {
    map(
        separated_pair(opt(patch_locator), tag(".."), opt(patch_locator)),
        |(begin, end)| PatchRangeBounds { begin, end },
    )(input)
}
