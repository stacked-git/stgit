// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchRange`] and [`PatchRangeBounds`].

use winnow::{
    combinator::{alt, opt, separated_pair},
    ModalResult, Parser,
};

use super::patch_locator;
use crate::patch::{PatchRange, PatchRangeBounds};

pub(in super::super) fn patch_range(input: &mut &str) -> ModalResult<PatchRange> {
    alt((
        patch_range_bounds.map(PatchRange::Range),
        patch_locator.map(PatchRange::Single),
    ))
    .parse_next(input)
}

pub(super) fn patch_range_bounds(input: &mut &str) -> ModalResult<PatchRangeBounds> {
    separated_pair(opt(patch_locator), "..", opt(patch_locator))
        .map(|(begin, end)| PatchRangeBounds { begin, end })
        .parse_next(input)
}
