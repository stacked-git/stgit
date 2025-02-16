// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchLocator`].

use winnow::{
    ascii::hex_digit1,
    combinator::{alt, opt, repeat},
    ModalResult, Parser,
};

use super::{
    name::patch_name,
    numbers::{negative_int, nonplussed_int, plusative_int, sign, unsigned_int},
    Sign,
};
use crate::patch::{PatchId, PatchLocator, PatchOffsetAtom, PatchOffsets};

pub(in super::super) fn patch_locator(input: &mut &str) -> ModalResult<PatchLocator> {
    alt((
        patch_locator_name,
        patch_locator_from_last,
        patch_locator_top,
        patch_locator_base,
    ))
    .parse_next(input)
}

fn patch_locator_name(input: &mut &str) -> ModalResult<PatchLocator> {
    (patch_name, patch_offsets)
        .map(|(patchname, offsets)| PatchLocator {
            id: PatchId::Name(patchname),
            offsets,
        })
        .parse_next(input)
}

fn patch_locator_from_last(input: &mut &str) -> ModalResult<PatchLocator> {
    ('^', opt(nonplussed_int), patch_offsets)
        .map(|(_, below_last, offsets)| PatchLocator {
            id: PatchId::BelowLast(below_last),
            offsets,
        })
        .parse_next(input)
}

pub(in super::super) fn patch_locator_top(input: &mut &str) -> ModalResult<PatchLocator> {
    alt((
        ('@', patch_offsets).map(|(_, offsets)| PatchLocator {
            id: PatchId::Top,
            offsets,
        }),
        ('~', opt(unsigned_int), patch_offsets).map(|(_, n, offsets)| PatchLocator {
            id: PatchId::BelowTop(n),
            offsets,
        }),
    ))
    .parse_next(input)
}

pub(in super::super) fn patch_locator_base(input: &mut &str) -> ModalResult<PatchLocator> {
    ("{base}", patch_offsets)
        .map(|(_, offsets)| PatchLocator {
            id: PatchId::Base,
            offsets,
        })
        .parse_next(input)
}

pub(in super::super) fn patch_offsets(input: &mut &str) -> ModalResult<PatchOffsets> {
    repeat::<_, _, Vec<PatchOffsetAtom>, _, _>(0.., patch_offset_atom)
        .take()
        .map(|s: &str| PatchOffsets(s.to_string()))
        .parse_next(input)
}

pub(in super::super) fn patch_offset_atoms(input: &mut &str) -> ModalResult<Vec<PatchOffsetAtom>> {
    repeat(0.., patch_offset_atom).parse_next(input)
}

pub(in super::super) fn patch_offset_atom(input: &mut &str) -> ModalResult<PatchOffsetAtom> {
    alt((patch_offset_atom_plus, patch_offset_atom_tilde)).parse_next(input)
}

fn patch_offset_atom_plus(input: &mut &str) -> ModalResult<PatchOffsetAtom> {
    ('+', opt(unsigned_int))
        .map(|(_, n)| PatchOffsetAtom::Plus(n))
        .parse_next(input)
}

fn patch_offset_atom_tilde(input: &mut &str) -> ModalResult<PatchOffsetAtom> {
    ('~', opt(unsigned_int))
        .map(|(_, n)| PatchOffsetAtom::Tilde(n))
        .parse_next(input)
}

pub(in super::super) fn oid_prefix_offsets(
    input: &mut &str,
) -> ModalResult<(gix::hash::Prefix, PatchOffsets)> {
    (oid_prefix, patch_offsets).parse_next(input)
}

fn oid_prefix(input: &mut &str) -> ModalResult<gix::hash::Prefix> {
    hex_digit1
        .try_map(gix::hash::Prefix::from_hex)
        .parse_next(input)
}

pub(in super::super) fn sign_number_offsets(
    input: &mut &str,
) -> ModalResult<(Option<Sign>, Option<usize>, PatchOffsets)> {
    alt((
        (negative_int, patch_offsets)
            .map(|(n, offsets)| (Some(Sign::Minus), Some(n.unsigned_abs()), offsets)),
        (plusative_int, patch_offsets)
            .map(|(n, offsets)| (Some(Sign::Plus), Some(n.unsigned_abs()), offsets)),
        (unsigned_int, patch_offsets).map(|(n, offsets)| (None, Some(n), offsets)),
        (sign, patch_offsets).map(|(sign, offsets)| (Some(sign), None, offsets)),
    ))
    .parse_next(input)
}
