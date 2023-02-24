// SPDX-License-Identifier: GPL-2.0-only

//! Parsing support for [`PatchLocator`].

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char as the_char, hex_digit1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many0_count},
    sequence::tuple,
};

use super::{
    name::patch_name,
    numbers::{negative_int, nonplussed_int, plusative_int, sign, unsigned_int},
    Sign,
};
use crate::patch::{PatchId, PatchLocator, PatchOffsetAtom, PatchOffsets};

pub(in super::super) fn patch_locator(input: &str) -> nom::IResult<&str, PatchLocator> {
    alt((
        patch_locator_name,
        patch_locator_from_last,
        patch_locator_top,
        patch_locator_base,
    ))(input)
}

fn patch_locator_name(input: &str) -> nom::IResult<&str, PatchLocator> {
    map(
        tuple((patch_name, patch_offsets)),
        |(patchname, offsets)| PatchLocator {
            id: PatchId::Name(patchname),
            offsets,
        },
    )(input)
}

fn patch_locator_from_last(input: &str) -> nom::IResult<&str, PatchLocator> {
    map(
        tuple((the_char('^'), opt(nonplussed_int), patch_offsets)),
        |(_, below_last, offsets)| PatchLocator {
            id: PatchId::BelowLast(below_last),
            offsets,
        },
    )(input)
}

pub(in super::super) fn patch_locator_top(input: &str) -> nom::IResult<&str, PatchLocator> {
    alt((
        map(tuple((the_char('@'), patch_offsets)), |(_, offsets)| {
            PatchLocator {
                id: PatchId::Top,
                offsets,
            }
        }),
        map(
            tuple((the_char('~'), opt(unsigned_int), patch_offsets)),
            |(_, n, offsets)| PatchLocator {
                id: PatchId::BelowTop(n),
                offsets,
            },
        ),
    ))(input)
}

pub(in super::super) fn patch_locator_base(input: &str) -> nom::IResult<&str, PatchLocator> {
    map(tuple((tag("{base}"), patch_offsets)), |(_, offsets)| {
        PatchLocator {
            id: PatchId::Base,
            offsets,
        }
    })(input)
}

pub(in super::super) fn patch_offsets(input: &str) -> nom::IResult<&str, PatchOffsets> {
    map(recognize(many0_count(patch_offset_atom)), |s| {
        PatchOffsets(String::from(s))
    })(input)
}

pub(in super::super) fn patch_offset_atoms(
    input: &str,
) -> nom::IResult<&str, Vec<PatchOffsetAtom>> {
    many0(patch_offset_atom)(input)
}

pub(in super::super) fn patch_offset_atom(input: &str) -> nom::IResult<&str, PatchOffsetAtom> {
    alt((patch_offset_atom_plus, patch_offset_atom_tilde))(input)
}

fn patch_offset_atom_plus(input: &str) -> nom::IResult<&str, PatchOffsetAtom> {
    map(tuple((the_char('+'), opt(unsigned_int))), |(_, n)| {
        PatchOffsetAtom::Plus(n)
    })(input)
}

fn patch_offset_atom_tilde(input: &str) -> nom::IResult<&str, PatchOffsetAtom> {
    map(tuple((the_char('~'), opt(unsigned_int))), |(_, n)| {
        PatchOffsetAtom::Tilde(n)
    })(input)
}

pub(in super::super) fn oid_prefix_offsets(
    input: &str,
) -> nom::IResult<&str, (gix::hash::Prefix, PatchOffsets)> {
    tuple((oid_prefix, patch_offsets))(input)
}

fn oid_prefix(input: &str) -> nom::IResult<&str, gix::hash::Prefix> {
    map_res(hex_digit1, gix::hash::Prefix::from_hex)(input)
}

pub(in super::super) fn sign_number_offsets(
    input: &str,
) -> nom::IResult<&str, (Option<Sign>, Option<usize>, PatchOffsets)> {
    alt((
        map(tuple((negative_int, patch_offsets)), |(n, offsets)| {
            (Some(Sign::Minus), Some(n.unsigned_abs()), offsets)
        }),
        map(tuple((plusative_int, patch_offsets)), |(n, offsets)| {
            (Some(Sign::Plus), Some(n.unsigned_abs()), offsets)
        }),
        map(tuple((unsigned_int, patch_offsets)), |(n, offsets)| {
            (None, Some(n), offsets)
        }),
        map(tuple((sign, patch_offsets)), |(sign, offsets)| {
            (Some(sign), None, offsets)
        }),
    ))(input)
}
