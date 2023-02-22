// SPDX-License-Identifier: GPL-2.0-only

//! Parsers for revision specifications.
//!
//! Both patch-like or git-like revision specifications are recognized by StGit.

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::char as the_char,
    combinator::{map, opt, recognize},
    multi::{many0, many0_count},
    sequence::{delimited, preceded, terminated, tuple},
    Parser,
};

use super::{
    super::{GitRevisionSuffix, PatchLikeSpec, RangeRevisionSpec, SingleRevisionSpec},
    numbers::unsigned_int,
    patch_locator,
    range::patch_range_bounds,
};
use crate::wrap::{partial_ref_name, PartialRefName};

pub(in super::super) fn range_revision_spec(input: &str) -> nom::IResult<&str, RangeRevisionSpec> {
    alt((
        tuple((branch_prefix, patch_range_bounds))
            .map(|(branch, bounds)| RangeRevisionSpec::BranchRange { branch, bounds }),
        patch_range_bounds.map(RangeRevisionSpec::Range),
        single_revision_spec.map(RangeRevisionSpec::Single),
    ))(input)
}

pub(in super::super) fn single_revision_spec(
    input: &str,
) -> nom::IResult<&str, SingleRevisionSpec> {
    alt((
        tuple((branch_prefix, patch_like_spec))
            .map(|(branch, patch_like)| SingleRevisionSpec::Branch { branch, patch_like }),
        patch_and_or_git_like_spec,
    ))(input)
}

fn branch_prefix(input: &str) -> nom::IResult<&str, PartialRefName> {
    terminated(partial_ref_name, the_char(':'))(input)
}

fn patch_and_or_git_like_spec(input: &str) -> nom::IResult<&str, SingleRevisionSpec> {
    use std::cmp::Ordering;

    match (patch_like_spec(input), git_like_spec(input)) {
        (Ok((rest_patch, patch_like)), Ok((rest_git, git_like))) => {
            // The input looks like both a git revision spec and a patch-like revision spec.
            // If they consumed the same amount of input, then both are passed along to be
            // figured out at resolve-time. Otherwise, the one that consumed the most input
            // is declared the winner. E.g. "foo/bar" would be git-like.
            match rest_patch.len().cmp(&rest_git.len()) {
                Ordering::Equal => Ok((
                    rest_git,
                    SingleRevisionSpec::PatchAndGitLike(patch_like, git_like),
                )),
                Ordering::Less => Ok((rest_patch, SingleRevisionSpec::PatchLike(patch_like))),
                Ordering::Greater => Ok((rest_git, SingleRevisionSpec::GitLike(git_like))),
            }
        }
        (Ok((rest, patch_like)), Err(_)) => Ok((rest, SingleRevisionSpec::PatchLike(patch_like))),
        (Err(_), Ok((rest, git_like))) => Ok((rest, SingleRevisionSpec::GitLike(git_like))),
        (Err(e), Err(_)) => Err(e),
    }
}

pub(in super::super) fn patch_like_spec(input: &str) -> nom::IResult<&str, PatchLikeSpec> {
    map(
        tuple((patch_locator, git_revision_suffix)),
        |(patch_loc, suffix)| PatchLikeSpec { patch_loc, suffix },
    )(input)
}

fn git_like_spec(input: &str) -> nom::IResult<&str, String> {
    map(
        recognize(tuple((partial_ref_name, git_revision_suffix))),
        |s| s.to_string(),
    )(input)
}

fn git_revision_suffix(input: &str) -> nom::IResult<&str, GitRevisionSuffix> {
    map(
        recognize(many0_count(alt((
            at_braced,
            caret_braced,
            recognize(caret_number),
            recognize(tilde_number),
        )))),
        |s| GitRevisionSuffix(s.to_string()),
    )(input)
}

fn at_braced(input: &str) -> nom::IResult<&str, &str> {
    preceded(the_char('@'), braced)(input)
}

fn caret_braced(input: &str) -> nom::IResult<&str, &str> {
    preceded(the_char('^'), braced)(input)
}

fn caret_number(input: &str) -> nom::IResult<&str, Option<usize>> {
    map(tuple((the_char('^'), opt(unsigned_int))), |(_, n)| n)(input)
}

pub(crate) fn tilde_number(input: &str) -> nom::IResult<&str, Option<usize>> {
    map(tuple((the_char('~'), opt(unsigned_int))), |(_, n)| n)(input)
}

// fn colon_slash_text(input: &str) -> nom::IResult<&str, &str> {
//     use nom::combinator::rest;
//     preceded(tag(":/"), rest)(input)
// }

fn braced(input: &str) -> nom::IResult<&str, &str> {
    delimited(
        the_char('{'),
        recognize(many0(alt((is_not("\\}"), tag("\\}"))))),
        the_char('}'),
    )(input)
}
