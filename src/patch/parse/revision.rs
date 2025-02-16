// SPDX-License-Identifier: GPL-2.0-only

//! Parsers for revision specifications.
//!
//! Both patch-like or git-like revision specifications are recognized by StGit.

use winnow::{
    ascii::{digit1, take_escaped},
    combinator::{alt, delimited, opt, preceded, repeat, terminated},
    stream::Stream,
    token::{none_of, one_of},
    ModalResult, Parser,
};

use super::{
    super::{GitRevisionSuffix, PatchLikeSpec, RangeRevisionSpec, SingleRevisionSpec},
    numbers::unsigned_int,
    patch_locator,
    range::patch_range_bounds,
};
use crate::{branchloc::BranchLocator, wrap::partial_ref_name};

pub(in super::super) fn range_revision_spec(input: &mut &str) -> ModalResult<RangeRevisionSpec> {
    alt((
        (branch_prefix, patch_range_bounds)
            .map(|(branch_loc, bounds)| RangeRevisionSpec::BranchRange { branch_loc, bounds }),
        patch_range_bounds.map(RangeRevisionSpec::Range),
        single_revision_spec.map(RangeRevisionSpec::Single),
    ))
    .parse_next(input)
}

pub(in super::super) fn single_revision_spec(input: &mut &str) -> ModalResult<SingleRevisionSpec> {
    alt((
        (branch_prefix, patch_like_spec).map(|(branch_loc, patch_like)| {
            SingleRevisionSpec::Branch {
                branch_loc,
                patch_like,
            }
        }),
        patch_and_or_git_like_spec,
    ))
    .parse_next(input)
}

fn branch_prefix(input: &mut &str) -> ModalResult<BranchLocator> {
    terminated(branch_locator, ':').parse_next(input)
}

#[derive(Debug)]
struct PrevCheckoutError(&'static str);

impl std::fmt::Display for PrevCheckoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::error::Error for PrevCheckoutError {}

pub(crate) fn branch_locator(input: &mut &str) -> ModalResult<BranchLocator> {
    alt((
        delimited("@{-", digit1, "}")
            .try_map(|s: &str| s.parse::<usize>())
            .try_map(|n| {
                if n > 0 {
                    Ok(BranchLocator::PrevCheckout(n))
                } else {
                    Err(PrevCheckoutError(
                        "`0` is invalid reference to previous checkout",
                    ))
                }
            }),
        partial_ref_name.map(BranchLocator::Name),
        '-'.map(|_| BranchLocator::PrevCheckout(1)),
    ))
    .parse_next(input)
}

fn patch_and_or_git_like_spec(input: &mut &str) -> ModalResult<SingleRevisionSpec> {
    use std::cmp::Ordering;

    let start_checkpoint = input.checkpoint();
    let patch_like = patch_like_spec.parse_next(input);
    let patch_like_checkpoint = input.checkpoint();
    input.reset(&start_checkpoint);
    let git_like = git_like_spec.parse_next(input);
    let git_like_checkpoint = input.checkpoint();

    match (patch_like, git_like) {
        (Ok(patch_like), Ok(git_like)) => {
            // The input looks like both a git revision spec and a patch-like revision spec.
            // If they consumed the same amount of input, then both are passed along to be
            // figured out at resolve-time. Otherwise, the one that consumed the most input
            // is declared the winner. E.g. "foo/bar" would be git-like.
            match patch_like_checkpoint.cmp(&git_like_checkpoint) {
                Ordering::Equal => Ok(SingleRevisionSpec::PatchAndGitLike(patch_like, git_like)),
                Ordering::Less => {
                    input.reset(&patch_like_checkpoint);
                    Ok(SingleRevisionSpec::PatchLike(patch_like))
                }
                Ordering::Greater => Ok(SingleRevisionSpec::GitLike(git_like)),
            }
        }
        (Ok(patch_like), Err(_)) => {
            input.reset(&patch_like_checkpoint);
            Ok(SingleRevisionSpec::PatchLike(patch_like))
        }
        (Err(_), Ok(git_like)) => Ok(SingleRevisionSpec::GitLike(git_like)),
        (Err(e), Err(_)) => Err(e),
    }
}

pub(in super::super) fn patch_like_spec(input: &mut &str) -> ModalResult<PatchLikeSpec> {
    (patch_locator, git_revision_suffix)
        .map(|(patch_loc, suffix)| PatchLikeSpec { patch_loc, suffix })
        .parse_next(input)
}

fn git_like_spec(input: &mut &str) -> ModalResult<String> {
    (partial_ref_name, git_revision_suffix)
        .take()
        .map(|s| s.to_string())
        .parse_next(input)
}

fn git_revision_suffix(input: &mut &str) -> ModalResult<GitRevisionSuffix> {
    repeat::<_, _, Vec<&str>, _, _>(
        0..,
        alt((
            at_braced,
            caret_braced,
            caret_number.take(),
            tilde_number.take(),
        )),
    )
    .take()
    .parse_next(input)
    .map(|suffix: &str| GitRevisionSuffix(suffix.to_string()))
}

fn at_braced<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    preceded('@', braced).parse_next(input)
}

fn caret_braced<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    preceded('^', braced).parse_next(input)
}

fn caret_number(input: &mut &str) -> ModalResult<Option<usize>> {
    preceded('^', opt(unsigned_int)).parse_next(input)
}

pub(crate) fn tilde_number(input: &mut &str) -> ModalResult<Option<usize>> {
    preceded('~', opt(unsigned_int)).parse_next(input)
}

fn braced<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    delimited(
        '{',
        take_escaped(none_of(['\\', '{', '}']), '\\', one_of(['\\', '{', '}'])),
        '}',
    )
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_git_revision_suffix() {
        assert_eq!(
            git_revision_suffix.parse_peek(""),
            Ok(("", GitRevisionSuffix(String::from(""))))
        );
        assert_eq!(
            git_revision_suffix.parse_peek("@{abc}"),
            Ok(("", GitRevisionSuffix(String::from("@{abc}"))))
        );
        assert_eq!(
            git_revision_suffix.parse_peek("@{abc}^12"),
            Ok(("", GitRevisionSuffix(String::from("@{abc}^12"))))
        );
        assert_eq!(
            git_revision_suffix.parse_peek("@{a\\}bc}^12"),
            Ok(("", GitRevisionSuffix(String::from("@{a\\}bc}^12"))))
        );
    }

    #[test]
    fn test_caret_number() {
        assert_eq!(caret_number.parse_peek("^123"), Ok(("", Some(123usize))));
        assert_eq!(caret_number.parse_peek("^0"), Ok(("", Some(0usize))));
        assert_eq!(caret_number.parse_peek("^"), Ok(("", None)));
        assert_eq!(caret_number.parse_peek("^-123"), Ok(("-123", None)));
    }

    #[test]
    fn test_at_braced() {
        assert_eq!(at_braced.parse_peek("@{}"), Ok(("", "")));
        assert_eq!(at_braced.parse_peek("@{abc}"), Ok(("", "abc")));
        assert!(at_braced.parse_peek("@").is_err());
    }

    #[test]
    fn test_caret_braced() {
        assert_eq!(caret_braced.parse_peek("^{abc}"), Ok(("", "abc")));
    }

    #[test]
    fn test_braced() {
        assert_eq!(braced.parse_peek("{}"), Ok(("", "")));
        assert_eq!(braced.parse_peek("{abc}"), Ok(("", "abc")));
        assert_eq!(braced.parse_peek("{abc}bye"), Ok(("bye", "abc")));
        assert_eq!(braced.parse_peek("{a}bc}bye"), Ok(("bc}bye", "a")));
        assert_eq!(braced.parse_peek(r"{a\}bc}bye"), Ok(("bye", r"a\}bc")));
        assert_eq!(
            braced.parse_peek(r"{abc 123 \{\}+}bye"),
            Ok(("bye", r"abc 123 \{\}+"))
        );
        assert_eq!(
            braced.parse_peek(r"{ab\\c\}d\{e}"),
            Ok(("", r"ab\\c\}d\{e"))
        )
    }
}
