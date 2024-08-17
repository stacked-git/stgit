// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

use winnow::Parser;

use super::{name, offsets};
use crate::{
    branchloc::BranchLocator,
    patch::{
        parse::{branch_locator, single_revision_spec, tilde_number},
        GitRevisionSuffix, PatchId, PatchLikeSpec, PatchLocator, SingleRevisionSpec,
    },
    wrap::PartialRefName,
};

#[test]
fn tilde_stuff() {
    assert_eq!(tilde_number.parse_peek("~"), Ok(("", None)));
    assert_eq!(tilde_number.parse_peek("~1"), Ok(("", Some(1))));
    assert_eq!(tilde_number.parse_peek("~001"), Ok(("", Some(1))));
    assert_eq!(tilde_number.parse_peek("~1~~"), Ok(("~~", Some(1))));
    assert_eq!(tilde_number.parse_peek("~~~"), Ok(("~~", None)));
    assert_eq!(tilde_number.parse_peek("~abc"), Ok(("abc", None)));
    assert_eq!(tilde_number.parse_peek("~-1"), Ok(("-1", None)));
    assert!(tilde_number.parse_peek("").is_err());
    assert!(tilde_number.parse_peek("123").is_err());
    assert!(tilde_number.parse_peek("-1").is_err());
}

#[test]
fn single_specs() {
    assert_eq!(
        single_revision_spec.parse("^").unwrap(),
        SingleRevisionSpec::PatchLike(PatchLikeSpec {
            patch_loc: PatchLocator {
                id: PatchId::BelowLast(None),
                offsets: offsets("")
            },
            suffix: GitRevisionSuffix(String::from(""))
        })
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo").unwrap(),
        (
            "",
            SingleRevisionSpec::PatchAndGitLike(
                PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("foo")),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from(""))
                },
                String::from("foo")
            )
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo@{+1}").unwrap(),
        (
            "",
            SingleRevisionSpec::PatchAndGitLike(
                PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("foo")),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from("@{+1}"))
                },
                String::from("foo@{+1}")
            )
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("abc123").unwrap(),
        (
            "",
            SingleRevisionSpec::PatchAndGitLike(
                PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("abc123")),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from(""))
                },
                String::from("abc123")
            )
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("foozle-1.2.3^{}").unwrap(),
        (
            "",
            SingleRevisionSpec::PatchAndGitLike(
                PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("foozle-1.2.3")),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from("^{}"))
                },
                String::from("foozle-1.2.3^{}")
            )
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo/bar").unwrap(),
        ("", SingleRevisionSpec::GitLike(String::from("foo/bar")))
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo/bar:baz~1").unwrap(),
        (
            "",
            SingleRevisionSpec::Branch {
                branch_loc: BranchLocator::Name(PartialRefName("foo/bar".to_string())),
                patch_like: PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("baz")),
                        offsets: offsets("~1")
                    },
                    suffix: GitRevisionSuffix(String::from(""))
                }
            }
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo/bar:baz^").unwrap(),
        (
            "",
            SingleRevisionSpec::Branch {
                branch_loc: BranchLocator::Name(PartialRefName("foo/bar".to_string())),
                patch_like: PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("baz")),
                        offsets: offsets(""),
                    },
                    suffix: GitRevisionSuffix(String::from("^"))
                }
            }
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("foo/bar:^").unwrap(),
        (
            "",
            SingleRevisionSpec::Branch {
                branch_loc: BranchLocator::Name(PartialRefName("foo/bar".to_string())),
                patch_like: PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::BelowLast(None),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from(""))
                }
            }
        )
    );
    assert_eq!(
        single_revision_spec
            .parse_peek("foo:baz^{/search}")
            .unwrap(),
        (
            "",
            SingleRevisionSpec::Branch {
                branch_loc: BranchLocator::Name(PartialRefName(String::from("foo"))),
                patch_like: PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("baz")),
                        offsets: offsets(""),
                    },
                    suffix: GitRevisionSuffix(String::from("^{/search}"))
                }
            }
        )
    );
    assert_eq!(
        single_revision_spec.parse_peek("baz^{/search}").unwrap(),
        (
            "",
            SingleRevisionSpec::PatchAndGitLike(
                PatchLikeSpec {
                    patch_loc: PatchLocator {
                        id: PatchId::Name(name("baz")),
                        offsets: offsets("")
                    },
                    suffix: GitRevisionSuffix(String::from("^{/search}"))
                },
                String::from("baz^{/search}")
            )
        )
    );
}

#[test]
fn branch_locators() {
    assert_eq!(
        branch_locator.parse_peek("name").unwrap(),
        (
            "",
            BranchLocator::Name(PartialRefName::from_str("name").unwrap())
        )
    );

    assert_eq!(
        branch_locator.parse_peek("dir/name").unwrap(),
        (
            "",
            BranchLocator::Name(PartialRefName::from_str("dir/name").unwrap())
        )
    );

    assert_eq!(
        branch_locator.parse_peek("refs/heads/dir/name").unwrap(),
        (
            "",
            BranchLocator::Name(PartialRefName::from_str("refs/heads/dir/name").unwrap())
        )
    );

    assert_eq!(
        branch_locator.parse_peek("@{-1}").unwrap(),
        ("", BranchLocator::PrevCheckout(1),)
    );

    assert_eq!(
        branch_locator.parse_peek("@{-3}").unwrap(),
        ("", BranchLocator::PrevCheckout(3),)
    );

    assert_eq!(
        branch_locator.parse_peek("-").unwrap(),
        ("", BranchLocator::PrevCheckout(1),)
    );

    assert_eq!(
        branch_locator.parse_peek("-123").unwrap(),
        (
            "",
            BranchLocator::Name(PartialRefName::from_str("-123").unwrap())
        )
    );
}
