// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

use super::super::*;

#[test]
fn display_offset_atom() {
    assert_eq!(PatchOffsetAtom::Plus(None).to_string(), "+");
    assert_eq!(PatchOffsetAtom::Plus(Some(1)).to_string(), "+1");
    assert_eq!(PatchOffsetAtom::Plus(Some(0)).to_string(), "+0");
    assert_eq!(PatchOffsetAtom::Plus(Some(42)).to_string(), "+42");

    assert_eq!(PatchOffsetAtom::Tilde(None).to_string(), "~");
    assert_eq!(PatchOffsetAtom::Tilde(Some(1)).to_string(), "~1");
    assert_eq!(PatchOffsetAtom::Tilde(Some(0)).to_string(), "~0");
    assert_eq!(PatchOffsetAtom::Tilde(Some(42)).to_string(), "~42");
}

#[test]
fn display_offset() {
    let check_same = |s| assert_eq!(PatchOffsets::from_str(s).unwrap().as_ref(), s);
    check_same("~+~+1~2");
    check_same("~0");
    check_same("+0");
    check_same("~0017");
}

#[test]
fn display_patch_id() {
    let check_same = |s| assert_eq!(PatchLocator::from_str(s).unwrap().id.to_string(), s);
    check_same("patch");
    check_same("patch+17");
    check_same("{base}");
    check_same("{base}++");
    check_same("@+1");
    check_same("@-1");
    check_same("@@-1");
    check_same("^1");
    check_same("^-3");
    check_same("^0");
    check_same("-1");
    check_same("-0");
    check_same("-000");
    check_same("-001");
    check_same("--3");

    let check_display =
        |s, expected| assert_eq!(PatchLocator::from_str(s).unwrap().id.to_string(), expected);

    check_display("^000", "^0");
    check_display("^001", "^1");
    check_display("^-003", "^-3");
}

#[test]
fn display_locators() {
    let check_same = |s| assert_eq!(PatchLocator::from_str(s).unwrap().to_string(), s);

    check_same("patch-1");
    check_same("patch+2");
    check_same("patch~3");
    check_same("^~+003");
    check_same("^-3~02");
    check_same("{base}++~");
    check_same("@~++");
}

#[test]
fn display_range() {
    let check_same = |s| assert_eq!(PatchRange::from_str(s).unwrap().to_string(), s);

    check_same("abc..def");
    check_same("abc..");
    check_same("..def");
    check_same("..");
    check_same("^..^");
    check_same("^0..^0");
    check_same("{base}..@");
    check_same("@..");
    check_same("..@-3");
    check_same("~1..@~3");
    check_same("~1");
    check_same("patch");
    check_same("patch++~++");
}

#[test]
fn display_single_revisions() {
    let check_same = |s| assert_eq!(SingleRevisionSpec::from_str(s).unwrap().to_string(), s);

    check_same("@");
    check_same("{base}");
    check_same("patch");
    check_same("abc^3");
    check_same("foo/bar/baz:patch~1@{/hi}");
    check_same("name@{push}");
    check_same("name^{}");
    check_same("name@{-1}");
    check_same("{base}^^^");
    check_same("@^");
    check_same("foo/bar");
    check_same("foo/bar^^^");
}

#[test]
fn display_range_revisions() {
    let check_same = |s| assert_eq!(RangeRevisionSpec::from_str(s).unwrap().to_string(), s);

    check_same("foo/bar/baz:@..");
    check_same("foo/bar:..");
    check_same("foo/bar:..@");
    check_same("foo/bar:name~~..^-3++");
    check_same("foo:@^{}");
    check_same("name");
    check_same("name^{u}");
    check_same("name^{}~~~");
    check_same("name+3~1^{}~~~");
}
