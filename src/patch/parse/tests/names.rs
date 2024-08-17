// SPDX-License-Identifier: GPL-2.0-only

use winnow::Parser;

use super::name;
use crate::patch::parse::name::patch_name;

#[test]
fn name_parsing() {
    assert_eq!(patch_name.parse_peek("patch"), Ok(("", name("patch"))));
    assert_eq!(patch_name.parse_peek("patch."), Ok((".", name("patch"))));
    assert_eq!(patch_name.parse_peek("p.atch"), Ok(("", name("p.atch"))));
    assert_eq!(patch_name.parse_peek("p..atch"), Ok(("..atch", name("p"))));
    assert_eq!(
        patch_name.parse_peek("patch name"),
        Ok((" name", name("patch")))
    );
    assert_eq!(
        patch_name.parse_peek("patch@{17}"),
        Ok(("@{17}", name("patch")))
    );
    assert_eq!(
        patch_name.parse_peek("patch@.{17}"),
        Ok(("", name("patch@.{17}")))
    );
    assert_eq!(
        patch_name.parse_peek("patch@.@{17}"),
        Ok((".@{17}", name("patch@")))
    );
    assert_eq!(patch_name.parse_peek("patch@"), Ok(("", name("patch@"))));
    assert_eq!(patch_name.parse_peek("\\-patch"), Ok(("", name("-patch"))));
    assert_eq!(
        patch_name.parse_peek("pa\\-tch"),
        Ok(("\\-tch", name("pa")))
    );
    assert_eq!(patch_name.parse_peek("-pa-tch"), Ok(("", name("-pa-tch"))));
    assert_eq!(patch_name.parse_peek("-"), Ok(("", name("-"))));
    assert_eq!(patch_name.parse_peek("\\-"), Ok(("", name("-"))));
    assert_eq!(patch_name.parse_peek("@@"), Ok(("", name("@@"))));
    assert_eq!(patch_name.parse_peek("@17"), Ok(("", name("@17"))));
    assert_eq!(patch_name.parse_peek("@-3"), Ok(("", name("@-3"))));
    assert_eq!(patch_name.parse_peek("@@@{17}"), Ok(("@{17}", name("@@"))));
    assert_eq!(
        patch_name.parse_peek("@.@@{17}"),
        Ok(("@{17}", name("@.@")))
    );
    assert_eq!(
        patch_name.parse_peek("@.lock.@@{17}"),
        Ok(("@{17}", name("@.lock.@")))
    );
    assert_eq!(patch_name.parse_peek("{base}@"), Ok(("", name("{base}@"))));
    assert_eq!(patch_name.parse_peek("patch^2"), Ok(("^2", name("patch"))));
    assert_eq!(patch_name.parse_peek("∅"), Ok(("", name("∅"))));
    assert_eq!(patch_name.parse_peek("∅^2"), Ok(("^2", name("∅"))));
}
