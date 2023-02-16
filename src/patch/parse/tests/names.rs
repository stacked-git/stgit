// SPDX-License-Identifier: GPL-2.0-only

use crate::patch::parse::name::patch_name;

use super::name;

#[test]
fn name_parsing() {
    assert_eq!(patch_name("patch"), Ok(("", name("patch"))));
    assert_eq!(patch_name("patch."), Ok((".", name("patch"))));
    assert_eq!(patch_name("p.atch"), Ok(("", name("p.atch"))));
    assert_eq!(patch_name("p..atch"), Ok(("..atch", name("p"))));
    assert_eq!(patch_name("patch name"), Ok((" name", name("patch"))));
    assert_eq!(patch_name("patch@{17}"), Ok(("@{17}", name("patch"))));
    assert_eq!(patch_name("patch@.{17}"), Ok(("", name("patch@.{17}"))));
    assert_eq!(patch_name("patch@.@{17}"), Ok((".@{17}", name("patch@"))));
    assert_eq!(patch_name("patch@"), Ok(("", name("patch@"))));
    assert_eq!(patch_name("\\-patch"), Ok(("", name("-patch"))));
    assert_eq!(patch_name("pa\\-tch"), Ok(("\\-tch", name("pa"))));
    assert_eq!(patch_name("-pa-tch"), Ok(("", name("-pa-tch"))));
    assert_eq!(patch_name("-"), Ok(("", name("-"))));
    assert_eq!(patch_name("\\-"), Ok(("", name("-"))));
    assert_eq!(patch_name("@@"), Ok(("", name("@@"))));
    assert_eq!(patch_name("@17"), Ok(("", name("@17"))));
    assert_eq!(patch_name("@-3"), Ok(("", name("@-3"))));
    assert_eq!(patch_name("@@@{17}"), Ok(("@{17}", name("@@"))));
    assert_eq!(patch_name("@.@@{17}"), Ok(("@{17}", name("@.@"))));
    assert_eq!(patch_name("@.lock.@@{17}"), Ok(("@{17}", name("@.lock.@"))));
    assert_eq!(patch_name("{base}@"), Ok(("", name("{base}@"))));
    assert_eq!(patch_name("patch^2"), Ok(("^2", name("patch"))));
    assert_eq!(patch_name("∅"), Ok(("", name("∅"))));
    assert_eq!(patch_name("∅^2"), Ok(("^2", name("∅"))));
}
