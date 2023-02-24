// SPDX-License-Identifier: GPL-2.0-only

mod locators;
mod names;
mod ranges;
mod revisions;

use std::str::FromStr;

use nom::combinator::opt;

use super::numbers::unsigned_int;
use crate::patch::{PatchName, PatchOffsets};

fn name(s: &str) -> PatchName {
    PatchName::from_str(s).expect("valid patch name")
}

fn offsets(s: &str) -> PatchOffsets {
    PatchOffsets::from_str(s).expect("valid patch offsets")
}

#[test]
fn number_stuff() {
    assert_eq!(opt(unsigned_int)(""), Ok(("", None)));
    assert_eq!(
        opt(unsigned_int)("0000000000000000000000017"),
        Ok(("", Some(17)))
    );
    assert_eq!(opt(unsigned_int)("1234"), Ok(("", Some(1234))));
    assert_eq!(opt(unsigned_int)("-1234"), Ok(("-1234", None)));
    assert_eq!(opt(unsigned_int)("1234abc"), Ok(("abc", Some(1234))));
    assert_eq!(opt(unsigned_int)("abc"), Ok(("abc", None)));
}
