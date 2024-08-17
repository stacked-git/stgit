// SPDX-License-Identifier: GPL-2.0-only

use winnow::{error::ErrMode, Parser};

use super::{
    super::{
        oid_prefix_offsets, patch_locator, patch_offset_atom, patch_offset_atoms, patch_offsets,
    },
    name, offsets,
};
use crate::patch::{PatchId, PatchLocator, PatchOffsetAtom, PatchOffsets};

fn good_locator(s: &str) -> (&str, PatchLocator) {
    patch_locator.parse_peek(s).expect("valid patch locator")
}

fn complete_locator(s: &str) -> PatchLocator {
    let (rest, patch_loc) = good_locator(s);
    assert!(rest.is_empty());
    patch_loc
}

#[test]
fn offset_parsing() {
    assert_eq!(
        patch_offset_atom.parse_peek("+1~2"),
        Ok(("~2", PatchOffsetAtom::Plus(Some(1)))),
    );
    assert_eq!(
        patch_offset_atoms.parse_peek("+~"),
        Ok((
            "",
            vec![PatchOffsetAtom::Plus(None), PatchOffsetAtom::Tilde(None)]
        )),
    );
    assert_eq!(
        patch_offset_atoms.parse_peek("+2~3++~4~"),
        Ok((
            "",
            vec![
                PatchOffsetAtom::Plus(Some(2)),
                PatchOffsetAtom::Tilde(Some(3)),
                PatchOffsetAtom::Plus(None),
                PatchOffsetAtom::Plus(None),
                PatchOffsetAtom::Tilde(Some(4)),
                PatchOffsetAtom::Tilde(None),
            ]
        ))
    );
    assert_eq!(
        patch_offsets.parse_peek("+2~3"),
        Ok(("", PatchOffsets("+2~3".to_string())))
    );
    assert_eq!(
        patch_offsets.parse_peek(""),
        Ok(("", PatchOffsets("".to_string())))
    );
}

#[test]
fn locator_errors() {
    assert!(matches!(
        patch_locator.parse_peek(".patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek(".."),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("..patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("*patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("[patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("/patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("?patch"),
        Err(ErrMode::Backtrack(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek(" patch"),
        Err(ErrMode::Backtrack(_))
    ));
}

#[test]
fn locator_failures() {
    assert!(matches!(
        patch_locator.parse_peek("patch.lock"),
        Err(ErrMode::Cut(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("patch.lock~"),
        Err(ErrMode::Cut(_))
    ));
    assert!(matches!(
        patch_locator.parse_peek("patch.lock@{-3}"),
        Err(ErrMode::Cut(_))
    ));
}

#[test]
fn special_locators() {
    assert_eq!(
        complete_locator("{base}"),
        PatchLocator {
            id: PatchId::Base,
            offsets: offsets(""),
        }
    );
    assert_eq!(
        good_locator("{base}.@{17}"),
        (
            ".@{17}",
            PatchLocator {
                id: PatchId::Base,
                offsets: offsets(""),
            }
        )
    );

    assert_eq!(
        complete_locator("@"),
        PatchLocator {
            id: PatchId::Top,
            offsets: offsets(""),
        }
    );
    assert_eq!(
        good_locator("@."),
        (
            ".",
            PatchLocator {
                id: PatchId::Top,
                offsets: offsets(""),
            }
        )
    );
    assert_eq!(
        good_locator("@.."),
        (
            "..",
            PatchLocator {
                id: PatchId::Top,
                offsets: offsets(""),
            }
        )
    );
    assert_eq!(
        good_locator("@{17}"),
        (
            "{17}",
            PatchLocator {
                id: PatchId::Top,
                offsets: offsets(""),
            }
        )
    );
}

#[test]
fn locator_parsing() {
    assert_eq!(
        complete_locator("patch"),
        PatchLocator {
            id: PatchId::Name(name("patch")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("patch{17}"),
        PatchLocator {
            id: PatchId::Name(name("patch{17}")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("patch~1"),
        PatchLocator {
            id: PatchId::Name(name("patch")),
            offsets: offsets("~1"),
        },
    );
    assert_eq!(
        good_locator("patch~1~~+.."),
        (
            "..",
            PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets("~1~~+"),
            }
        ),
    );
    assert_eq!(
        good_locator("patch@{17"),
        (
            "@{17",
            PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets(""),
            }
        )
    );
    assert_eq!(
        complete_locator("patch@"),
        PatchLocator {
            id: PatchId::Name(name("patch@")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("@+3"),
        PatchLocator {
            id: PatchId::Name(name("@+3")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("@~3"),
        PatchLocator {
            id: PatchId::Top,
            offsets: offsets("~3"),
        },
    );
    assert_eq!(
        good_locator("@@{-3}"),
        (
            "@{-3}",
            PatchLocator {
                id: PatchId::Top,
                offsets: offsets(""),
            }
        )
    );
    assert_eq!(
        good_locator("@@@{-3}"),
        (
            "@{-3}",
            PatchLocator {
                id: PatchId::Name(name("@@")),
                offsets: offsets(""),
            }
        )
    );
    assert_eq!(
        complete_locator("+3"),
        PatchLocator {
            id: PatchId::Name(name("+3")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("-3"),
        PatchLocator {
            id: PatchId::Name(name("-3")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("\\-3"),
        PatchLocator {
            id: PatchId::Name(name("-3")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("~3"),
        PatchLocator {
            id: PatchId::BelowTop(Some(3)),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("3"),
        PatchLocator {
            id: PatchId::Name(name("3")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        good_locator("3@{-1}"),
        (
            "@{-1}",
            PatchLocator {
                id: PatchId::Name(name("3")),
                offsets: offsets(""),
            }
        ),
    );
    assert_eq!(
        complete_locator("3~~~"),
        PatchLocator {
            id: PatchId::Name(name("3")),
            offsets: offsets("~~~"),
        },
    );
    assert_eq!(
        good_locator("3~~~-3"),
        (
            "-3",
            PatchLocator {
                id: PatchId::Name(name("3")),
                offsets: offsets("~~~"),
            }
        ),
    );
    assert_eq!(
        complete_locator("~7~"),
        PatchLocator {
            id: PatchId::BelowTop(Some(7)),
            offsets: offsets("~"),
        }
    );
    assert_eq!(
        complete_locator("^"),
        PatchLocator {
            id: PatchId::BelowLast(None),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("^3"),
        PatchLocator {
            id: PatchId::BelowLast(Some(3)),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("^-3"),
        PatchLocator {
            id: PatchId::BelowLast(Some(-3)),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("^-3++"),
        PatchLocator {
            id: PatchId::BelowLast(Some(-3)),
            offsets: offsets("++"),
        }
    );
    assert_eq!(
        complete_locator("abcd"),
        PatchLocator {
            id: PatchId::Name(name("abcd")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("abc"),
        PatchLocator {
            id: PatchId::Name(name("abc")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("abcd~~"),
        PatchLocator {
            id: PatchId::Name(name("abcd")),
            offsets: offsets("~~"),
        }
    );
    assert_eq!(
        complete_locator("1234"),
        PatchLocator {
            id: PatchId::Name(name("1234")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("123"),
        PatchLocator {
            id: PatchId::Name(name("123")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("ABC123"),
        PatchLocator {
            id: PatchId::Name(name("ABC123")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        complete_locator("abcde12345abcde12345abcde12345abcde12345"),
        PatchLocator {
            id: PatchId::Name(name("abcde12345abcde12345abcde12345abcde12345")),
            offsets: offsets(""),
        }
    );
    assert_eq!(
        // Too long to be an oid prefix
        complete_locator("abcde12345abcde12345abcde12345abcde12345f"),
        PatchLocator {
            id: PatchId::Name(name("abcde12345abcde12345abcde12345abcde12345f")),
            offsets: offsets(""),
        }
    );
}

#[test]
fn non_ascii_locators() {
    assert_eq!(
        complete_locator("pat♡ch™"),
        PatchLocator {
            id: PatchId::Name(name("pat♡ch™")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        complete_locator("pat♡ch™~3"),
        PatchLocator {
            id: PatchId::Name(name("pat♡ch™")),
            offsets: offsets("~3"),
        },
    );
    assert_eq!(
        complete_locator("♡++"),
        PatchLocator {
            id: PatchId::Name(name("♡++")),
            offsets: offsets(""),
        },
    );
    assert_eq!(
        good_locator("♡.."),
        (
            "..",
            PatchLocator {
                id: PatchId::Name(name("♡")),
                offsets: offsets(""),
            },
        )
    );
}

#[test]
fn oid_prefix_parsing() {
    let prefix = |s| gix::hash::Prefix::from_hex(s).expect("valid hex hash prefix");
    assert_eq!(
        oid_prefix_offsets.parse_peek("abc123").unwrap(),
        ("", (prefix("abc123"), offsets("")))
    );
    assert_eq!(
        oid_prefix_offsets.parse_peek("abc123FEDCBA00").unwrap(),
        ("", (prefix("abc123fedcba00"), offsets("")))
    );
    assert_ne!(
        oid_prefix_offsets
            .parse_peek("abc123FEDCBA000")
            .unwrap()
            .1
             .0,
        prefix("abc123fedcba")
    );
    assert_eq!(
        oid_prefix_offsets.parse_peek("123abc~~++").unwrap(),
        ("", (prefix("123abc"), offsets("~~++")))
    );
    assert_eq!(
        oid_prefix_offsets.parse_peek("123abc..987defg").unwrap(),
        ("..987defg", (prefix("123abc"), offsets("")))
    );
    assert!(matches!(
        oid_prefix_offsets.parse_peek("123"),
        Err(ErrMode::Backtrack(_)),
    ));
    assert!(matches!(
        oid_prefix_offsets.parse_peek("0123456789012345678901234567890123456789eeee"),
        Err(ErrMode::Backtrack(_)),
    ));
}
