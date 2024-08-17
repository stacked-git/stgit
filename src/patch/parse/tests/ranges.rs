// SPDX-License-Identifier: GPL-2.0-only

use winnow::Parser;

use super::{super::patch_range, name, offsets};
use crate::patch::{PatchId, PatchLocator, PatchRange, PatchRangeBounds};

#[test]
fn range_parsing() {
    assert_eq!(
        patch_range.parse_peek("patch").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets("")
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("patch~3").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets("~3"),
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("~3").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::BelowTop(Some(3)),
                offsets: offsets(""),
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("patch~3..patch").unwrap(),
        (
            "",
            PatchRange::Range(PatchRangeBounds {
                begin: Some(PatchLocator {
                    id: PatchId::Name(name("patch")),
                    offsets: offsets("~3"),
                }),
                end: Some(PatchLocator {
                    id: PatchId::Name(name("patch")),
                    offsets: offsets(""),
                })
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("@~~~..").unwrap(),
        (
            "",
            PatchRange::Range(PatchRangeBounds {
                begin: Some(PatchLocator {
                    id: PatchId::Top,
                    offsets: offsets("~~~"),
                }),
                end: None
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("@~~~..patch{17}").unwrap(),
        (
            "",
            PatchRange::Range(PatchRangeBounds {
                begin: Some(PatchLocator {
                    id: PatchId::Top,
                    offsets: offsets("~~~"),
                }),
                end: Some(PatchLocator {
                    id: PatchId::Name(name("patch{17}")),
                    offsets: offsets(""),
                })
            })
        )
    );
    assert_eq!(
        patch_range.parse_peek("^3..^").unwrap(),
        (
            "",
            PatchRange::Range(PatchRangeBounds {
                begin: Some(PatchLocator {
                    id: PatchId::BelowLast(Some(3)),
                    offsets: offsets(""),
                }),
                end: Some(PatchLocator {
                    id: PatchId::BelowLast(None),
                    offsets: offsets(""),
                })
            })
        )
    );
}
