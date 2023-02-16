// SPDX-License-Identifier: GPL-2.0-only

use crate::patch::{PatchId, PatchLocator, PatchRange, PatchRangeBounds};

use super::super::patch_range;
use super::{name, offsets};

#[test]
fn range_parsing() {
    assert_eq!(
        patch_range("patch").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets("")
            })
        )
    );
    assert_eq!(
        patch_range("patch~3").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::Name(name("patch")),
                offsets: offsets("~3"),
            })
        )
    );
    assert_eq!(
        patch_range("~3").unwrap(),
        (
            "",
            PatchRange::Single(PatchLocator {
                id: PatchId::BelowTop(Some(3)),
                offsets: offsets(""),
            })
        )
    );
    assert_eq!(
        patch_range("patch~3..patch").unwrap(),
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
        patch_range("@~~~..").unwrap(),
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
        patch_range("@~~~..patch{17}").unwrap(),
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
        patch_range("^3..^").unwrap(),
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
