// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`PatchId`].

use super::PatchId;

impl std::fmt::Display for PatchId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatchId::Name(patchname) => patchname.fmt(f),
            PatchId::Base => "{base}".fmt(f),
            PatchId::Top => '@'.fmt(f),
            PatchId::BelowTop(Some(n)) => format!("~{n}").fmt(f),
            PatchId::BelowTop(None) => '~'.fmt(f),
            PatchId::BelowLast(None) => '^'.fmt(f),
            PatchId::BelowLast(Some(n)) => format!("^{n}").fmt(f),
        }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn format_patch_ids() {
        let pn = PatchId::Name(crate::patch::PatchName::from_str("p1").unwrap());
        assert_eq!("p1  ", format!("{:4}", pn));
        assert_eq!("@   ", format!("{:4}", PatchId::Top));
        assert_eq!("~   ", format!("{:4}", PatchId::BelowTop(None)));
        assert_eq!("~1  ", format!("{:4}", PatchId::BelowTop(Some(1))));
        assert_eq!("^   ", format!("{:4}", PatchId::BelowLast(None)));
        assert_eq!("^1  ", format!("{:4}", PatchId::BelowLast(Some(1))));
    }
}
