// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`PatchId`].

use std::fmt::Write;

use super::PatchId;

impl std::fmt::Display for PatchId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatchId::Name(patchname) => write!(f, "{patchname}"),
            PatchId::Base => f.write_str("{base}"),
            PatchId::Top => f.write_char('@'),
            PatchId::BelowTop(Some(n)) => write!(f, "~{n}"),
            PatchId::BelowTop(None) => f.write_char('~'),
            PatchId::BelowLast(None) => f.write_char('^'),
            PatchId::BelowLast(Some(n)) => write!(f, "^{n}"),
        }
    }
}
