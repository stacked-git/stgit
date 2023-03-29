// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`PatchOffsets`] and [`PatchOffsetAtom`].

use std::{fmt::Write, str::FromStr};

use super::{PatchOffsetAtom, PatchOffsets};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("invalid patch offsets `{0}`")]
    InvalidPatchOffsets(String),
}

impl std::fmt::Display for PatchOffsetAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (prefix, maybe_n) = match self {
            PatchOffsetAtom::Plus(n) => ('+', n),
            PatchOffsetAtom::Tilde(n) => ('~', n),
        };
        if let Some(n) = maybe_n {
            write!(f, "{prefix}{n}")
        } else {
            f.write_char(prefix)
        }
    }
}

impl std::fmt::Display for PatchOffsets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for PatchOffsets {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(super::parse::patch_offsets))(s)
            .map(|(_, offsets)| offsets)
            .map_err(|_| Error::InvalidPatchOffsets(s.to_string()))
    }
}

impl AsRef<str> for PatchOffsets {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl PatchOffsets {
    /// Predicate indicating whether the offsets string contains any offset atoms.
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Concatenate with another [`PatchOffsets`], creating a new instance.
    pub(super) fn join(self, other: &Self) -> Self {
        Self([self.0.as_str(), other.0.as_str()].concat())
    }

    /// Construct constituent [`PatchOffsetAtom`] instances.
    pub(super) fn atoms(&self) -> Vec<PatchOffsetAtom> {
        super::parse::patch_offset_atoms(&self.0)
            .expect("previously validated")
            .1
    }
}
