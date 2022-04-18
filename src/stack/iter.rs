//! Patch name iterators.

use std::iter::Chain;
use std::slice::Iter;

use crate::patchname::PatchName;

/// Iterator over all patches (applied, unapplied, and hidden).
pub(crate) struct AllPatches<'s>(
    Chain<Chain<Iter<'s, PatchName>, Iter<'s, PatchName>>, Iter<'s, PatchName>>,
);

impl<'s> AllPatches<'s> {
    pub(crate) fn new(
        applied: &'s [PatchName],
        unapplied: &'s [PatchName],
        hidden: &'s [PatchName],
    ) -> Self {
        Self(applied.iter().chain(unapplied.iter()).chain(hidden.iter()))
    }
}

impl<'s> Iterator for AllPatches<'s> {
    type Item = &'s PatchName;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

/// Chained iterator over two patch lists, applied and unapplied, or unapplied and
/// hidden.
pub(crate) struct BothPatches<'s>(Chain<Iter<'s, PatchName>, Iter<'s, PatchName>>);

impl<'s> BothPatches<'s> {
    pub(crate) fn new(a: &'s [PatchName], b: &'s [PatchName]) -> Self {
        Self(a.iter().chain(b.iter()))
    }
}

impl<'s> Iterator for BothPatches<'s> {
    type Item = &'s PatchName;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
