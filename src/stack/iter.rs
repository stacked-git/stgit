use std::iter::Chain;
use std::slice::Iter;

use super::state::StackState;
use crate::patchname::PatchName;

pub(crate) struct AllPatches<'s>(
    Chain<Chain<Iter<'s, PatchName>, Iter<'s, PatchName>>, Iter<'s, PatchName>>,
);

impl<'s> AllPatches<'s> {
    pub(crate) fn new(state: &'s StackState) -> Self {
        Self(
            state
                .applied
                .iter()
                .chain(state.unapplied.iter())
                .chain(state.hidden.iter()),
        )
    }
}

impl<'s> Iterator for AllPatches<'s> {
    type Item = &'s PatchName;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
