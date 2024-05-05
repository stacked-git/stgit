// SPDX-License-Identifier: GPL-2.0-only

//! The [`StackStateAccess`] trait allows uniform access to stack information for
//! stack-like objects.

use std::rc::Rc;

use super::{
    iter::{AllPatches, BothPatches},
    state::PatchState,
};
use crate::patch::{LocationConstraint, LocationGroup, PatchName};

/// Trait for accessing information about a stack, including its parent branch.
///
/// This is a super-trait of [`StackStateAccess`].
pub(crate) trait StackAccess<'repo>: StackStateAccess<'repo> {
    /// Get branch name.
    ///
    /// This is the branch's short name, e.g. "main".
    fn get_branch_name(&self) -> &str;

    /// Get full reference name of branch.
    ///
    /// E.g. "refs/heads/main".
    fn get_branch_refname(&self) -> &gix::refs::FullNameRef;

    /// Get reference name of the stack metadata.
    ///
    /// E.g. "refs/stacks/main".
    fn get_stack_refname(&self) -> &str;

    /// Get branch's head commit.
    ///
    /// May be different than the stack's base commit if the stack state is out of sync
    /// with the branch due to use of external tools (i.e. `git`).
    fn get_branch_head(&self) -> &Rc<gix::Commit<'repo>>;

    /// Get stack's base commit.
    fn base(&self) -> &Rc<gix::Commit<'repo>>;
}

/// Trait for accessing stack state.
///
/// Both `Stack` and `StackTransaction` implement this interface.
pub(crate) trait StackStateAccess<'repo> {
    /// Get slice of applied patch names.
    fn applied(&self) -> &[PatchName];

    /// Get slice of unapplied patch names.
    fn unapplied(&self) -> &[PatchName];

    /// Get slice of hidden patch names.
    fn hidden(&self) -> &[PatchName];

    /// Get patch state for given patch name.
    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo>;

    /// Test whether given patch name exists in the stack.
    ///
    /// N.B. use [`StackStateAccess::collides()`] to test for potential patch name
    /// collisions.
    fn has_patch(&self, patchname: &PatchName) -> bool;

    /// Test whether given patch name collides with an existing patch name.
    ///
    /// A patch name collides if it is different from only by case from a patch in the
    /// stack.
    fn collides(&self, patchname: &PatchName) -> Option<&PatchName> {
        self.all_patches().find(|pn| patchname.collides(pn))
    }

    /// Get stack's top commit, or base if no applied patches.
    fn top(&self) -> &Rc<gix::Commit<'repo>>;

    /// Get recorded head of the stack.
    ///
    /// N.B. this is probably not what you want. See also
    /// [`crate::stack::Stack::branch_head`].
    fn head(&self) -> &Rc<gix::Commit<'repo>>;

    /// Get the commit for the given patch name.
    fn get_patch_commit(&self, patchname: &PatchName) -> &Rc<gix::Commit<'repo>> {
        &self.get_patch(patchname).commit
    }

    /// Get the commit id for the given patch name.
    fn get_patch_commit_id(&self, patchname: &PatchName) -> gix::ObjectId {
        self.get_patch_commit(patchname).id
    }

    /// Test whether given patch name is applied.
    fn is_applied(&self, patchname: &PatchName) -> bool {
        self.applied().contains(patchname)
    }

    /// Test whether given patch name is unapplied.
    fn is_unapplied(&self, patchname: &PatchName) -> bool {
        self.unapplied().contains(patchname)
    }

    /// Test whether given patch name is hidden.
    fn is_hidden(&self, patchname: &PatchName) -> bool {
        self.hidden().contains(patchname)
    }

    /// Iterator over all patch names: applied, unapplied, and hidden.
    fn all_patches(&self) -> AllPatches<'_> {
        AllPatches::new(self.applied(), self.unapplied(), self.hidden())
    }

    /// Iterator over applied and unapplied patch names.
    fn applied_and_unapplied(&self) -> BothPatches<'_> {
        BothPatches::new(self.applied(), self.unapplied())
    }

    /// Return absolute index of patch in stack.
    fn index_of(&self, patchname: &PatchName) -> usize {
        self.all_patches()
            .position(|pn| pn == patchname)
            .expect("patchname must exist")
    }

    /// Signed distance from a patch to an optional reference patch in the stack.
    ///
    /// When the reference patchname is `None`, the distance is taken from the stack
    /// base.
    fn distance_from(&self, patchname: &PatchName, ref_patchname: Option<&PatchName>) -> isize {
        if Some(patchname) == ref_patchname {
            return 0;
        }

        let mut iter = self.all_patches().enumerate();
        let (index0, is_patchname_first) = iter
            .find_map(|(i, pn)| {
                if pn == patchname {
                    Some((i, true))
                } else if Some(pn) == ref_patchname {
                    Some((i, false))
                } else {
                    None
                }
            })
            .expect("patchname must be in stack");

        if is_patchname_first {
            if let Some(ref_patchname) = ref_patchname {
                let index1 = iter
                    .find_map(|(i, pn)| (pn == ref_patchname).then_some(i))
                    .expect("ref_patchname must be in stack");
                let distance: isize = (index1 - index0).try_into().expect("distance fits isize");
                -distance
            } else {
                // No reference patch, so measuring from stack base.
                (index0 + 1).try_into().expect("index fits isize")
            }
        } else {
            let index1 = iter
                .find_map(|(i, pn)| (pn == patchname).then_some(i))
                .expect("patchname must be in stack");
            (index1 - index0).try_into().expect("distance fits isize")
        }
    }

    fn location_group(&self, patchname: &PatchName) -> LocationGroup {
        if self.applied().contains(patchname) {
            LocationGroup::Applied
        } else if self.unapplied().contains(patchname) {
            LocationGroup::Unapplied
        } else if self.hidden().contains(patchname) {
            LocationGroup::Hidden
        } else {
            panic!("BUG: location_group() must be called with known patch name")
        }
    }

    fn get_allowed(&self, constraint: LocationConstraint) -> Vec<&PatchName> {
        match constraint {
            LocationConstraint::All => self.all_patches().collect(),
            LocationConstraint::Visible => self.applied_and_unapplied().collect(),
            LocationConstraint::Applied => self.applied().iter().collect(),
            LocationConstraint::Unapplied => self.unapplied().iter().collect(),
            LocationConstraint::Hidden => self.hidden().iter().collect(),
        }
    }
}
