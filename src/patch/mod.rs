// SPDX-License-Identifier: GPL-2.0-only

//! Abstractions for specifying patches within a stack.

mod constraint;
pub(crate) mod edit;
mod identifier;
pub(crate) mod locator;
pub(crate) mod name;
mod offset;
pub(crate) mod parse;
pub(crate) mod range;
pub(crate) mod revspec;

#[cfg(test)]
mod tests;

use std::rc::Rc;

use serde::{Deserialize, Serialize};

pub(crate) use self::{edit as patchedit, range as patchrange};
use crate::branchloc::BranchLocator;

/// A range of patches in the stack.
///
/// A patch range is specified on the command line as `[<locator>]..[<locator>]`, e.g.
/// `p0..p3`. The begin and end locators, typically a patch names, are both optional.
/// The begin and end patch locators are inclusive to the range.
///
/// Ranges with open beginnings always start with the bottommost patch, i.e. the first
/// patch after the stack's base commit.
///
/// The last patch in an open-ended range depends on command-specific policy which is
/// determined by the [`RangeConstraint`] used with [`patchrange::resolve_names()`]
/// or [`patchrange::resolve_names_contiguous()`].
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatchRange {
    /// A range consisting of a single patch.
    Single(PatchLocator),
    /// A range bound by optional begin and end patches.
    Range(PatchRangeBounds),
}

/// Patch locations bounding a range of patches.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PatchRangeBounds {
    /// Beginning patch of the range.
    begin: Option<PatchLocator>,
    /// Ending patch of the range. The range includes this patch.
    end: Option<PatchLocator>,
}

/// Location of a patch within the stack.
///
/// A location consists of a patch identifier along with an optional offset. Locations
/// originating from command line arguments are resolved into a patch name using
/// [`PatchLocator::resolve_name()`].
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PatchLocator {
    id: PatchId,
    offsets: PatchOffsets,
}

/// Identifier for a patch or position within the stack.
///
/// The canonical patch identifier is its name. Other identifiers may be specified
/// in command line arguments using special syntax.
///
/// The stack base is spelled `{base}`. Note that a (positive) offset must be supplied
/// in a [`PatchLocator`] using [`PatchId::Base`] since the stack's base is outside the
/// stack and not a patch itself.
///
/// The topmost patch can be explicitly spelled with `@`. The topmost patch is also
/// implicit in locations only containing a relative offset, e.g. `~1` or `+3`.
///
/// The last visible patch is spelled `^` and may be followed by a signed integer
/// indicating an offset in the direction of previous patches; i.e. `^3` would be three
/// patches *before* the last patch and `^-3` would be three patches *after* the last
/// patch, into the hidden patches.
///
/// Absolute indexes into the stack, relative offsets, and commit id prefixes are also
/// valid identifiers.  However, these identifiers are ambiguous with patch names since
/// they cannot be disambiguated syntactically. These ambiguous patch identifiers are
/// disambiguated in the context of the actual stack. When an ambiguous identifier
/// matches a patch name in the stack, it is always interpreted as the patch name. Only
/// if the identifier does not match a patch name in the stack is it interpreted
/// otherwise.
///
/// As an example, `5` could either be a patch name or an absolute index. If the stack
/// contains a patch named `5` then this identifier would refer to the patch named `5`
/// (regardless of its index). If the stack did not contain a patch named `5`, then the
/// identifier would resolve to the sixth patch (index `5`) in the the stack.
#[derive(Clone, Debug, PartialEq)]
enum PatchId {
    Name(PatchName),
    Base,
    Top,
    BelowLast(Option<isize>),
    BelowTop(Option<usize>),
}

/// Offsets from one patch location to another in the stack.
///
/// On the command line, these offsets take the form of concatenations of `+[<n>]` or
/// `~[<n>]` where the optional `<n>` is an unsigned integer.
#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct PatchOffsets(String);

/// An individual offset atoms such as `+`, `~`, `~3`, or `+1`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PatchOffsetAtom {
    /// Offset to the n'th next patch in the stack.
    Plus(Option<usize>),
    /// Offset to the n'th previous patch in the stack.
    Tilde(Option<usize>),
}

/// A [`String`] that follows the patch naming rules.
///
/// A valid patch name must meet all the rules of a git reference name, plus the
/// additional restriction of not containing '/' as well as not being one of the
/// reserved strings `@` or `{base}`.
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct PatchName(pub(self) String);

/// A patch specified by the user on the command line may be constrained to a subset of
/// the stack locations.
#[derive(Clone, Copy, Debug)]
pub(crate) enum LocationConstraint {
    /// All patches within the stack are allowed.
    All,
    /// Only visible (non-hidden) patches are allowed.
    Visible,
    /// Only currently applied patches are allowed.
    Applied,
    /// Only currently unapplied patches are allowed.
    Unapplied,
    /// Only currently hidden patches are allowed.
    Hidden,
}

/// Indicates which patches are allowed in user-supplied patch ranges.
///
/// The [`RangeConstraint::AllWithAppliedBoundary`] and
/// [`RangeConstraint::VisibleWithAppliedBoundary`] variants allow all and visible
/// (applied `+` unapplied), respectively, but constrain open-ended patch ranges to the
/// last applied patch when the beginning of the open range is an applied patch.
#[derive(Clone, Copy, Debug)]
pub(crate) enum RangeConstraint {
    /// All patches within the stack are allowed in the range.
    All,
    /// All patches within the stack are allowed in the range, but open-ended ranges
    /// stop at the last applied patch.
    AllWithAppliedBoundary,
    /// All visible (non-hidden) patches are allowed in the range.
    Visible,
    /// All visible (non-hidden) patches are allowed in the range, but open-ended ranges
    /// stop at the last applied patch.
    VisibleWithAppliedBoundary,
    /// Only applied patches are allowed in the range.
    Applied,
    /// Only unapplied patches are allowed in the range.
    Unapplied,
    /// Only hidden patches are allowed in the range.
    Hidden,
}

/// The groups of patches within the stack.
///
/// The stack consists of all the applied patches, then unapplied, followed by any
/// hidden patches.
#[derive(Debug)]
pub(crate) enum LocationGroup {
    Applied,
    Unapplied,
    Hidden,
}

/// Specification for a multiple StGit revisions.
///
/// A StGit revision specification resolves to a commit that may or may not have an
/// associated patch name. Thus StGit revision specifications may be a patch locator, but
/// may also be a git revision specifier which resolves to a commit that may be outside
/// the stack.
///
/// Patch ranges are allowed, but git revision ranges *are not* allowed. Any
/// specification using the ".." range syntax will be resolved as a StGit patch range.
/// Multiple git revision specifications may be supplied, however, as separate arguments
/// when `RangeRevisionSpec` is used to parse command line arguments.
///
/// Similarly, when an optional "branch-name:" prefix is supplied, the remainder of the
/// specification after the ":" must be either a [`PatchRange`] or a single
/// [`PatchLocator`].
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum RangeRevisionSpec {
    BranchRange {
        branch_loc: BranchLocator,
        bounds: PatchRangeBounds,
    },
    Range(PatchRangeBounds),
    Single(SingleRevisionSpec),
}

/// Specification of a single StGit revision.
///
/// A StGit revision specification resolves to a single commit that could be either
/// inside or outside a stack. This differs from a [`PatchLocator`] which must resolve
/// to a patch inside a stack.
///
/// Any StGit offsets in the specification, i.e. `~[<n>]` or `+[<n>]`, *may* resolve to
/// commits below the stack. Furthermore, git revision suffixes may be supplied after
/// any patch locator offsets. For example, `{base}~^2` would refer to the second
/// parent of the parent of the current stack's base commit.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SingleRevisionSpec {
    Branch {
        branch_loc: BranchLocator,
        patch_like: PatchLikeSpec,
    },
    PatchAndGitLike(PatchLikeSpec, String),
    GitLike(String),
    PatchLike(PatchLikeSpec),
}

/// A resolved StGit revision consisting of an optional patch name and a commit.
#[derive(Debug)]
pub(crate) struct StGitRevision<'repo> {
    pub(crate) patchname: Option<PatchName>,
    pub(crate) commit: Rc<gix::Commit<'repo>>,
}

/// Resolved StGit boundary revisions.
#[derive(Debug)]
pub(crate) enum StGitBoundaryRevisions<'repo> {
    Single(StGitRevision<'repo>),
    Bounds((StGitRevision<'repo>, StGitRevision<'repo>)),
}

/// A regular [`PatchLocator`] with an optional git revision suffix.
///
/// These extended locators are used with [`SingleRevisionSpec`] (and
/// transitively [`RangeRevisionSpec`]).
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PatchLikeSpec {
    patch_loc: PatchLocator,
    suffix: GitRevisionSuffix,
}

/// A git revision suffix string.
///
/// StGit is able to recognize these strings, but their interpretation is handled
/// outside of StGit, in either [`gix`] or `git` itself.
#[derive(Clone, Debug, PartialEq)]
struct GitRevisionSuffix(pub(crate) String);
