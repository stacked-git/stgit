// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`PatchRange`] and [`PatchRangeBounds`].

use std::str::FromStr;

use super::{
    PatchName, PatchRange, PatchRangeBounds, RangeConstraint, StGitBoundaryRevisions, StGitRevision,
};
use crate::stack::{StackAccess, StackStateAccess};

/// Patch range parsing error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error(transparent)]
    Name(#[from] super::name::Error),

    #[error(transparent)]
    Locator(#[from] super::locator::Error),

    #[error("invalid patch range `{0}`")]
    InvalidPatchRange(String),

    #[error("patch `{patchname}` is used more than once")]
    Duplicate { patchname: PatchName },

    #[error("`{range}` not contiguous with preceding range `{prev_range}`")]
    NotContiguous { range: String, prev_range: String },

    #[error("end patch `{end_patchname}` is out of order with `{begin_patchname}`")]
    BoundaryOrder {
        begin_patchname: PatchName,
        end_patchname: PatchName,
    },
}

impl std::fmt::Display for PatchRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatchRange::Single(patch_loc) => patch_loc.fmt(f),
            PatchRange::Range(bounds) => bounds.fmt(f),
        }
    }
}

impl FromStr for PatchRange {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use winnow::Parser;
        super::parse::patch_range
            .parse(s)
            .map_err(|_| Error::InvalidPatchRange(s.to_string()))
    }
}

impl From<&PatchRangeBounds> for PatchRange {
    fn from(bounds: &PatchRangeBounds) -> Self {
        PatchRange::Range(bounds.clone())
    }
}

impl std::fmt::Display for PatchRangeBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.begin.as_ref(), self.end.as_ref()) {
            (None, None) => write!(f, ".."),
            (None, Some(end)) => write!(f, "..{end}"),
            (Some(begin), None) => write!(f, "{begin}.."),
            (Some(begin), Some(end)) => write!(f, "{begin}..{end}"),
        }
    }
}

impl PatchRangeBounds {
    /// Resolve patchname and commit object based on patch location and offsets.
    ///
    /// Unlike [`resolve_names()`], the offsets are allowed to land on commits outside
    /// of the stack.
    pub(crate) fn resolve_revisions<'repo>(
        &self,
        stack: &impl StackAccess<'repo>,
        use_applied_boundary: bool,
    ) -> Result<StGitBoundaryRevisions<'repo>, Error> {
        let Self { begin, end } = self;
        let rev0 = if let Some(patch_loc) = begin {
            patch_loc.resolve_revision(stack)?
        } else if let Some(first_patchname) = stack.applied().first() {
            StGitRevision {
                patchname: Some(first_patchname.clone()),
                commit: stack.get_patch_commit(first_patchname).clone(),
            }
        } else {
            StGitRevision {
                patchname: None,
                commit: stack.base().clone(),
            }
        };
        let rev1 = if let Some(patch_loc) = end {
            patch_loc.resolve_revision(stack)?
        } else if use_applied_boundary {
            if let Some(top_patchname) = stack.applied().last() {
                StGitRevision {
                    patchname: Some(top_patchname.clone()),
                    commit: stack.get_patch_commit(top_patchname).clone(),
                }
            } else {
                StGitRevision {
                    patchname: None,
                    commit: stack.base().clone(),
                }
            }
        } else if let Some(last_patchname) = stack.applied_and_unapplied().last() {
            StGitRevision {
                patchname: Some(last_patchname.clone()),
                commit: stack.get_patch_commit(last_patchname).clone(),
            }
        } else {
            StGitRevision {
                patchname: None,
                commit: stack.base().clone(),
            }
        };

        Ok(StGitBoundaryRevisions::Bounds((rev0, rev1)))
    }
}

/// Resolve user-provided patch ranges into patch names.
///
/// The subset of known patches allowed in the patch ranges is indicated by the `allow`
/// parameter.
///
/// It is an error for the same patch to be duplicated in any of the provided ranges.
///
/// The ordering of patches as found in `ranges` does not have to match the ordering
/// found in the stack. See [`resolve_names_contiguous()`] for a similar function which
/// does impose this ordering constraint.
pub(crate) fn resolve_names<'a, 'repo>(
    stack: &'a impl StackStateAccess<'repo>,
    ranges: impl IntoIterator<Item = &'a PatchRange>,
    allow: RangeConstraint,
) -> Result<Vec<PatchName>, Error> {
    let allowed_patches: Vec<&PatchName> = stack.get_allowed(allow.into());
    let mut patches: Vec<PatchName> = Vec::new();

    for range in ranges {
        match range {
            PatchRange::Range(PatchRangeBounds { begin, end }) => {
                let begin = begin
                    .as_ref()
                    .map(|loc| loc.resolve_name(stack))
                    .transpose()?
                    .map_or_else(
                        || Ok(None),
                        |pn| Some(pn.constrain(stack, allow.into())).transpose(),
                    )?;

                let end = end
                    .as_ref()
                    .map(|loc| loc.resolve_name(stack))
                    .transpose()?
                    .map_or_else(
                        || Ok(None),
                        |pn| Some(pn.constrain(stack, allow.into())).transpose(),
                    )?;

                let begin_pos = begin.map_or(0, |patchname| {
                    allowed_patches
                        .iter()
                        .position(|&pn| pn == &patchname)
                        .expect("begin patchname already constrained to the allowed patches")
                });

                let end_pos = if let Some(patchname) = end {
                    allowed_patches
                        .iter()
                        .position(|&pn| pn == &patchname)
                        .expect("end patchname already constrained to allowed patches")
                } else if allow.use_applied_boundary()
                    && !stack.applied().is_empty()
                    && begin_pos < stack.applied().len()
                {
                    stack.applied().len() - 1
                } else if !allowed_patches.is_empty() {
                    allowed_patches.len() - 1
                } else {
                    continue;
                };

                let selected_patches = if begin_pos <= end_pos {
                    allowed_patches[begin_pos..=end_pos].to_vec()
                } else {
                    allowed_patches[end_pos..=begin_pos]
                        .iter()
                        .rev()
                        .copied()
                        .collect()
                };

                for pn in selected_patches {
                    let patchname = pn.clone();
                    if patches.contains(pn) {
                        return Err(Error::Duplicate { patchname });
                    }
                    patches.push(patchname);
                }
            }

            PatchRange::Single(patch_loc) => {
                let patchname = patch_loc
                    .resolve_name(stack)?
                    .constrain(stack, allow.into())?;
                if patches.contains(&patchname) {
                    return Err(Error::Duplicate { patchname });
                }
                patches.push(patchname);
            }
        }
    }

    Ok(patches)
}

/// Resolve user-provided patch ranges into contiguous patch names.
///
/// It is an error if any of the ranges provided in `ranges` are discontiguous.
pub(crate) fn resolve_names_contiguous<'a>(
    stack: &'a impl StackStateAccess<'a>,
    ranges: impl IntoIterator<Item = &'a PatchRange>,
    allow: RangeConstraint,
) -> Result<Vec<PatchName>, Error> {
    let allowed_patches: Vec<&PatchName> = stack.get_allowed(allow.into());
    let mut patches: Vec<PatchName> = Vec::new();
    let mut next_pos: Option<usize> = None;
    let mut prev_range: Option<&PatchRange> = None;

    for range in ranges {
        match range {
            PatchRange::Range(PatchRangeBounds { begin, end }) => {
                let begin = begin
                    .as_ref()
                    .map(|loc| loc.resolve_name(stack))
                    .transpose()?
                    .map_or_else(
                        || Ok(None),
                        |pn| Some(pn.constrain(stack, allow.into())).transpose(),
                    )?;

                let end = end
                    .as_ref()
                    .map(|loc| loc.resolve_name(stack))
                    .transpose()?
                    .map_or_else(
                        || Ok(None),
                        |pn| Some(pn.constrain(stack, allow.into())).transpose(),
                    )?;

                let begin_pos = begin.map_or(0, |patchname| {
                    allowed_patches
                        .iter()
                        .position(|&pn| pn == &patchname)
                        .expect("begin patchname already constrained to the allowed patches")
                });
                if next_pos.is_some() && Some(begin_pos) != next_pos {
                    return Err(Error::NotContiguous {
                        range: range.to_string(),
                        prev_range: prev_range.unwrap().to_string(),
                    });
                }

                let end_pos = if let Some(patchname) = end {
                    let end_pos = allowed_patches
                        .iter()
                        .position(|&pn| pn == &patchname)
                        .expect("end patchname already constrained to allowed patches");
                    if end_pos < begin_pos {
                        return Err(Error::BoundaryOrder {
                            begin_patchname: allowed_patches[begin_pos].clone(),
                            end_patchname: patchname,
                        });
                    }
                    end_pos
                } else if allow.use_applied_boundary()
                    && !stack.applied().is_empty()
                    && begin_pos < stack.applied().len()
                {
                    stack.applied().len() - 1
                } else if !allowed_patches.is_empty() {
                    allowed_patches.len() - 1
                } else {
                    continue;
                };

                let selected_patches = allowed_patches[begin_pos..=end_pos].to_vec();

                for pn in selected_patches {
                    let patchname = pn.clone();
                    if patches.contains(pn) {
                        return Err(Error::Duplicate { patchname });
                    }
                    patches.push(patchname.clone());
                }

                next_pos = Some(end_pos + 1);
            }
            PatchRange::Single(patch_loc) => {
                let patchname = patch_loc
                    .resolve_name(stack)?
                    .constrain(stack, allow.into())?;
                if patches.contains(&patchname) {
                    return Err(Error::Duplicate { patchname });
                } else {
                    let pos = allowed_patches
                        .iter()
                        .position(|&pn| pn == &patchname)
                        .expect("patchname already constrained to allowed patches");
                    if next_pos.is_some() && Some(pos) != next_pos {
                        return Err(Error::NotContiguous {
                            range: range.to_string(),
                            prev_range: prev_range.unwrap().to_string(),
                        });
                    }
                    patches.push(patchname);
                    next_pos = Some(pos + 1);
                }
            }
        }

        prev_range = Some(range);
    }

    Ok(patches)
}
