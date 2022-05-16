//! Parse user-provided patch ranges.

use std::str::FromStr;

use crate::{patchname::PatchName, stack::StackStateAccess};

/// Patch range parsing error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error(transparent)]
    PatchName(#[from] crate::patchname::Error),

    #[error("Patch `{patchname}` is used more than once")]
    Duplicate { patchname: PatchName },

    #[error("Patch `{patchname}` in `{range}` is not allowed")]
    BoundaryNotAllowed { patchname: PatchName, range: String },

    #[error("Patch `{patchname}` in `{range}` does not exist")]
    BoundaryNotKnown { patchname: PatchName, range: String },

    #[error(
        "Patch `{patchname}` in `{range}` does not exist, \
             but is similar to {similar_patchnames}"
    )]
    BoundarySimilar {
        patchname: PatchName,
        range: String,
        similar_patchnames: String,
    },

    #[error("{whence} patch `{patchname}` is not allowed")]
    PatchNotAllowed {
        whence: &'static str,
        patchname: PatchName,
    },

    #[error("Patch `{patchname}` does not exist")]
    PatchNotKnown { patchname: PatchName },

    #[error(
        "Patch `{patchname}` does not exist, \
         but is similar to {similar_patchnames}"
    )]
    PatchSimilar {
        patchname: PatchName,
        similar_patchnames: String,
    },

    #[error("`{range}` not contiguous with preceding range `{prev_range}`")]
    NotContiguous { range: String, prev_range: String },

    #[error("End patch `{end_patchname}` is out of order with `{begin_patchname}`")]
    BoundaryOrder {
        begin_patchname: PatchName,
        end_patchname: PatchName,
    },
}

/// Indicates which patches are allowed in user-supplied patch ranges.
///
/// The `AllWithAppliedBoundary` and `VisibleWithAppliedBoundary` variants allow all and
/// visible (applied + unapplied), respectively, but constrain open-ended patch ranges
/// to the last applied patch when the beginning of the open range is an applied patch.
#[derive(Clone, Copy)]
pub(crate) enum Allow {
    All,
    AllWithAppliedBoundary,
    Visible,
    VisibleWithAppliedBoundary,
    Applied,
    Unapplied,
    Hidden,
}

impl Allow {
    fn use_applied_boundary(&self) -> bool {
        matches!(
            self,
            Allow::AllWithAppliedBoundary | Allow::VisibleWithAppliedBoundary
        )
    }

    fn get_allowed<'repo, 'a>(
        &self,
        stack_state: &'a impl StackStateAccess<'repo>,
    ) -> Vec<&'a PatchName> {
        match self {
            Allow::All | Allow::AllWithAppliedBoundary => stack_state.all_patches().collect(),
            Allow::Visible | Allow::VisibleWithAppliedBoundary => {
                stack_state.applied_and_unapplied().collect()
            }
            Allow::Applied => stack_state.applied().iter().collect(),
            Allow::Unapplied => stack_state.unapplied().iter().collect(),
            Allow::Hidden => stack_state.hidden().iter().collect(),
        }
    }
}

/// Parse user-provided patch range strings.
///
/// A patch range names one or more patches. A patch range may be one of the following
/// forms:
///
/// - A single (valid) patch name.
/// - An open ended patch range, e.g. `patch..`.
/// - An open beginning patch range, e.g. `..patch`
/// - Or a closed range, e.g. `patch0..patchN`
///
/// The subset of known patches allowed in user-provided patch ranges is indicated by
/// the `allow` parameter.
///
/// It is an error for the same patch to be duplicated in any of the `range_args`.
///
/// The ordering of patches as found in `range_args` does not have to match the ordering
/// found in the stack. See [`parse_contiguous()`] for a similar function which does
/// impose this ordering constraint.
pub(crate) fn parse<'repo, 'a>(
    range_args: impl IntoIterator<Item = &'a str>,
    stack_state: &impl StackStateAccess<'repo>,
    allow: Allow,
) -> Result<Vec<PatchName>, Error> {
    let allowed_patches: Vec<&PatchName> = allow.get_allowed(stack_state);
    let mut patches: Vec<PatchName> = Vec::new();

    for arg in range_args {
        if let Some((begin_name, end_name)) = arg.split_once("..") {
            let begin_pos = if begin_name.is_empty() {
                0
            } else {
                let patchname = PatchName::from_str(begin_name)?;
                allowed_patches
                    .iter()
                    .position(|&pn| pn == &patchname)
                    .ok_or_else(|| {
                        if stack_state.has_patch(&patchname) {
                            Error::BoundaryNotAllowed {
                                patchname,
                                range: arg.to_string(),
                            }
                        } else if let Some(similar_patchnames) =
                            similar_patchnames(&patchname, &allowed_patches)
                        {
                            Error::BoundarySimilar {
                                patchname,
                                range: arg.to_string(),
                                similar_patchnames,
                            }
                        } else {
                            Error::BoundaryNotKnown {
                                patchname,
                                range: arg.to_string(),
                            }
                        }
                    })?
            };

            let end_pos = if !end_name.is_empty() {
                let patchname = PatchName::from_str(end_name)?;
                allowed_patches
                    .iter()
                    .position(|&pn| pn == &patchname)
                    .ok_or_else(|| {
                        if stack_state.has_patch(&patchname) {
                            Error::BoundaryNotAllowed {
                                patchname,
                                range: arg.to_string(),
                            }
                        } else if let Some(similar_patchnames) =
                            similar_patchnames(&patchname, &allowed_patches)
                        {
                            Error::BoundarySimilar {
                                patchname,
                                range: arg.to_string(),
                                similar_patchnames,
                            }
                        } else {
                            Error::BoundaryNotKnown {
                                patchname,
                                range: arg.to_string(),
                            }
                        }
                    })?
            } else if allow.use_applied_boundary()
                && !stack_state.applied().is_empty()
                && begin_pos < stack_state.applied().len()
            {
                stack_state.applied().len() - 1
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
        } else {
            let patchname = PatchName::from_str(arg)?;
            if patches.contains(&patchname) {
                return Err(Error::Duplicate { patchname });
            } else if !allowed_patches.contains(&&patchname) {
                return Err(if stack_state.has_patch(&patchname) {
                    Error::PatchNotAllowed {
                        whence: from_whence(&patchname, stack_state),
                        patchname,
                    }
                } else if let Some(similar_patchnames) =
                    similar_patchnames(&patchname, &allowed_patches)
                {
                    Error::PatchSimilar {
                        patchname,
                        similar_patchnames,
                    }
                } else {
                    Error::PatchNotKnown { patchname }
                });
            }
            patches.push(patchname);
        }
    }

    Ok(patches)
}

pub(crate) fn parse_contiguous<'repo, 'a>(
    range_args: impl IntoIterator<Item = &'a str>,
    stack_state: &impl StackStateAccess<'repo>,
    allow: Allow,
) -> Result<Vec<PatchName>, Error> {
    let allowed_patches: Vec<&PatchName> = allow.get_allowed(stack_state);
    let mut patches: Vec<PatchName> = Vec::new();
    let mut next_pos: Option<usize> = None;
    let mut prev_range = "";

    for arg in range_args {
        if let Some((begin_name, end_name)) = arg.split_once("..") {
            let begin_pos = if begin_name.is_empty() {
                0
            } else {
                let patchname = PatchName::from_str(begin_name)?;
                allowed_patches
                    .iter()
                    .position(|&pn| pn == &patchname)
                    .ok_or_else(|| {
                        if stack_state.has_patch(&patchname) {
                            Error::BoundaryNotAllowed {
                                patchname,
                                range: arg.to_string(),
                            }
                        } else if let Some(similar_patchnames) =
                            similar_patchnames(&patchname, &allowed_patches)
                        {
                            Error::BoundarySimilar {
                                patchname,
                                range: arg.to_string(),
                                similar_patchnames,
                            }
                        } else {
                            Error::BoundaryNotKnown {
                                patchname,
                                range: arg.to_string(),
                            }
                        }
                    })
                    .and_then(|begin_pos| {
                        if next_pos.is_some() && Some(begin_pos) != next_pos {
                            Err(Error::NotContiguous {
                                range: arg.to_string(),
                                prev_range: prev_range.to_string(),
                            })
                        } else {
                            Ok(begin_pos)
                        }
                    })?
            };

            let end_pos = if !end_name.is_empty() {
                let patchname = PatchName::from_str(end_name)?;
                allowed_patches
                    .iter()
                    .position(|&pn| pn == &patchname)
                    .ok_or_else(|| {
                        if stack_state.has_patch(&patchname) {
                            Error::BoundaryNotAllowed {
                                patchname: patchname.clone(),
                                range: arg.to_string(),
                            }
                        } else if let Some(similar_patchnames) =
                            similar_patchnames(&patchname, &allowed_patches)
                        {
                            Error::BoundarySimilar {
                                patchname: patchname.clone(),
                                range: arg.to_string(),
                                similar_patchnames,
                            }
                        } else {
                            Error::BoundaryNotKnown {
                                patchname: patchname.clone(),
                                range: arg.to_string(),
                            }
                        }
                    })
                    .and_then(|end_pos| {
                        if end_pos < begin_pos {
                            Err(Error::BoundaryOrder {
                                begin_patchname: allowed_patches[begin_pos].clone(),
                                end_patchname: patchname.clone(),
                            })
                        } else {
                            Ok(end_pos)
                        }
                    })?
            } else if allow.use_applied_boundary()
                && !stack_state.applied().is_empty()
                && begin_pos < stack_state.applied().len()
            {
                stack_state.applied().len() - 1
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
                patches.push(patchname);
            }

            next_pos = Some(end_pos + 1);
        } else {
            let patchname = PatchName::from_str(arg)?;
            if patches.contains(&patchname) {
                return Err(Error::Duplicate { patchname });
            } else {
                let pos = allowed_patches
                    .iter()
                    .position(|&pn| pn == &patchname)
                    .ok_or_else(|| {
                        if stack_state.has_patch(&patchname) {
                            Error::PatchNotAllowed {
                                whence: from_whence(&patchname, stack_state),
                                patchname: patchname.clone(),
                            }
                        } else if let Some(similar_patchnames) =
                            similar_patchnames(&patchname, &allowed_patches)
                        {
                            Error::PatchSimilar {
                                patchname: patchname.clone(),
                                similar_patchnames,
                            }
                        } else {
                            Error::PatchNotKnown {
                                patchname: patchname.clone(),
                            }
                        }
                    })
                    .and_then(|pos| {
                        if next_pos.is_some() && Some(pos) != next_pos {
                            Err(Error::NotContiguous {
                                range: arg.to_string(),
                                prev_range: prev_range.to_string(),
                            })
                        } else {
                            Ok(pos)
                        }
                    })?;
                patches.push(patchname);
                next_pos = Some(pos + 1);
            }
        }

        prev_range = arg;
    }

    Ok(patches)
}

/// Parse a single (non-range) patch argument.
pub(crate) fn parse_single<'repo>(
    arg: &str,
    stack_state: &impl StackStateAccess<'repo>,
    allow: Allow,
) -> Result<PatchName, Error> {
    let allowed_patches: Vec<&PatchName> = allow.get_allowed(stack_state);
    let allowed_patches = allowed_patches.as_slice();
    let patchname = PatchName::from_str(arg)?;
    if allowed_patches.contains(&&patchname) {
        Ok(patchname)
    } else if stack_state.has_patch(&patchname) {
        Err(Error::PatchNotAllowed {
            whence: from_whence(&patchname, stack_state),
            patchname,
        })
    } else if let Some(similar_patchnames) = similar_patchnames(&patchname, allowed_patches) {
        Err(Error::PatchSimilar {
            patchname,
            similar_patchnames,
        })
    } else {
        Err(Error::PatchNotKnown { patchname })
    }
}

/// Find similar patch names from a list of allowed patch names.
fn similar_patchnames(patchname: &PatchName, allowed_patchnames: &[&PatchName]) -> Option<String> {
    let similar: Vec<&PatchName> = allowed_patchnames
        .iter()
        .filter(|pn| strsim::jaro_winkler(pn.as_ref(), patchname.as_ref()) > 0.75)
        .copied()
        .collect();

    if similar.is_empty() {
        None
    } else if similar.len() == 1 {
        Some(format!("`{}`", similar[0]))
    } else if similar.len() == 2 {
        Some(format!("`{}` and `{}`", similar[0], similar[1]))
    } else {
        let mut similar_str = String::new();
        for pn in similar[..similar.len() - 1].iter() {
            similar_str.push_str(&format!("`{pn}`, "));
        }
        similar_str.push_str(&format!("and `{}`", similar[similar.len() - 1]));
        Some(similar_str)
    }
}

/// Determine whether a patch is applied, unapplied, or hidden.
fn from_whence<'repo>(
    patchname: &PatchName,
    stack_state: &impl StackStateAccess<'repo>,
) -> &'static str {
    if stack_state.hidden().contains(patchname) {
        "Hidden"
    } else if stack_state.unapplied().contains(patchname) {
        "Unapplied"
    } else if stack_state.applied().contains(patchname) {
        "Applied"
    } else {
        panic!("patch {patchname} must be known")
    }
}
