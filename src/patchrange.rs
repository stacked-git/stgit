//! Parse user-provided patch ranges.

use std::str::FromStr;

use crate::patchname::PatchName;

/// Patch range parsing error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error(transparent)]
    PatchName(#[from] crate::patchname::Error),

    #[error("patch `{patchname}` is used more than once")]
    Duplicate { patchname: PatchName },

    #[error("patch `{patchname}` in `{range}` is not allowed")]
    BoundaryNotAllowed { patchname: PatchName, range: String },

    #[error("patch `{patchname}` in `{range}` does not exist")]
    BoundaryNotKnown { patchname: PatchName, range: String },

    #[error("patch `{patchname}` is not allowed")]
    PatchNotAllowed { patchname: PatchName },

    #[error("patch `{patchname}` does not exist")]
    PatchNotKnown { patchname: PatchName },

    #[error("`{range}` not contiguous with preceding range `{prev_range}`")]
    NotContiguous { range: String, prev_range: String },

    #[error("end patch `{end_patchname}` is out of order with `{begin_patchname}`")]
    BoundaryOrder {
        begin_patchname: PatchName,
        end_patchname: PatchName,
    },
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
/// Each `str` from `range_args` is validated and checked against the provided
/// `allowed_patches` and `known_patches` iterators.
///
/// It is an error for the same patch to be duplicated in any of the `range_args`.
///
/// The ordering of patches as found in `range_args` does not have to match the ordering
/// found in `allowed_patches`. See [`parse_contiguous_patch_range()`] for a similar
/// function which does impose this ordering constraint.
pub(crate) fn parse_patch_ranges<'a>(
    range_args: impl IntoIterator<Item = &'a str>,
    allowed_patches: impl IntoIterator<Item = &'a PatchName>,
    known_patches: impl IntoIterator<Item = &'a PatchName>,
) -> Result<Vec<PatchName>, Error> {
    let allowed_patches: Vec<&PatchName> = allowed_patches.into_iter().collect();
    let known_patches: Vec<&PatchName> = known_patches.into_iter().collect();
    let mut patch_range: Vec<PatchName> = Vec::new();

    for arg in range_args {
        if let Some((begin_name, end_name)) = arg.split_once("..") {
            if begin_name.is_empty() && end_name.is_empty() {
                if patch_range.is_empty() {
                    patch_range.extend(allowed_patches.iter().map(|pn| (*pn).clone()));
                } else {
                    return Err(Error::Duplicate {
                        patchname: patch_range[0].clone(),
                    });
                }
            } else {
                let valid_range: Vec<&PatchName> = if begin_name.is_empty() {
                    let end_patchname = PatchName::from_str(end_name)?;
                    if let Some(end_pos) =
                        allowed_patches.iter().position(|&pn| pn == &end_patchname)
                    {
                        allowed_patches[..=end_pos].to_vec()
                    } else if !known_patches.contains(&&end_patchname) {
                        return Err(Error::BoundaryNotKnown {
                            patchname: end_patchname,
                            range: arg.to_string(),
                        });
                    } else {
                        return Err(Error::BoundaryNotAllowed {
                            patchname: end_patchname,
                            range: arg.to_string(),
                        });
                    }
                } else if end_name.is_empty() {
                    let begin_patchname = PatchName::from_str(begin_name)?;
                    if let Some(begin_pos) = allowed_patches
                        .iter()
                        .position(|&pn| pn == &begin_patchname)
                    {
                        allowed_patches[begin_pos..].to_vec()
                    } else if !known_patches.contains(&&begin_patchname) {
                        return Err(Error::BoundaryNotKnown {
                            patchname: begin_patchname,
                            range: arg.to_string(),
                        });
                    } else {
                        return Err(Error::BoundaryNotAllowed {
                            patchname: begin_patchname,
                            range: arg.to_string(),
                        });
                    }
                } else {
                    let begin_patchname = PatchName::from_str(begin_name)?;
                    let end_patchname = PatchName::from_str(end_name)?;
                    let begin_pos = allowed_patches
                        .iter()
                        .position(|&pn| pn == &begin_patchname)
                        .ok_or_else(|| {
                            if !known_patches.contains(&&begin_patchname) {
                                Error::BoundaryNotKnown {
                                    patchname: begin_patchname.clone(),
                                    range: arg.to_string(),
                                }
                            } else {
                                Error::BoundaryNotAllowed {
                                    patchname: begin_patchname.clone(),
                                    range: arg.to_string(),
                                }
                            }
                        })?;

                    let end_pos = allowed_patches
                        .iter()
                        .position(|&pn| pn == &end_patchname)
                        .ok_or_else(|| {
                            if !known_patches.contains(&&end_patchname) {
                                Error::BoundaryNotKnown {
                                    patchname: begin_patchname.clone(),
                                    range: arg.to_string(),
                                }
                            } else {
                                Error::BoundaryNotAllowed {
                                    patchname: begin_patchname.clone(),
                                    range: arg.to_string(),
                                }
                            }
                        })?;

                    if begin_pos <= end_pos {
                        allowed_patches[begin_pos..=end_pos].to_vec()
                    } else {
                        allowed_patches[end_pos..=begin_pos]
                            .iter()
                            .rev()
                            .copied()
                            .collect()
                    }
                };

                for pn in valid_range {
                    let patchname = pn.clone();
                    if !patch_range.contains(pn) {
                        patch_range.push(patchname);
                    } else {
                        return Err(Error::Duplicate { patchname });
                    }
                }
            }
        } else {
            let patchname = PatchName::from_str(arg)?;
            if allowed_patches.contains(&&patchname) {
                if !patch_range.contains(&patchname) {
                    patch_range.push(patchname);
                } else {
                    return Err(Error::Duplicate { patchname });
                }
            } else if !known_patches.contains(&&patchname) {
                return Err(Error::PatchNotKnown { patchname });
            } else {
                return Err(Error::PatchNotAllowed { patchname });
            }
        }
    }

    Ok(patch_range)
}

/// Parse user-provided patch range strings.
///
/// This is like [`parse_patch_ranges`], but the patches as determined from `range_args`
/// must be a contiguous, in-order subset of `allowed_patches`.
pub(crate) fn parse_contiguous_patch_range<'a>(
    range_args: impl IntoIterator<Item = &'a str>,
    allowed_patches: impl IntoIterator<Item = &'a PatchName>,
    known_patches: impl IntoIterator<Item = &'a PatchName>,
) -> Result<Vec<PatchName>, Error> {
    let mut allowed_patches: Vec<&PatchName> = allowed_patches.into_iter().collect();
    let known_patches: Vec<&PatchName> = known_patches.into_iter().collect();
    let mut patch_range: Vec<PatchName> = Vec::new();
    let mut prev_range = "";

    for arg in range_args {
        if let Some((begin_name, end_name)) = arg.split_once("..") {
            if begin_name.is_empty() && end_name.is_empty() {
                patch_range.extend(allowed_patches.drain(..).cloned())
            } else if begin_name.is_empty() {
                let end_patchname = PatchName::from_str(end_name)?;
                if let Some(end_pos) = allowed_patches.iter().position(|&pn| pn == &end_patchname) {
                    patch_range.extend(allowed_patches.drain(..=end_pos).cloned());
                } else if !known_patches.contains(&&end_patchname) {
                    return Err(Error::BoundaryNotKnown {
                        patchname: end_patchname,
                        range: arg.to_string(),
                    });
                } else {
                    return Err(Error::BoundaryNotAllowed {
                        patchname: end_patchname,
                        range: arg.to_string(),
                    });
                }
            } else if end_name.is_empty() {
                let begin_patchname = PatchName::from_str(begin_name)?;
                if let Some(begin_pos) = allowed_patches
                    .iter()
                    .position(|&pn| pn == &begin_patchname)
                {
                    if !patch_range.is_empty() && begin_pos > 0 {
                        return Err(Error::NotContiguous {
                            range: arg.to_string(),
                            prev_range: prev_range.to_string(),
                        });
                    }
                    allowed_patches.drain(..begin_pos);
                    patch_range.extend(allowed_patches.drain(..).cloned());
                } else if !known_patches.contains(&&begin_patchname) {
                    return Err(Error::BoundaryNotKnown {
                        patchname: begin_patchname,
                        range: arg.to_string(),
                    });
                } else {
                    return Err(Error::BoundaryNotAllowed {
                        patchname: begin_patchname,
                        range: arg.to_string(),
                    });
                }
            } else {
                let begin_patchname = PatchName::from_str(begin_name)?;
                let end_patchname = PatchName::from_str(end_name)?;
                let begin_pos = allowed_patches
                    .iter()
                    .position(|&pn| pn == &begin_patchname)
                    .ok_or_else(|| {
                        if !known_patches.contains(&&begin_patchname) {
                            Error::BoundaryNotKnown {
                                patchname: begin_patchname.clone(),
                                range: arg.to_string(),
                            }
                        } else {
                            Error::BoundaryNotAllowed {
                                patchname: begin_patchname.clone(),
                                range: arg.to_string(),
                            }
                        }
                    })?;

                if !patch_range.is_empty() && begin_pos > 0 {
                    return Err(Error::NotContiguous {
                        range: arg.to_string(),
                        prev_range: prev_range.to_string(),
                    });
                }

                let end_pos = allowed_patches
                    .iter()
                    .position(|&pn| pn == &end_patchname)
                    .ok_or_else(|| {
                        if !known_patches.contains(&&end_patchname) {
                            Error::BoundaryNotKnown {
                                patchname: end_patchname.clone(),
                                range: arg.to_string(),
                            }
                        } else {
                            Error::BoundaryNotAllowed {
                                patchname: end_patchname.clone(),
                                range: arg.to_string(),
                            }
                        }
                    })?;
                if begin_pos <= end_pos {
                    patch_range.extend(allowed_patches.drain(begin_pos..=end_pos).cloned());
                    allowed_patches.drain(..begin_pos);
                } else {
                    return Err(Error::BoundaryOrder {
                        begin_patchname,
                        end_patchname,
                    });
                }
            }
        } else {
            let patchname = PatchName::from_str(arg)?;
            let pos = allowed_patches
                .iter()
                .position(|&pn| pn == &patchname)
                .ok_or_else(|| {
                    if !known_patches.contains(&&patchname) {
                        Error::PatchNotKnown {
                            patchname: patchname.clone(),
                        }
                    } else {
                        Error::PatchNotAllowed {
                            patchname: patchname.clone(),
                        }
                    }
                })?;

            if !patch_range.is_empty() && pos > 0 {
                return Err(Error::NotContiguous {
                    range: arg.to_string(),
                    prev_range: prev_range.to_string(),
                });
            }

            allowed_patches.drain(..=pos);
            patch_range.push(patchname);
        }

        prev_range = arg;
    }

    Ok(patch_range)
}
