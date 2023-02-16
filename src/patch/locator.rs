// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for [`PatchLocator`].

use std::{cmp::Ordering, fmt::Write, rc::Rc, str::FromStr};

use crate::stack::{StackAccess, StackStateAccess};

use super::{PatchId, PatchLocator, PatchName, PatchOffsetAtom, PatchOffsets, StGitRevision};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("invalid patch locator `{0}`")]
    InvalidPatchLocator(String),

    #[error(
        "patch `{patchname}` does not exist, \
         but is similar to {similar_patchnames}"
    )]
    PatchSimilar {
        patchname: PatchName,
        similar_patchnames: String,
    },

    #[error("patch `{patchname}` does not exist")]
    PatchNotKnown { patchname: PatchName },

    #[error("invalid patch index `{0}`")]
    InvalidPatchIndex(usize),

    #[error("`{offsets}` from {id} goes outside the stack")]
    InvalidPatchOffset { id: String, offsets: PatchOffsets },

    #[error("no patch at {0}")]
    InvalidOffsetFrom(String),

    #[error("an offset is required when locating a patch relative to the stack base")]
    BaseNeedsOffset,

    #[error("a positive offset is required when locating a patch relative to the stack base")]
    BaseNeedsPositiveOffset,

    #[error("cannot locate last patch because stack is empty")]
    NoLastPatch,

    #[error("ambiguous commit id `{oid_prefix}` matches patches {patchnames}")]
    AmbiguousCommitId {
        oid_prefix: gix::hash::Prefix,
        patchnames: String,
    },

    #[error("finding ancestor: {0}")]
    Ancestors(String),
}

impl FromStr for PatchLocator {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(super::parse::patch_locator))(s)
            .map(|(_, location)| location)
            .map_err(|_| Error::InvalidPatchLocator(s.to_string()))
    }
}

impl std::fmt::Display for PatchLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.id, self.offsets)
    }
}

/// A disambiguated [`PatchLocator`].
///
/// A user-provided [`PatchLocator`] instance may be ambiguous due to valid patch names
/// also being valid stack indices, offsets, and/or commit ids. This struct is used as
/// an intermediate form during patch locator resolution.
struct DisambiguatedLocator<'a> {
    id: DisambiguatedId<'a>,
    offsets: PatchOffsets,
}

/// A disambiguated [`PatchId`].
///
/// Valid patch names that also *could be* commit ids, stack indices, or offsets are
/// disambiguated in the context of a stack. When a provided id matches one of the
/// stack's patch names, that name wins the disambiguation. Only when an ambiguous name
/// does not match a patch name in the stack will it be interpreted as a commit id,
/// index, or offset.
///
/// This struct is used as an intermediate form when resolving patch locators.
#[derive(Debug)]
enum DisambiguatedId<'a> {
    Name(&'a PatchName),
    CommitId(gix::hash::Prefix),
    Top,
    Base,
    Index(usize),
    FromTop(isize),
    FromBase(isize),
    FromLast(isize),
}

impl<'a> DisambiguatedId<'a> {
    fn string_for_error(&self) -> String {
        match self {
            DisambiguatedId::Name(name) => format!("`{name}`"),
            DisambiguatedId::CommitId(oid_prefix) => format!("commit id `{oid_prefix}`"),
            DisambiguatedId::Top => "topmost patch".to_string(),
            DisambiguatedId::Base => "stack base".to_string(),
            DisambiguatedId::Index(index) => format!("stack index `{index}`"),
            DisambiguatedId::FromTop(offset) => format!("`{offset:+}` from topmost patch"),
            DisambiguatedId::FromBase(offset) => format!("`{offset:+}` from stack base"),
            DisambiguatedId::FromLast(offset) => {
                if *offset == 0 {
                    "last patch".to_string()
                } else {
                    format!("{offset:+} from last patch")
                }
            }
        }
    }
}

impl PatchLocator {
    /// Resolve a patch name in the provided stack from this locator.
    ///
    /// The locator's offsets must stay within the given stack.
    pub(crate) fn resolve_name<'a, 'repo>(
        &'a self,
        stack: &'a impl StackStateAccess<'repo>,
    ) -> Result<PatchName, Error> {
        let DisambiguatedLocator { id, offsets } = self.disambiguate(stack);

        let patches: Vec<_> = stack.all_patches().collect();

        let mut index: isize = match id {
            DisambiguatedId::Name(pn) => {
                if stack.has_patch(pn) {
                    Ok(stack.index_of(pn) as isize)
                } else if let Some(similar_patchnames) = similar_patchnames(pn, stack) {
                    Err(Error::PatchSimilar {
                        patchname: pn.clone(),
                        similar_patchnames,
                    })
                } else {
                    Err(Error::PatchNotKnown {
                        patchname: pn.clone(),
                    })
                }
            }
            DisambiguatedId::CommitId(oid_prefix) => {
                let matching_names: Vec<_> = patches
                    .iter()
                    .filter_map(|&pn| {
                        (oid_prefix.cmp_oid(&stack.get_patch_commit_id(pn)) == Ordering::Equal)
                            .then_some(pn)
                    })
                    .collect();
                match matching_names.len() {
                    0 => panic!("disambiguation should prevent this"),
                    1 => Ok(stack.index_of(matching_names[0]) as isize),
                    _ => Err(Error::AmbiguousCommitId {
                        oid_prefix,
                        patchnames: patchnames_string(&matching_names).unwrap(),
                    }),
                }
            }
            DisambiguatedId::Top => Ok((stack.applied().len() as isize) - 1),
            DisambiguatedId::Base => {
                if offsets.is_empty() {
                    Err(Error::BaseNeedsOffset)
                } else {
                    Ok(-1)
                }
            }
            DisambiguatedId::Index(index) => {
                if index < patches.len() {
                    Ok(index as isize)
                } else {
                    Err(Error::InvalidPatchIndex(index))
                }
            }
            DisambiguatedId::FromTop(offset) => {
                let index = (stack.applied().len() as isize) - 1 + offset;
                if index >= 0 && (index as usize) < patches.len() {
                    Ok(index)
                } else {
                    Err(Error::InvalidOffsetFrom(id.string_for_error()))
                }
            }
            DisambiguatedId::FromBase(offset) => {
                if offset < 1 {
                    Err(Error::BaseNeedsPositiveOffset)
                } else {
                    let index = -1 + offset;
                    if (index as usize) < patches.len() {
                        Ok(index)
                    } else {
                        Err(Error::InvalidOffsetFrom(id.string_for_error()))
                    }
                }
            }
            DisambiguatedId::FromLast(offset) => {
                if patches.is_empty() {
                    Err(Error::NoLastPatch)
                } else if let Some(index) = (stack.applied_and_unapplied().count() - 1)
                    .checked_add_signed(offset)
                    .filter(|&index| index < patches.len())
                {
                    Ok(index as isize)
                } else {
                    Err(Error::InvalidOffsetFrom(id.string_for_error()))
                }
            }
        }?;

        for atom in offsets.atoms() {
            let new_index = match atom {
                PatchOffsetAtom::Plus(n) => index.checked_add_unsigned(n.unwrap_or(1)),
                PatchOffsetAtom::Tilde(n) => index.checked_sub_unsigned(n.unwrap_or(1)),
            };

            if new_index.is_none()
                || new_index < Some(0)
                || new_index >= Some(patches.len() as isize)
            {
                return Err(Error::InvalidPatchOffset {
                    id: id.string_for_error(),
                    offsets,
                });
            }

            index = new_index.unwrap();
        }

        Ok(patches[index as usize].clone())
    }

    /// Resolve patchname and commit object based on patch location and offsets.
    ///
    /// Unlike [`PatchLocator::resolve_name()`], the offsets are allowed to land on a
    /// commit outside of the stack.
    pub(crate) fn resolve_revision<'a, 'repo>(
        &'a self,
        stack: &'a impl StackAccess<'repo>,
    ) -> Result<StGitRevision<'repo>, Error> {
        let DisambiguatedLocator { id, offsets } = self.disambiguate(stack);

        let patches: Vec<_> = stack.all_patches().collect();

        let mut index: isize = match id {
            DisambiguatedId::Name(pn) => {
                if stack.has_patch(pn) {
                    Ok(stack.index_of(pn) as isize)
                } else if let Some(similar_patchnames) = similar_patchnames(pn, stack) {
                    Err(Error::PatchSimilar {
                        patchname: pn.clone(),
                        similar_patchnames,
                    })
                } else {
                    Err(Error::PatchNotKnown {
                        patchname: pn.clone(),
                    })
                }
            }
            DisambiguatedId::CommitId(oid_prefix) => {
                let matching_names: Vec<_> = patches
                    .iter()
                    .filter_map(|&pn| {
                        (oid_prefix.cmp_oid(&stack.get_patch_commit_id(pn)) == Ordering::Equal)
                            .then_some(pn)
                    })
                    .collect();
                match matching_names.len() {
                    0 => panic!("disambiguation should prevent this"),
                    1 => Ok(stack.index_of(matching_names[0]) as isize),
                    _ => Err(Error::AmbiguousCommitId {
                        oid_prefix,
                        patchnames: patchnames_string(&matching_names).unwrap(),
                    }),
                }
            }
            DisambiguatedId::Top => Ok((stack.applied().len() as isize) - 1),
            DisambiguatedId::Base => Ok(-1),
            DisambiguatedId::Index(index) => {
                if index < patches.len() {
                    Ok(index as isize)
                } else {
                    Err(Error::InvalidPatchIndex(index))
                }
            }
            DisambiguatedId::FromTop(offset) => {
                let index = (stack.applied().len() as isize) - 1 + offset;
                if index >= 0 && (index as usize) < patches.len() {
                    Ok(index)
                } else {
                    Err(Error::InvalidOffsetFrom(id.string_for_error()))
                }
            }
            DisambiguatedId::FromBase(offset) => {
                let index = -1 + offset;
                if index < 0 || (index as usize) < patches.len() {
                    Ok(index)
                } else {
                    Err(Error::InvalidOffsetFrom(id.string_for_error()))
                }
            }
            DisambiguatedId::FromLast(offset) => {
                if patches.is_empty() {
                    Err(Error::NoLastPatch)
                } else if let Some(index) = (stack.applied_and_unapplied().count() - 1)
                    .checked_add_signed(offset)
                    .filter(|&index| index < patches.len())
                {
                    Ok(index as isize)
                } else {
                    Err(Error::InvalidOffsetFrom(id.string_for_error()))
                }
            }
        }?;

        for atom in offsets.atoms() {
            let new_index = match atom {
                PatchOffsetAtom::Plus(n) => index.checked_add_unsigned(n.unwrap_or(1)),
                PatchOffsetAtom::Tilde(n) => index.checked_sub_unsigned(n.unwrap_or(1)),
            };

            if new_index.is_none() || new_index >= Some(patches.len() as isize) {
                return Err(Error::InvalidPatchOffset {
                    id: id.string_for_error(),
                    offsets,
                });
            }

            index = new_index.unwrap();
        }

        if index >= 0 {
            let patchname = patches[index as usize].clone();
            let commit = stack.get_patch_commit(&patchname).clone();
            Ok(StGitRevision {
                patchname: Some(patchname),
                commit,
            })
        } else if index == -1 {
            Ok(StGitRevision {
                patchname: None,
                commit: stack.base().clone(),
            })
        } else {
            let mut walk = stack
                .base()
                .ancestors()
                .first_parent_only()
                .all()
                .map_err(|e| Error::Ancestors(e.to_string()))?
                .error_on_missing_commit();

            let n = index.unsigned_abs().checked_sub(1).unwrap();

            if let Some(Ok(id)) = walk.nth(n) {
                let object = id.object().map_err(|_| {
                    Error::Ancestors(format!("id `{id}` does not point to an object"))
                })?;
                let commit = object.try_into_commit().map_err(|_| {
                    Error::Ancestors(format!("id `{id}` does not point to a commit"))
                })?;
                Ok(StGitRevision {
                    patchname: None,
                    commit: Rc::new(commit),
                })
            } else {
                Err(Error::Ancestors(
                    "repository does not have enough commit depth".to_string(),
                ))
            }
        }
    }

    fn disambiguate<'a, 'repo>(
        &'a self,
        stack: &'a impl StackStateAccess<'repo>,
    ) -> DisambiguatedLocator<'a> {
        match &self.id {
            PatchId::Base => DisambiguatedLocator {
                id: DisambiguatedId::Base,
                offsets: self.offsets.clone(),
            },
            PatchId::Top => {
                if stack.applied().is_empty() {
                    DisambiguatedLocator {
                        id: DisambiguatedId::Base,
                        offsets: self.offsets.clone(),
                    }
                } else {
                    DisambiguatedLocator {
                        id: DisambiguatedId::Top,
                        offsets: self.offsets.clone(),
                    }
                }
            }
            PatchId::BelowTop(n) => DisambiguatedLocator {
                id: DisambiguatedId::FromTop(n.map_or(-1, |n| -(n as isize))),
                offsets: self.offsets.clone(),
            },
            PatchId::BelowLast(n) => DisambiguatedLocator {
                id: DisambiguatedId::FromLast(n.map_or(0, |n| -n)),
                offsets: self.offsets.clone(),
            },
            PatchId::Name(patchname) if stack.has_patch(patchname) => DisambiguatedLocator {
                id: DisambiguatedId::Name(patchname),
                offsets: self.offsets.clone(),
            },
            PatchId::Name(patchname) => {
                use nom::combinator::{all_consuming, complete};
                let patch_offsets = |s| {
                    complete(all_consuming(super::parse::patch_offsets))(s)
                        .map(|(_, output)| output)
                        .ok()
                };
                let oid_prefix_offsets = |s| {
                    complete(all_consuming(super::parse::oid_prefix_offsets))(s)
                        .map(|(_, output)| output)
                        .ok()
                };
                let sign_number_offsets = |s| {
                    complete(all_consuming(super::parse::sign_number_offsets))(s)
                        .map(|(_, output)| output)
                        .ok()
                };
                let patch_locator_top = |s| {
                    complete(all_consuming(super::parse::patch_locator_top))(s)
                        .map(|(_, output)| output)
                        .ok()
                };
                let patch_locator_base = |s| {
                    complete(all_consuming(super::parse::patch_locator_base))(s)
                        .map(|(_, output)| output)
                        .ok()
                };

                let name: &str = patchname.as_ref();

                // A patch from the stack is a prefix match for this patchname and the
                // remaining suffix can be interpreted as "plussy" offsets, e.g.
                // "++3+".
                if let Some((prefix_pn, offsets)) = stack
                    .all_patches()
                    .filter_map(|pn| {
                        name.strip_prefix(pn.0.as_str())
                            .and_then(patch_offsets)
                            .map(|suffix| (pn, suffix))
                    })
                    .max_by_key(|(pn, _suffix)| pn.len())
                {
                    DisambiguatedLocator {
                        id: DisambiguatedId::Name(prefix_pn),
                        offsets: offsets.join(&self.offsets),
                    }
                } else if let Some(PatchLocator { id, offsets }) = patch_locator_top(name) {
                    let id = match id {
                        PatchId::Top => DisambiguatedId::Top,
                        PatchId::BelowTop(n) => {
                            DisambiguatedId::FromTop(n.map_or(-1, |n| -(n as isize)))
                        }
                        _ => panic!(),
                    };
                    let offsets = offsets.join(&self.offsets);
                    DisambiguatedLocator { id, offsets }
                } else if let Some(PatchLocator {
                    id: PatchId::Base,
                    offsets,
                }) = patch_locator_base(name)
                {
                    DisambiguatedLocator {
                        id: DisambiguatedId::Base,
                        offsets: offsets.join(&self.offsets),
                    }
                } else if let Some((oid_prefix, offsets)) =
                    oid_prefix_offsets(name).filter(|(oid_prefix, _)| {
                        stack.all_patches().any(|pn| {
                            oid_prefix.cmp_oid(&stack.get_patch_commit_id(pn)) == Ordering::Equal
                        })
                    })
                {
                    DisambiguatedLocator {
                        id: DisambiguatedId::CommitId(oid_prefix),
                        offsets: offsets.join(&self.offsets),
                    }
                } else if let Some((sign, maybe_n, offsets)) = sign_number_offsets(name) {
                    use super::parse::Sign;
                    let id = match sign {
                        Some(sign) => {
                            let n: isize = maybe_n
                                .unwrap_or(1)
                                .try_into()
                                .expect("parser ensures n fits in isize");
                            let n = match sign {
                                Sign::Plus => n,
                                Sign::Minus => n.checked_neg().unwrap(),
                            };
                            if stack.applied().is_empty() {
                                DisambiguatedId::FromBase(n)
                            } else {
                                DisambiguatedId::FromTop(n)
                            }
                        }
                        None => {
                            let n = maybe_n.expect("no sign implies there must have been a number");
                            DisambiguatedId::Index(n)
                        }
                    };
                    let offsets = offsets.join(&self.offsets);
                    DisambiguatedLocator { id, offsets }
                } else {
                    DisambiguatedLocator {
                        id: DisambiguatedId::Name(patchname),
                        offsets: self.offsets.clone(),
                    }
                }
            }
        }
    }
}

fn similar_patchnames<'a>(
    patchname: &PatchName,
    stack: &impl StackStateAccess<'a>,
) -> Option<String> {
    let similar: Vec<&PatchName> = stack
        .all_patches()
        .filter(|pn| strsim::jaro_winkler(pn.as_ref(), patchname.as_ref()) > 0.75)
        .collect();
    patchnames_string(&similar)
}

fn patchnames_string(patchnames: &[&PatchName]) -> Option<String> {
    match patchnames.len() {
        0 => None,
        1 => Some(format!("`{}`", patchnames[0])),
        2 => Some(format!("`{}` and `{}`", patchnames[0], patchnames[1])),
        _ => {
            let mut s = String::new();
            for pn in patchnames.iter().take(patchnames.len() - 1) {
                write!(&mut s, "`{pn}`, ").unwrap();
            }
            let last_pn = patchnames.last().unwrap();
            write!(&mut s, "and `{last_pn}`").unwrap();
            Some(s)
        }
    }
}
