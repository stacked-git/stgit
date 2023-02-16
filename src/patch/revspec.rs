// SPDX-License-Identifier: GPL-2.0-only

//! Implementations for patch-aware revision specifications.
//!
//! A StGit revspec is like a git revspec (see gitrevisions(7)), but with a coupld of
//! stack and patch aware features:
//!
//! - The special `{base}` specification may be used to refer to the base commit of the
//!   current stack. This specification may be suffixed in the usual ways, e.g.
//!   `{base}~` may be used to specify the base commit's parent.
//! - Names of patches in the current stack may be specified. E.g. a specification of
//!   `patch` would refer to the patch `patch`'s commit. This is equivalent to
//!   specifying `refs/stacks/<branch>/patch`.

use std::{rc::Rc, str::FromStr};

use anyhow::{Context, Result};

use crate::{
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
};

use super::{
    patchrange, GitRevisionSuffix, PartialRefName, PatchLikeSpec, PatchRange, RangeConstraint,
    RangeRevisionSpec, SingleRevisionSpec, StGitBoundaryRevisions, StGitRevision,
};

/// StGit revision specification error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("invalid StGit revision `{0}`: {1}")]
    InvalidRevision(String, String),

    #[error("revision not found `{0}`")]
    RevisionNotFound(String),

    #[error(transparent)]
    Name(#[from] super::name::Error),

    #[error(transparent)]
    Range(#[from] super::range::Error),

    #[error(transparent)]
    Locator(#[from] super::locator::Error),
}

impl AsRef<str> for GitRevisionSuffix {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<str> for PartialRefName {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl FromStr for RangeRevisionSpec {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(super::parse::range_revision_spec))(s)
            .map(|(_, spec)| spec)
            .map_err(|e| Error::InvalidRevision(s.to_string(), e.to_string()))
    }
}

impl FromStr for SingleRevisionSpec {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(super::parse::single_revision_spec))(s)
            .map(|(_, spec)| spec)
            .map_err(|e| Error::InvalidRevision(s.to_string(), e.to_string()))
    }
}

impl FromStr for PatchLikeSpec {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use nom::combinator::{all_consuming, complete};
        complete(all_consuming(super::parse::patch_like_spec))(s)
            .map(|(_, patch_like)| patch_like)
            .map_err(|e| e.to_string())
    }
}

impl std::fmt::Display for RangeRevisionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RangeRevisionSpec::BranchRange { branch, bounds } => write!(f, "{branch}:{bounds}"),
            RangeRevisionSpec::Range(bounds) => write!(f, "{bounds}"),
            RangeRevisionSpec::Single(spec) => write!(f, "{spec}"),
        }
    }
}

impl std::fmt::Display for SingleRevisionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SingleRevisionSpec::Branch { branch, patch_like } => write!(f, "{branch}:{patch_like}"),
            SingleRevisionSpec::PatchAndGitLike(_, git_like) => f.write_str(git_like),
            SingleRevisionSpec::GitLike(git_like) => f.write_str(git_like),
            SingleRevisionSpec::PatchLike(patch_like) => write!(f, "{patch_like}"),
        }
    }
}

impl std::fmt::Display for PatchLikeSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.patch_loc, self.suffix)
    }
}

impl std::fmt::Display for PartialRefName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl std::fmt::Display for GitRevisionSuffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl RangeRevisionSpec {
    /// Resolve begin and end boundary revisions from a range revision specification.
    pub(crate) fn resolve_revisions<'repo>(
        &self,
        repo: &'repo gix::Repository,
        stack: Option<&'repo impl StackAccess<'repo>>,
        use_applied_boundary: bool,
    ) -> Result<StGitBoundaryRevisions<'repo>> {
        match self {
            RangeRevisionSpec::BranchRange { branch, bounds } => {
                let stack = Stack::from_branch(
                    repo,
                    Some(branch.as_ref()),
                    InitializationPolicy::AllowUninitialized,
                )?;
                bounds
                    .resolve_revisions(&stack, use_applied_boundary)
                    .map_err(anyhow::Error::from)
            }
            RangeRevisionSpec::Range(bounds) => {
                if let Some(stack) = stack {
                    bounds
                        .resolve_revisions(stack, use_applied_boundary)
                        .map_err(anyhow::Error::from)
                } else {
                    let stack =
                        Stack::from_branch(repo, None, InitializationPolicy::AllowUninitialized)?;
                    bounds
                        .resolve_revisions(&stack, use_applied_boundary)
                        .map_err(anyhow::Error::from)
                }
            }
            RangeRevisionSpec::Single(single_spec) => {
                let rev = single_spec.resolve(repo, stack)?;
                Ok(StGitBoundaryRevisions::Single(rev))
            }
        }
    }
}

/// Resolve many ranged revision specifications.
pub(crate) fn resolve<'a, 'repo>(
    repo: &'repo gix::Repository,
    stack: Option<&'repo impl StackAccess<'repo>>,
    specs: impl IntoIterator<Item = &'a RangeRevisionSpec>,
    allow: RangeConstraint,
) -> Result<Vec<StGitRevision<'repo>>> {
    let mut revs = Vec::new();
    for spec in specs {
        match spec {
            RangeRevisionSpec::BranchRange { branch, bounds } => {
                let stack = Stack::from_branch(
                    repo,
                    Some(branch.as_ref()),
                    InitializationPolicy::AllowUninitialized,
                )?;
                let range = PatchRange::from(bounds);
                for patchname in patchrange::resolve_names(&stack, [&range], allow)? {
                    let commit = stack.get_patch_commit(&patchname).clone();
                    let patchname = Some(patchname);
                    revs.push(StGitRevision { patchname, commit });
                }
            }
            RangeRevisionSpec::Range(bounds) => {
                let range = PatchRange::from(bounds);
                if let Some(stack) = stack {
                    for patchname in patchrange::resolve_names(stack, [&range], allow)? {
                        let commit = stack.get_patch_commit(&patchname).clone();
                        let patchname = Some(patchname);
                        revs.push(StGitRevision { patchname, commit });
                    }
                } else {
                    let stack =
                        Stack::from_branch(repo, None, InitializationPolicy::AllowUninitialized)?;
                    for patchname in patchrange::resolve_names(&stack, [&range], allow)? {
                        let commit = stack.get_patch_commit(&patchname).clone();
                        let patchname = Some(patchname);
                        revs.push(StGitRevision { patchname, commit });
                    }
                }
            }
            RangeRevisionSpec::Single(single_spec) => {
                let rev = single_spec.resolve(repo, stack)?;
                revs.push(rev);
            }
        }
    }
    Ok(revs)
}

impl PatchLikeSpec {
    /// Resolve a patch-like revision specification.
    pub(crate) fn resolve<'a, 'repo>(
        &'a self,
        repo: &'repo gix::Repository,
        stack: &'a impl StackAccess<'repo>,
    ) -> Result<StGitRevision<'repo>> {
        let rev = self.patch_loc.resolve_revision(stack)?;
        if self.suffix.as_ref().is_empty() {
            Ok(rev)
        } else {
            let spec = format!("{}{}", rev.commit.id, self.suffix.as_ref());
            let object = repo.rev_parse_single_ex(&spec)?.object()?;
            let commit = object.peel_tags_to_end()?.try_into_commit()?;
            Ok(StGitRevision {
                patchname: None,
                commit: Rc::new(commit),
            })
        }
    }

    /// Resolve a patch-like revision specification into a [`gix::Object`].
    pub(crate) fn resolve_object<'a, 'repo>(
        &'a self,
        repo: &'repo gix::Repository,
        stack: &'a impl StackAccess<'repo>,
    ) -> Result<gix::Object<'repo>> {
        let rev = self.patch_loc.resolve_revision(stack)?;
        if self.suffix.as_ref().is_empty() {
            Ok(rev.commit.id().object()?)
        } else {
            let spec = format!("{}{}", rev.commit.id, self.suffix.as_ref());
            Ok(repo.rev_parse_single_ex(&spec)?.object()?)
        }
    }
}

/// Resolve git-like revision specification.
fn resolve_git_like<'repo>(
    repo: &'repo gix::Repository,
    spec: &str,
) -> Result<StGitRevision<'repo>> {
    let object = repo.rev_parse_single_ex(spec)?.object()?;
    let commit = object.peel_tags_to_end()?.try_into_commit()?;
    // TODO: try to find a patch mapping to this commit in the stack?
    Ok(StGitRevision {
        patchname: None,
        commit: Rc::new(commit),
    })
}

impl SingleRevisionSpec {
    /// Resolve single revision specification.
    pub(crate) fn resolve<'repo>(
        &self,
        repo: &'repo gix::Repository,
        stack: Option<&impl StackAccess<'repo>>,
    ) -> Result<StGitRevision<'repo>> {
        match self {
            SingleRevisionSpec::Branch { branch, patch_like } => {
                let stack = Stack::from_branch(
                    repo,
                    Some(branch.as_ref()),
                    InitializationPolicy::AllowUninitialized,
                )?;
                patch_like.resolve(repo, &stack)
            }
            SingleRevisionSpec::PatchAndGitLike(patch_like, git_like) => {
                if let Some(stack) = stack {
                    patch_like
                        .resolve(repo, stack)
                        .or_else(|e| resolve_git_like(repo, git_like).map_err(|_| e))
                } else if let Ok(stack) =
                    Stack::from_branch(repo, None, InitializationPolicy::AllowUninitialized)
                {
                    patch_like
                        .resolve(repo, &stack)
                        .or_else(|e| resolve_git_like(repo, git_like).map_err(|_| e))
                } else {
                    resolve_git_like(repo, git_like)
                }
            }
            SingleRevisionSpec::GitLike(spec) => resolve_git_like(repo, spec),
            SingleRevisionSpec::PatchLike(patch_like) => {
                if let Some(stack) = stack {
                    patch_like.resolve(repo, stack)
                } else {
                    let stack =
                        Stack::from_branch(repo, None, InitializationPolicy::AllowUninitialized)
                            .with_context(|| format!(
                                "initializing stack from current branch for patch-like revision {patch_like}"
                            ))?;
                    patch_like.resolve(repo, &stack)
                }
            }
        }
    }

    /// Resolve single revision specification into a [`gix::Object`].
    pub(crate) fn resolve_object<'repo>(
        &self,
        repo: &'repo gix::Repository,
        stack: &impl StackAccess<'repo>,
    ) -> Result<gix::Object<'repo>> {
        let object = match self {
            SingleRevisionSpec::Branch { branch, patch_like } => {
                let stack = Stack::from_branch(
                    repo,
                    Some(branch.as_ref()),
                    InitializationPolicy::AllowUninitialized,
                )?;
                patch_like.resolve_object(repo, &stack)?
            }
            SingleRevisionSpec::PatchAndGitLike(patch_like, git_like) => {
                patch_like.resolve_object(repo, stack).or_else(|e| {
                    repo.rev_parse_single_ex(git_like)
                        .map_err(anyhow::Error::from)
                        .and_then(|id| id.object().map_err(anyhow::Error::from))
                        .map_err(|_| e)
                })?
            }
            SingleRevisionSpec::GitLike(name_suffix) => {
                repo.rev_parse_single_ex(name_suffix)?.object()?
            }
            SingleRevisionSpec::PatchLike(patch_like) => patch_like.resolve_object(repo, stack)?,
        };
        Ok(object)
    }

    /// Resolve single revision specification into a [`gix::Tree`].
    pub(crate) fn resolve_tree<'repo>(
        &self,
        repo: &'repo gix::Repository,
        stack: &impl StackAccess<'repo>,
    ) -> Result<gix::Tree<'repo>> {
        Ok(self.resolve_object(repo, stack)?.peel_to_tree()?)
    }
}
