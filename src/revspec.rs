// SPDX-License-Identifier: GPL-2.0-only

//! Parse patch-aware revision specifications.
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

use anyhow::Result;

use crate::stack::{Stack, StackAccess};

/// StGit revision specification error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("Invalid StGit revision `{0}`")]
    InvalidRevision(String),

    #[error("Revision not found `{0}`")]
    RevisionNotFound(String),
}

/// Parse a StGit revision specification into constituent branch and remainder spec.
///
/// The general form of a StGit revision specification is `[<branch>:]<spec>`.
///
/// The provided `branch` argument is used as a fallback if the provided `spec` does not
/// contain an embedded branch specification. Any branch specified in `spec` takes
/// precedence over the `branch` argument.
///
/// Returns a tuple of the branch and spec remainder strings. It is possible for either
/// or both to be `None`.
pub(crate) fn parse_branch_and_spec<'a>(
    branch: Option<&'a str>,
    spec: Option<&'a str>,
) -> (Option<&'a str>, Option<&'a str>) {
    if let Some(spec) = spec {
        if let Some((branch, spec)) = spec.split_once(':') {
            // The branch from the spec string overrides the branch argument.
            if spec.is_empty() {
                (Some(branch), None)
            } else {
                (Some(branch), Some(spec))
            }
        } else {
            (branch, Some(spec))
        }
    } else {
        (branch, None)
    }
}

/// Parse and lookup StGit revision specification.
pub(crate) fn parse_stgit_revision<'repo>(
    repo: &'repo git2::Repository,
    spec: Option<&str>,
    branch: Option<&str>,
) -> Result<git2::Object<'repo>> {
    let (branch, spec) = parse_branch_and_spec(branch, spec);

    if let Some(spec) = spec {
        let stack = Stack::from_branch(repo, branch)?;
        if let Some((_, spec)) = spec.split_once("{base}") {
            let revspec = format!("{}{spec}", stack.base().id());
            revparse_single(repo, &revspec)
        } else {
            let patch_revspec = stack.patch_revspec(spec);
            revparse_single(repo, &patch_revspec).or_else(|_| revparse_single(repo, spec))
        }
    } else if let Some(branch) = branch {
        revparse_single(repo, branch)
    } else {
        let object = repo.head()?.peel(git2::ObjectType::Any)?;
        Ok(object)
    }
}

/// [`git2::Repository::revparse_single()`] with StGit-specific error mapping.
fn revparse_single<'repo>(
    repo: &'repo git2::Repository,
    spec: &str,
) -> Result<git2::Object<'repo>> {
    let object = repo.revparse_single(spec).map_err(|e| -> anyhow::Error {
        match e.code() {
            git2::ErrorCode::InvalidSpec => Error::InvalidRevision(spec.to_string()).into(),
            git2::ErrorCode::NotFound => Error::RevisionNotFound(spec.to_string()).into(),
            _ => e.into(),
        }
    })?;
    Ok(object)
}
