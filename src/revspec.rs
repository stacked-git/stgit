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

use anyhow::{anyhow, Result};

use crate::stack::{InitializationPolicy, Stack, StackAccess};

/// StGit revision specification error variants.
#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("invalid StGit revision `{0}`: {1}")]
    InvalidRevision(String, String),

    #[error("revision not found `{0}`")]
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
    repo: &'repo git_repository::Repository,
    spec: Option<&str>,
    branch: Option<&str>,
) -> Result<git_repository::Object<'repo>> {
    let (branch, spec) = parse_branch_and_spec(branch, spec);

    if let Some(spec) = spec {
        let stack = Stack::from_branch(repo, branch, InitializationPolicy::AllowUninitialized)?;
        if let Some((_, spec)) = spec.split_once("{base}") {
            let revspec = format!("{}{spec}", stack.base().id);
            rev_parse_single(repo, &revspec)
        } else {
            let patch_revspec = stack.patch_revspec(spec);
            rev_parse_single(repo, &patch_revspec).or_else(|_| rev_parse_single(repo, spec))
        }
    } else if let Some(branch) = branch {
        rev_parse_single(repo, branch)
    } else if let Some(id_result) = repo.head()?.into_fully_peeled_id() {
        let id = id_result?;
        Ok(id.object()?)
    } else {
        Err(anyhow!("HEAD is unborn"))
    }
}

/// [`git_repository::Repository::rev_parse_single()`] with StGit-specific error mapping.
fn rev_parse_single<'repo>(
    repo: &'repo git_repository::Repository,
    spec: &str,
) -> Result<git_repository::Object<'repo>> {
    use git_repository::{
        refs::file::find::existing::Error as FindError,
        revision::spec::parse::{single::Error as SingleError, Error as SpecParseError},
    };

    // Catch revspec ranges early because gitoxide will only flag this as an error if
    // both specs in the range are valid.
    if spec.contains("..") {
        return Err(Error::InvalidRevision(
            spec.to_string(),
            "revspec must resolve to a single object".to_string(),
        )
        .into());
    }

    let object = repo
        .rev_parse_single(spec)
        .map_err(|single_err| -> anyhow::Error {
            match single_err {
                SingleError::Parse(inner) => {
                    let mut spec_parse_err = &inner;
                    loop {
                        match spec_parse_err {
                            SpecParseError::FindReference(find_err) => match find_err {
                                e @ FindError::Find(_) => {
                                    break Error::InvalidRevision(spec.to_string(), e.to_string())
                                        .into()
                                }
                                FindError::NotFound { name: _ } => {
                                    break Error::RevisionNotFound(spec.to_string()).into()
                                }
                            },
                            SpecParseError::Multi { current, next } => {
                                if let Some(next) = next {
                                    spec_parse_err = next
                                        .downcast_ref::<SpecParseError>()
                                        .expect("next error is SpecParseError");
                                } else {
                                    spec_parse_err = current
                                        .downcast_ref::<SpecParseError>()
                                        .expect("current error is SpecParseError");
                                }
                            }
                            SpecParseError::SingleNotFound => {
                                break Error::RevisionNotFound(spec.to_string()).into()
                            }
                            e => {
                                break Error::InvalidRevision(spec.to_string(), e.to_string())
                                    .into()
                            }
                        }
                    }
                }
                e @ SingleError::RangedRev { spec: _ } => {
                    Error::InvalidRevision(spec.to_string(), e.to_string()).into()
                }
            }
        })?
        .object()?;
    Ok(object)
}
