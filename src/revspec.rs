use anyhow::Result;

use crate::stack::{Stack, StackStateAccess};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("invalid StGit revision `{0}`")]
    InvalidRevision(String),

    #[error("revision not found `{0}`")]
    RevisionNotFound(String),
}

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

pub(crate) fn parse_stgit_revision<'repo>(
    repo: &'repo git2::Repository,
    spec: Option<&str>,
    branch: Option<&str>,
) -> Result<git2::Object<'repo>> {
    let (branch, spec) = parse_branch_and_spec(branch, spec);

    if let Some(spec) = spec {
        let stack = Stack::from_branch(repo, branch)?;
        if let Some((_, spec)) = spec.split_once("{base}") {
            let revspec = format!("{}{}", stack.base().id(), spec);
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
