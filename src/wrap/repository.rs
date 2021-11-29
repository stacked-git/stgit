#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;
use std::{ffi::OsStr, io::Write};

use crate::{error::Error, wrap::signature::get_epoch_time_string};

pub(crate) fn commit_ex(
    repo: &git2::Repository,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &str,
    tree_id: git2::Oid,
    parent_ids: &[git2::Oid],
) -> Result<git2::Oid, Error> {
    let (gpgsign, commit_encoding) = if let Ok(config) = repo.config() {
        let gpgsign = config.get_bool("commit.gpgsign").unwrap_or(false);
        let encoding = config.get_string("i18n.commitencoding").ok();
        (gpgsign, encoding)
    } else {
        (false, None)
    };
    if gpgsign || commit_encoding.is_some() {
        // TODO: encode message
        // Use git for any commit that needs to be signed
        git_commit_tree(
            repo.path(),
            author,
            committer,
            message.as_bytes(),
            tree_id,
            parent_ids,
            gpgsign,
        )
    } else {
        // Use git2 for all other occasions
        let tree = repo.find_tree(tree_id)?;
        let mut parents: Vec<git2::Commit<'_>> = Vec::new();
        for parent_id in parent_ids {
            parents.push(repo.find_commit(*parent_id)?);
        }
        let parents: Vec<&git2::Commit<'_>> = parents.iter().collect();

        Ok(repo.commit(None, author, committer, message, &tree, &parents)?)
    }
}

pub(crate) fn get_branch<'repo>(
    repo: &'repo git2::Repository,
    branch_name: Option<&str>,
) -> Result<git2::Branch<'repo>, Error> {
    if let Some(name) = branch_name {
        let branch = repo
            .find_branch(name, git2::BranchType::Local)
            .map_err(|e| {
                if e.class() == git2::ErrorClass::Reference {
                    match e.code() {
                        git2::ErrorCode::NotFound => Error::BranchNotFound(name.to_string()),
                        git2::ErrorCode::InvalidSpec => Error::InvalidBranchName(name.to_string()),
                        git2::ErrorCode::UnbornBranch => Error::UnbornBranch(format!("`{}`", name)),
                        _ => e.into(),
                    }
                } else {
                    e.into()
                }
            })?;
        Ok(branch)
    } else if repo.head_detached()? {
        Err(Error::HeadDetached)
    } else {
        let head = repo.head().map_err(|e| {
            if e.code() == git2::ErrorCode::UnbornBranch {
                Error::UnbornBranch(e.message().to_string())
            } else {
                e.into()
            }
        })?;
        if head.is_branch() {
            Ok(git2::Branch::wrap(head))
        } else {
            Err(Error::HeadNotBranch(
                String::from_utf8_lossy(head.name_bytes()).to_string(),
            ))
        }
    }
}

#[inline]
fn git_commit_tree(
    repo_path: &std::path::Path,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &[u8],
    tree_id: git2::Oid,
    parent_ids: &[git2::Oid],
    gpgsign: bool,
) -> Result<git2::Oid, Error> {
    let mut command = std::process::Command::new("git");
    command.arg("commit-tree").arg(format!("{}", tree_id));
    for parent_id in parent_ids {
        command.arg("-p").arg(format!("{}", parent_id));
    }
    if gpgsign {
        command.arg("-S");
    }
    if cfg!(unix) {
        command
            .env("GIT_AUTHOR_NAME", OsStr::from_bytes(author.name_bytes()))
            .env("GIT_AUTHOR_EMAIL", OsStr::from_bytes(author.email_bytes()))
            .env(
                "GIT_COMMITTER_NAME",
                OsStr::from_bytes(committer.name_bytes()),
            )
            .env(
                "GIT_COMMITTER_EMAIL",
                OsStr::from_bytes(committer.email_bytes()),
            )
            // TODO: reencode dates?
            .env("GIT_AUTHOR_DATE", get_epoch_time_string(author.when()))
            .env(
                "GIT_COMMITTER_DATE",
                get_epoch_time_string(committer.when()),
            );
    } else {
        command
            .env(
                "GIT_AUTHOR_NAME",
                &author
                    .name()
                    .expect("author name must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_AUTHOR_EMAIL",
                &author
                    .email()
                    .expect("author email must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_COMMITTER_NAME",
                &committer
                    .name()
                    .expect("committer name must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_COMMITTER_EMAIL",
                &committer
                    .email()
                    .expect("committer email must be valid utf-8 on non-unix"),
            )
            .env("GIT_AUTHOR_DATE", get_epoch_time_string(author.when()))
            .env(
                "GIT_COMMITTER_DATE",
                get_epoch_time_string(committer.when()),
            );
    }

    command
        .env("GIT_DIR", repo_path)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());

    let mut child = command.spawn()?;

    {
        let mut stdin = child
            .stdin
            .take()
            .expect("failed to open stdin of `git commit-tree`");

        stdin.write_all(message)?;
    }

    let output = child.wait_with_output()?;
    if output.status.success() {
        let output_str =
            std::str::from_utf8(&output.stdout).expect("`git commit-tree` output non-UTF8 oid");
        Ok(git2::Oid::from_str(output_str.trim_end())?)
    } else {
        Err(Error::Generic(format!(
            "`git commit-tree`: {} ({})",
            String::from_utf8_lossy(&output.stderr).trim_end(),
            output.status.code().unwrap_or(-1)
        )))
    }
}
