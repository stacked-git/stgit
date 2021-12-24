use std::{ffi::OsStr, io::Write};

use crate::{error::Error, signature::get_epoch_time_string};

pub(crate) struct CommitData {
    pub author: git2::Signature<'static>,
    pub committer: git2::Signature<'static>,
    pub message: String,
    pub tree_id: git2::Oid,
    pub parent_ids: Vec<git2::Oid>,
}

impl CommitData {
    pub fn new(
        author: git2::Signature<'static>,
        committer: git2::Signature<'static>,
        message: String,
        tree_id: git2::Oid,
        parent_ids: Vec<git2::Oid>,
    ) -> Self {
        Self {
            author,
            committer,
            message,
            tree_id,
            parent_ids,
        }
    }

    pub fn replace_message(self, message: String) -> Self {
        Self { message, ..self }
    }

    pub fn commit(self, repo: &git2::Repository) -> Result<git2::Oid, Error> {
        commit_ex(
            repo,
            &self.author,
            &self.committer,
            &self.message,
            self.tree_id,
            self.parent_ids,
        )
    }
}

impl From<&git2::Commit<'_>> for CommitData {
    fn from(commit: &git2::Commit<'_>) -> Self {
        Self {
            author: commit.author().to_owned(),
            committer: commit.committer().to_owned(),
            message: commit
                .message_raw()
                .expect("Cannot extract CommitData from non-utf-8 encoded Commit")
                .to_string(),
            tree_id: commit.tree_id(),
            parent_ids: commit.parent_ids().collect(),
        }
    }
}

pub(crate) fn commit_ex(
    repo: &git2::Repository,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &str,
    tree_id: git2::Oid,
    parent_ids: impl IntoIterator<Item = git2::Oid>,
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
            parents.push(repo.find_commit(parent_id)?);
        }
        let parents: Vec<&git2::Commit<'_>> = parents.iter().collect();

        Ok(repo.commit(None, author, committer, message, &tree, &parents)?)
    }
}

#[inline]
fn git_commit_tree(
    repo_path: &std::path::Path,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &[u8],
    tree_id: git2::Oid,
    parent_ids: impl IntoIterator<Item = git2::Oid>,
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
        use std::os::unix::ffi::OsStrExt;
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
