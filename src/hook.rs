#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;
use std::{ffi::OsStr, io::Write, path::PathBuf};

use crate::{
    error::Error,
    wrap::{signature::get_epoch_time_string, CommitData},
};

pub(crate) fn run_commit_msg_hook(
    repo: &git2::Repository,
    commit_data: CommitData,
    editor_is_used: bool,
) -> Result<CommitData, Error> {
    let config = repo.config()?;
    let hooks_path = config
        .get_path("core.hookspath")
        .unwrap_or_else(|_| PathBuf::from("hooks"));
    let hooks_root = if hooks_path.is_absolute() {
        hooks_path
    } else {
        repo.path().join(hooks_path)
    };
    let hook_name = "commit-msg";
    let hook_path = hooks_root.join(hook_name);
    if !hook_path.exists() {
        return Ok(commit_data);
    }

    let mut msg_file = tempfile::NamedTempFile::new()?;
    msg_file.write_all(commit_data.message.as_bytes())?;
    let msg_file_path = msg_file.into_temp_path();

    let index = repo.index()?;
    let index_path = index.path().expect("repo's default index must be a file");

    // TODO: when git runs this hook, it only sets GIT_INDEX_FILE and sometimes
    // GIT_EDITOR. So author and committer vars are not clearly required.
    let mut hook_command = std::process::Command::new(hook_path);
    let author = &commit_data.author;
    let committer = &commit_data.committer;
    if cfg!(unix) {
        hook_command
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
        hook_command
            .env(
                "GIT_AUTHOR_NAME",
                author
                    .name()
                    .expect("author name must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_AUTHOR_EMAIL",
                author
                    .email()
                    .expect("author email must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_COMMITTER_NAME",
                committer
                    .name()
                    .expect("committer name must be valid utf-8 on non-unix"),
            )
            .env(
                "GIT_COMMITTER_EMAIL",
                committer
                    .email()
                    .expect("committer email must be valid utf-8 on non-unix"),
            )
            .env("GIT_AUTHOR_DATE", get_epoch_time_string(author.when()))
            .env(
                "GIT_COMMITTER_DATE",
                get_epoch_time_string(committer.when()),
            );
    }
    hook_command.env("GIT_INDEX_FILE", index_path);
    if !editor_is_used {
        hook_command.env("GIT_EDITOR", ":");
    }

    hook_command.arg(&msg_file_path);

    let status = hook_command
        .status()
        .map_err(|e| Error::Hook(hook_name.to_string(), e.to_string()))?;

    if status.success() {
        let message_bytes = std::fs::read(&msg_file_path)?;
        let message = String::from_utf8(message_bytes).map_err(|_| {
            Error::Hook(
                hook_name.to_string(),
                "message is not valid UTF-8".to_string(),
            )
        })?;

        Ok(commit_data.replace_message(message))
    } else {
        Err(Error::Hook(
            hook_name.to_string(),
            format!("returned {}", status.code().unwrap_or(-1)),
        ))
    }
}
