use std::{io::Write, path::PathBuf};

use crate::{commitdata::CommitData, error::Error};

pub(crate) fn run_commit_msg_hook<'repo>(
    repo: &git2::Repository,
    commit_data: CommitData<'repo>,
    editor_is_used: bool,
) -> Result<CommitData<'repo>, Error> {
    let config = repo.config()?;
    let hooks_path = config
        .get_path("core.hookspath")
        .unwrap_or(PathBuf::from("hooks"));
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
    hook_command
        .env("GIT_AUTHOR_NAME", &commit_data.author.name)
        .env("GIT_AUTHOR_EMAIL", &commit_data.author.email)
        .env("GIT_AUTHOR_DATE", commit_data.author.get_epoch_time_string())
        .env("GIT_COMMITTER_NAME", &commit_data.committer.name)
        .env("GIT_COMMITTER_EMAIL", &commit_data.committer.email)
        .env("GIT_COMMITTER_DATE", commit_data.committer.get_epoch_time_string())
        .env("GIT_INDEX_FILE", index_path);
    if !editor_is_used {
        hook_command.env("GIT_EDITOR", ":");
    }

    hook_command.arg(&msg_file_path);

    hook_command.status().map_err(|e| {
        Error::HookError(hook_name.to_string(), e.to_string())
    })?;

    let message_bytes = std::fs::read(&msg_file_path)?;
    let message = String::from_utf8(message_bytes).map_err(|_| {
        Error::HookError(hook_name.to_string(), "message is not valid UTF-8".to_string())
    })?;

    Ok(commit_data.replace_message(message))
}
