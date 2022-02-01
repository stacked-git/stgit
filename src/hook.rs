use std::{io::Write, path::PathBuf};

use anyhow::{anyhow, Context, Result};

use crate::commit::CommitMessage;

fn get_hook_path(repo: &git2::Repository, hook_name: &str) -> PathBuf {
    let hooks_path = if let Ok(config) = repo.config() {
        config
            .get_path("core.hookspath")
            .unwrap_or_else(|_| PathBuf::from("hooks"))
    } else {
        PathBuf::from("hooks")
    };
    let hooks_root = if hooks_path.is_absolute() {
        hooks_path
    } else {
        repo.path().join(hooks_path)
    };
    hooks_root.join(hook_name)
}

pub(crate) fn run_pre_commit_hook(repo: &git2::Repository, use_editor: bool) -> Result<()> {
    let hook_name = "pre-commit";
    let hook_path = get_hook_path(repo, hook_name);
    let hook_meta = match std::fs::metadata(&hook_path) {
        Ok(meta) => meta,
        Err(_) => return Ok(()), // ignore missing hook
    };

    if !hook_meta.is_file() {
        return Ok(());
    }

    if cfg!(unix) {
        use std::os::linux::fs::MetadataExt;
        // Ignore non-executable hooks
        if hook_meta.st_mode() & 0o111 == 0 {
            return Ok(());
        }
    }

    let mut hook_command = std::process::Command::new(hook_path);
    let workdir = repo
        .workdir()
        .expect("should not get this far with a bare repo");
    hook_command.current_dir(workdir);
    if !use_editor {
        hook_command.env("GIT_EDITOR", ":");
    }
    let index = repo.index().expect("gotta have an index");
    let index_path = index.path().expect("index must be a file");
    hook_command.env("GIT_INDEX_FILE", index_path);
    hook_command.stdin(std::process::Stdio::null());

    let status = hook_command
        .status()
        .with_context(|| format!("`{hook_name}` hook"))?;

    if status.success() {
        Ok(())
    } else {
        Err(anyhow!(
            "`{hook_name}` hook returned {}",
            status.code().unwrap_or(-1)
        ))
    }
}

pub(crate) fn run_commit_msg_hook<'repo>(
    repo: &git2::Repository,
    message: CommitMessage<'repo>,
    editor_is_used: bool,
) -> Result<CommitMessage<'repo>> {
    let hook_name = "commit-msg";
    let hook_path = get_hook_path(repo, hook_name);
    let hook_meta = match std::fs::metadata(&hook_path) {
        Ok(meta) => meta,
        Err(_) => return Ok(message), // ignore missing hook
    };

    if !hook_meta.is_file() {
        return Ok(message);
    }

    if cfg!(unix) {
        use std::os::linux::fs::MetadataExt;
        // Ignore non-executable hooks
        if hook_meta.st_mode() & 0o111 == 0 {
            return Ok(message);
        }
    }

    let mut msg_file = tempfile::NamedTempFile::new()?;
    msg_file.write_all(message.raw_bytes())?;
    let msg_file_path = msg_file.into_temp_path();

    let index = repo.index()?;
    let index_path = index.path().expect("repo's default index must be a file");

    // TODO: when git runs this hook, it only sets GIT_INDEX_FILE and sometimes
    // GIT_EDITOR. So author and committer vars are not clearly required.
    let mut hook_command = std::process::Command::new(hook_path);
    hook_command.env("GIT_INDEX_FILE", index_path);
    if !editor_is_used {
        hook_command.env("GIT_EDITOR", ":");
    }

    hook_command.arg(&msg_file_path);

    let status = hook_command
        .status()
        .with_context(|| format!("`{hook_name}` hook"))?;

    if status.success() {
        let message_bytes = std::fs::read(&msg_file_path)?;
        let encoding = message.encoding()?;
        let message = encoding
            .decode_without_bom_handling_and_without_replacement(&message_bytes)
            .ok_or_else(|| {
                anyhow!("message could not be decoded with `{}`", encoding.name())
                    .context("`{hook_name}` hook")
            })?;
        Ok(CommitMessage::from(message.to_string()))
    } else {
        Err(anyhow!(
            "`{hook_name}` hook returned {}",
            status.code().unwrap_or(-1)
        ))
    }
}
