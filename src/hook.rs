// SPDX-License-Identifier: GPL-2.0-only

//! Support for using git repository hooks.

use std::{io::Write, path::PathBuf};

use anyhow::{anyhow, Context, Result};

use crate::wrap::Message;

/// Find path to hook script given a hook name.
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

/// Run the git `pre-commit` hook script.
///
/// The `use_editor` flag determines whether the hook should be allowed to invoke an
/// interactive editor.
///
/// Returns `Ok(true)` if the hook ran and completed successfully, `Err()` if the hook ran but failed,
/// and `Ok(false)` if the hook did not run due to the script not existing, not being a file, or not
/// being executable.
pub(crate) fn run_pre_commit_hook(repo: &git2::Repository, use_editor: bool) -> Result<bool> {
    let hook_name = "pre-commit";
    let hook_path = get_hook_path(repo, hook_name);
    let hook_meta = match std::fs::metadata(&hook_path) {
        Ok(meta) => meta,
        Err(_) => return Ok(false), // ignore missing hook
    };

    if !hook_meta.is_file() {
        return Ok(false);
    }

    // Ignore non-executable hooks
    if !is_executable(&hook_meta) {
        return Ok(false);
    }

    let mut hook_command = std::process::Command::new(hook_path);
    let workdir = repo
        .workdir()
        .expect("should not get this far with a bare repo");
    if !use_editor {
        hook_command.env("GIT_EDITOR", ":");
    }

    let status = hook_command
        .current_dir(workdir)
        .stdin(std::process::Stdio::null())
        .status()
        .with_context(|| format!("`{hook_name}` hook"))?;

    if status.success() {
        Ok(true)
    } else {
        Err(anyhow!(
            "`{hook_name}` hook returned {}",
            status.code().unwrap_or(-1)
        ))
    }
}

/// Run the git `commit-msg` hook script.
///
/// The given commit message is written to a temporary file before invoking the
/// `commit-msg` script, and deleted after the script exits.
///
/// The `use_editor` flag determines whether the hook should be allowed to invoke an
/// interactive editor.
///
/// Returns successfully if the hook script does not exist, is not a file, or is not
/// executable.
pub(crate) fn run_commit_msg_hook<'repo>(
    repo: &git2::Repository,
    message: Message<'repo>,
    use_editor: bool,
) -> Result<Message<'repo>> {
    let hook_name = "commit-msg";
    let hook_path = get_hook_path(repo, hook_name);
    let hook_meta = match std::fs::metadata(&hook_path) {
        Ok(meta) => meta,
        Err(_) => return Ok(message), // ignore missing hook
    };

    if !hook_meta.is_file() {
        return Ok(message);
    }

    // Ignore non-executable hooks
    if !is_executable(&hook_meta) {
        return Ok(message);
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
    if !use_editor {
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
                anyhow!("Message could not be decoded with `{}`", encoding.name())
                    .context("`{hook_name}` hook")
            })?;
        Ok(Message::from(message.to_string()))
    } else {
        Err(anyhow!(
            "`{hook_name}` hook returned {}",
            status.code().unwrap_or(-1)
        ))
    }
}

#[cfg(unix)]
fn is_executable(meta: &std::fs::Metadata) -> bool {
    use std::os::unix::fs::MetadataExt;
    meta.mode() & 0o111 != 0
}

#[cfg(not(unix))]
fn is_executable(_meta: &std::fs::Metadata) -> bool {
    true
}
