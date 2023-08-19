// SPDX-License-Identifier: GPL-2.0-only

//! Support for using git repository hooks.

use std::{
    borrow::Cow,
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use bstr::BString;

use crate::wrap::Message;

/// Find path to hook script given a hook name.
///
/// Returns None if the hook script is not found or is not executable.
fn get_hook_path(repo: &gix::Repository, hook_name: &str) -> Result<Option<PathBuf>> {
    let config = repo.config_snapshot();
    let hooks_path =
        if let Some(core_hooks_path) = config.trusted_path("core.hookspath").transpose()? {
            if core_hooks_path.is_absolute() {
                core_hooks_path
            } else if repo.is_bare() {
                // The hooks path is relative to GIT_DIR in the case of a bare repo
                Cow::Owned(repo.common_dir().join(core_hooks_path))
            } else {
                // The hooks path is relative to the root of the working tree otherwise
                let work_dir = repo.work_dir().expect("non-bare repo must have work dir");
                Cow::Owned(work_dir.join(core_hooks_path))
            }
        } else {
            // No core.hookspath, use default .git/hooks location
            Cow::Owned(repo.common_dir().join("hooks"))
        };
    let hook_path = hooks_path.join(hook_name);

    let hook_meta = match std::fs::metadata(&hook_path) {
        Ok(meta) => meta,
        Err(_) => return Ok(None), // ignore missing hook
    };

    if !is_executable(&hook_meta) {
        return Ok(None);
    }

    let hook_path = gix::path::realpath(hook_path)?;

    Ok(Some(hook_path))
}

/// Run the git `pre-commit` hook script.
///
/// The `use_editor` flag determines whether the hook should be allowed to invoke an
/// interactive editor.
///
/// Returns `Ok(true)` if the hook ran and completed successfully, `Err()` if the hook
/// ran but failed, and `Ok(false)` if the hook did not run due to the script not
/// existing, not being a file, or not being executable.
pub(crate) fn run_pre_commit_hook(repo: &gix::Repository, use_editor: bool) -> Result<bool> {
    let hook_name = "pre-commit";
    let hook_path = if let Some(hook_path) = get_hook_path(repo, hook_name)? {
        hook_path
    } else {
        return Ok(false);
    };

    let work_dir = repo.work_dir().expect("not a bare repo");

    let mut hook_command = std::process::Command::new(hook_path);
    hook_command.current_dir(work_dir);
    if !use_editor {
        hook_command.env("GIT_EDITOR", ":");
    }

    let mut hook_command = make_sh_command_on_windows(hook_command);

    let status = hook_command
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
    repo: &gix::Repository,
    message: Message<'repo>,
    use_editor: bool,
) -> Result<Message<'repo>> {
    let hook_name = "commit-msg";
    let hook_path = if let Some(hook_path) = get_hook_path(repo, hook_name)? {
        hook_path
    } else {
        return Ok(message);
    };

    let work_dir = repo.work_dir().expect("not a bare repo");
    let temp_msg = TemporaryMessage::new(work_dir, &message)?;

    let index_path = repo.index_path();

    // TODO: when git runs this hook, it only sets GIT_INDEX_FILE and sometimes
    // GIT_EDITOR. So author and committer vars are not clearly required.
    let mut hook_command = std::process::Command::new(hook_path);
    hook_command.current_dir(work_dir);
    hook_command.env("GIT_INDEX_FILE", &index_path);
    if !use_editor {
        hook_command.env("GIT_EDITOR", ":");
    }

    hook_command.arg(temp_msg.filename());

    let mut hook_command = make_sh_command_on_windows(hook_command);

    let status = hook_command
        .status()
        .with_context(|| format!("`{hook_name}` hook"))?;

    if status.success() {
        let message_bytes = temp_msg.read()?;
        let encoding = message.encoding()?;
        let message = encoding
            .decode_without_bom_handling_and_without_replacement(&message_bytes)
            .ok_or_else(|| {
                anyhow!("message could not be decoded with `{}`", encoding.name())
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

/// Temporary commit message file for commit-msg hook.
///
/// The temporary file is created relative to the work dir using the StGit process id to
/// avoid collisions with other StGit processes.
struct TemporaryMessage<'repo> {
    work_dir: &'repo Path,
    filename: PathBuf,
}

impl<'repo> TemporaryMessage<'repo> {
    /// Create new temporary file containing commit message.
    fn new(work_dir: &'repo Path, message: &Message<'repo>) -> Result<Self> {
        let pid = std::process::id();
        let filename = PathBuf::from(format!(".stgit-msg-temp-{pid}"));
        let msg_path = work_dir.join(&filename);
        let mut msg_file = std::fs::OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(msg_path)?;
        msg_file.write_all(message.raw_bytes())?;
        Ok(Self { work_dir, filename })
    }

    /// Get name of temporary message file.
    ///
    /// This is not a complete path. The temporary file is relative to the `work_dir`.
    fn filename(&self) -> &Path {
        self.filename.as_ref()
    }

    /// Read contents of temporary message file.
    fn read(&self) -> Result<BString> {
        Ok(std::fs::read(self.work_dir.join(&self.filename))?.into())
    }
}

impl<'repo> Drop for TemporaryMessage<'repo> {
    fn drop(&mut self) {
        let msg_path = self.work_dir.join(&self.filename);
        if msg_path.is_file() {
            if let Err(e) = std::fs::remove_file(&msg_path) {
                panic!("failed to remove temp message {msg_path:?}: {e}");
            }
        }
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

#[cfg(not(windows))]
fn make_sh_command_on_windows(command: std::process::Command) -> std::process::Command {
    command
}

#[cfg(windows)]
fn make_sh_command_on_windows(command: std::process::Command) -> std::process::Command {
    let hook_path = command
        .get_program()
        .to_str()
        .expect("path to hook is valid UTF-8");
    assert!(!hook_path.contains('"'));
    assert!(!hook_path.contains('\''));

    let mut command_str = String::new();
    command_str.push('"');
    command_str.push_str(hook_path);
    command_str.push('"');
    for arg in command.get_args() {
        let arg = arg.to_str().expect("hook args are valid UTF-8");
        assert!(!arg.contains('"'));
        assert!(!arg.contains('\''));
        command_str.push(' ');
        command_str.push('"');
        command_str.push_str(arg);
        command_str.push('"');
    }

    let mut sh_command = std::process::Command::new("sh");
    sh_command.arg("-c");
    sh_command.arg(command_str);

    for (k, opt_v) in command.get_envs() {
        if let Some(v) = opt_v {
            sh_command.env(k, v);
        } else {
            sh_command.env_remove(k);
        }
    }

    if let Some(cur_dir) = command.get_current_dir() {
        sh_command.current_dir(cur_dir);
    }

    sh_command
}
