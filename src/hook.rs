use std::path::PathBuf;

use crate::{commitdata::CommitData, error::Error};

pub(crate) fn run_commit_msg_hook<'repo>(
    repo: &git2::Repository,
    commit_data: CommitData<'repo>,
    _editor_is_used: bool,
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

    // TODO: Hook environment should include:
    // GIT_AUTHOR_NAME
    // GIT_AUTHOR_EMAIL
    // GIT_AUTHOR_DATE
    // GIT_COMMITTER_NAME
    // GIT_COMMITTER_EMAIL
    // GIT_COMMITTER_DATE
    // and
    // GIT_EDITOR=":" if editor_is_used
    // and
    // GIT_INDEX_FILE (use default index for hooks)

    // TODO: Need to put message in temporary file
    // TODO: Run the hook
    // TODO: Read message from file

    Ok(commit_data) // TODO
}
