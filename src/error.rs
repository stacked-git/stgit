use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum Error {
    #[error("Git2: {0}")]
    GitError(#[from] git2::Error),

    #[error("Python: {0}")]
    PythonError(#[from] pyo3::PyErr),

    #[error("JSON deserialize error: {0}")]
    JsonError(#[from] serde_json::Error),

    #[error("{0}")]
    IoError(#[from] std::io::Error),

    #[error("{0}")]
    FormatError(#[from] std::fmt::Error),

    #[error("{0}")]
    PatchNameError(#[from] crate::patchname::Error),

    #[error("patch `{0}` already exists")]
    PatchNameExists(String),

    #[error("not on branch, HEAD is detached")]
    HeadDetached,

    #[error("branch `{0}` not initialized")]
    StGitStackNotInitialized(String),

    #[error("branch `{0}` already initialized")]
    StGitStackAlreadyInitialized(String),

    #[error("stack metadata not found")]
    StGitStackMetadataNotFound,

    #[error("non-UTF-8 branch name `{0}`")]
    NonUtf8BranchName(String),

    #[error("non-UTF-8 {0} `{1}`")]
    NonUtf8Argument(String, String),

    #[error("file `{0}` contains non-UTF-8 data")]
    NonUtf8File(String),

    #[error("patch description contains non-UTF-8 data")]
    NonUtf8PatchDescription,

    #[error("failed to parse patch description: {0}")]
    PatchDescriptionParseError(String),

    #[error("resolve outstanding conflicts first")]
    OutstandingConflicts,

    #[error("invalid name and email `{0}`")]
    InvalidNameEmail(String),

    #[error("invalid date `{0}`")]
    InvalidDate(String),

    #[error("problem with the editor `{0}`")]
    EditorFail(String),

    #[error("`{0}` hook: {1}")]
    HookError(String, String),
}
