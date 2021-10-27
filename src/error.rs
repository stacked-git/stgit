use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum Error {
    #[error("Git2: {0}")]
    Git(#[from] git2::Error),

    #[error("Python: {0}")]
    Python(#[from] pyo3::PyErr),

    #[error("JSON deserialize error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("{0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Format(#[from] std::fmt::Error),

    #[error("{0}")]
    PatchName(#[from] crate::patchname::Error),

    #[error("patch `{0}` already exists")]
    PatchNameExists(String),

    #[error("not on branch, HEAD is detached")]
    HeadDetached,

    #[error("branch `{0}` not found")]
    BranchNotFound(String),

    #[error("branch `{0}` not initialized")]
    StackNotInitialized(String),

    #[error("branch `{0}` already initialized")]
    StackAlreadyInitialized(String),

    #[error("stack metadata not found")]
    StackMetadataNotFound,

    #[error("non-UTF-8 branch name `{0}`")]
    NonUtf8BranchName(String),

    #[error("non-UTF-8 {0} `{1}`")]
    NonUtf8Argument(String, String),

    #[error("file `{0}` contains non-UTF-8 data")]
    NonUtf8File(String),

    #[error("patch description contains non-UTF-8 data")]
    NonUtf8PatchDescription,

    #[error("failed to parse patch description: {0}")]
    ParsePatchDescription(String),

    #[error("resolve outstanding conflicts first")]
    OutstandingConflicts,

    #[error("invalid name and email `{0}`")]
    InvalidNameEmail(String),

    #[error("invalid date `{0}`")]
    InvalidDate(String),

    #[error("problem with the editor `{0}`")]
    EditorFail(String),

    #[error("`{0}` hook: {1}")]
    Hook(String, String),
}
