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

    #[error("not on branch, HEAD is detached")]
    HeadDetached,

    #[error("branch `{0}` not initialized")]
    StGitStackNotInitialized(String),

    #[error("branch `{0}` already initialized")]
    StGitStackAlreadyInitialized(String),

    #[error("stack metadata not found")]
    StGitStackMetadataNotFound,

    #[error("non-UTF-8 name encountered")]
    StGitNonUtf8Name,
}
