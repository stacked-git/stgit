use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum Error {
    // #[error("invalid patch name: {0}")]
    // InvalidPatchName(String),
    #[error("Git2 error")]
    GitError {
        #[from]
        source: git2::Error,
    },

    #[error("Python error")]
    PythonError {
        #[from]
        source: pyo3::PyErr,
    },

    #[error("JSON deserialize error")]
    JsonError {
        #[from]
        source: serde_json::Error,
    },

    #[error("not on branch, HEAD is detached")]
    HeadDetached,

    #[error("branch `{0}` not initialized")]
    StGitStackNotInitialized(String),

    #[error("stack metadata not found")]
    StGitStackMetadataNotFound,

    #[error("")]
    IoError(#[from] std::io::Error),

    #[error("")]
    FormatError(#[from] std::fmt::Error),
}
