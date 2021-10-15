use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum Error {
    // #[error("invalid patch name: {0}")]
    // InvalidPatchName(String),
    #[error("Python error")]
    PythonError {
        #[from]
        source: pyo3::PyErr,
    },
}
