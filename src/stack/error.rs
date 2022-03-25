#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("complete the in-progress `{0}` before trying again")]
    ActiveRepositoryState(String),

    #[error("{0}")]
    CheckoutConflicts(String),

    #[error("{0}")]
    FoldConflicts(String),

    #[error("resolve outstanding conflicts first")]
    OutstandingConflicts,

    // TODO: lowercase
    #[error("No patches applied")]
    NoAppliedPatches,

    #[error("{0}\nCommand aborted (all changes rolled back)")]
    TransactionAborted(String),

    #[error("{msg}")]
    TransactionHalt { msg: String, conflicts: bool },
}
