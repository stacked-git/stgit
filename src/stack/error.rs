#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("{0}")]
    CheckoutConflicts(String),

    #[error("{0}")]
    FoldConflicts(String),

    #[error("resolve outstanding conflicts first")]
    OutstandingConflicts,

    // TODO: lowercase
    #[error("No patches applied")]
    NoAppliedPatches,

    #[error("{msg}")]
    TransactionHalt { msg: String, conflicts: bool },
}
