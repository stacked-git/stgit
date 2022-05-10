#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("{0}")]
    CheckoutConflicts(String),

    #[error("{0}")]
    CausedConflicts(String),

    #[error("Resolve outstanding conflicts first")]
    OutstandingConflicts,

    #[error("No patches applied")]
    NoAppliedPatches,

    #[error("{msg}")]
    TransactionHalt { msg: String, conflicts: bool },
}
