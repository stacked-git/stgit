// SPDX-License-Identifier: GPL-2.0-only

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("{0}")]
    CheckoutConflicts(String),

    #[error("{0}")]
    CausedConflicts(String),

    #[error("resolve outstanding conflicts first")]
    OutstandingConflicts,

    #[error("no patches applied")]
    NoAppliedPatches,

    #[error("{msg}")]
    TransactionHalt { msg: String, conflicts: bool },
}
