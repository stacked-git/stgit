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

pub(crate) fn repo_state_to_str(state: git2::RepositoryState) -> &'static str {
    match state {
        git2::RepositoryState::Clean => "clean",
        git2::RepositoryState::Merge => "merge",
        git2::RepositoryState::Revert | git2::RepositoryState::RevertSequence => "revert",
        git2::RepositoryState::CherryPick | git2::RepositoryState::CherryPickSequence => {
            "cherry-pick"
        }
        git2::RepositoryState::Bisect => "bisect",
        git2::RepositoryState::Rebase => "rebase",
        git2::RepositoryState::RebaseInteractive => "interactive rebase",
        git2::RepositoryState::RebaseMerge => "rebase merge",
        git2::RepositoryState::ApplyMailbox => "apply mailbox",
        git2::RepositoryState::ApplyMailboxOrRebase => "rebase or apply mailbox",
    }
}
