// SPDX-License-Identifier: GPL-2.0-only

/// Options for fine-tuning stack transaction behaviors.
pub(super) struct TransactionOptions {
    pub(super) conflict_mode: ConflictMode,
    pub(super) allow_push_conflicts: Option<bool>,
    pub(super) discard_changes: bool,
    pub(super) use_index_and_worktree: bool,
    pub(super) set_head: bool,
    pub(super) allow_bad_head: bool,
    pub(super) committer_date_is_author_date: bool,
}

impl Default for TransactionOptions {
    fn default() -> Self {
        Self {
            conflict_mode: ConflictMode::Disallow,
            allow_push_conflicts: None,
            discard_changes: false,
            use_index_and_worktree: false,
            set_head: true,
            allow_bad_head: false,
            committer_date_is_author_date: false,
        }
    }
}

/// Policies for whether a transaction may execute when conflicts emerge from the
/// transactions operations.
pub(crate) enum ConflictMode {
    /// Transaction execution will fail if there are conflicts recorded in the index.
    ///
    /// This is the default.
    Disallow,

    /// Transaction execution will succeed even if there are outstanding conflicts.
    Allow,

    /// Transaction execution will succeed with conflicts, but only if the topmost patch
    /// is unchanged by the transaction.
    AllowIfSameTop,
}

impl Default for ConflictMode {
    fn default() -> Self {
        Self::Disallow
    }
}
