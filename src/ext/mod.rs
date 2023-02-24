// SPDX-License-Identifier: GPL-2.0-only

//! Extenstion traits.

mod commit;
mod repository;
mod signature;
mod time;

pub(crate) use self::{
    commit::CommitExtended,
    repository::{CommitOptions, RepositoryExtended},
    signature::SignatureExtended,
    time::TimeExtended,
};
