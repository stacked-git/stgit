// SPDX-License-Identifier: GPL-2.0-only

//! Extenstion traits.

mod commit;
mod repository;
mod signature;
mod time;

pub(crate) use commit::CommitExtended;
pub(crate) use repository::{CommitOptions, RepositoryExtended};
pub(crate) use signature::SignatureExtended;
pub(crate) use time::TimeExtended;
