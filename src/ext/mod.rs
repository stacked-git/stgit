// SPDX-License-Identifier: GPL-2.0-only

//! Extenstion traits.

mod commit;
mod repository;
mod signature;
mod time;

pub(crate) use self::commit::CommitExtended;
pub(crate) use self::repository::{CommitOptions, RepositoryExtended};
pub(crate) use self::signature::SignatureExtended;
pub(crate) use self::time::TimeExtended;
