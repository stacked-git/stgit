// SPDX-License-Identifier: GPL-2.0-only

//! The StGit stack data structure.
mod access;
mod error;
mod iter;
mod serde;
#[allow(clippy::module_inception)]
mod stack;
mod state;
mod transaction;
mod upgrade;

pub(crate) use access::{StackAccess, StackStateAccess};
pub(crate) use error::Error;
pub(crate) use stack::{
    get_branch_name, state_refname_from_branch_name, InitializationPolicy, Stack,
};
pub(crate) use state::{PatchState, StackState};
pub(crate) use transaction::StackTransaction;
