// SPDX-License-Identifier: GPL-2.0-only

//! Execute commands with git, the stupid content tracker.
//!
//! Each function in this module calls-out to a specific git command that is useful to
//! StGit. This module originally existed to overcome limitations of libgit2, but
//! remains until gitoxide can replace its behaviors.

mod command;
mod context;
mod diff;
mod oid;
mod status;
mod tempindex;
mod version;

use std::cell::RefCell;

pub(crate) use self::{
    context::StupidContext,
    status::{Status, StatusOptions, Statuses},
};

pub(crate) trait Stupid<'repo, 'index> {
    /// Get StupidContext for running stupid commands.
    fn stupid(&'repo self) -> StupidContext<'repo, 'index>;
}

impl<'repo, 'index> Stupid<'repo, 'index> for gix::Repository {
    fn stupid(&'repo self) -> StupidContext<'repo, 'index> {
        StupidContext {
            git_dir: Some(self.git_dir()),
            work_dir: self.work_dir(),
            index_filename: None,
            git_version: RefCell::new(None::<self::version::StupidVersion>),
        }
    }
}
