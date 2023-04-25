// SPDX-License-Identifier: GPL-2.0-only

//! Context for creating and auto-deleting a temporary index file.
//!
//! The temporary index file is created relative to `git_dir`.
//!
//! The file name is based on the PID of the StGit process to mitigate the most
//! egregious name collision scenarios.
//!
//! When [`TempIndex`] instance is dropped, it will attempt to delete the temporary
//! index file. It is okay if the file no longer exists at drop-time, but a panic will
//! happen if the file exists but removal fails.

use std::path::{Path, PathBuf};

use anyhow::Result;

pub(crate) struct TempIndex<'repo> {
    git_dir: &'repo Path,
    filename: PathBuf,
}

impl<'repo> TempIndex<'repo> {
    /// Create new temporary index file relative to `git_dir`.
    ///
    /// The temporary index file will be auto-deleted when this value is dropped.
    pub(crate) fn new(git_dir: &'repo Path) -> Result<Self> {
        let pid = std::process::id();
        let filename = PathBuf::from(format!("index-temp-stg-{pid}"));
        let index_path = git_dir.join(&filename);
        std::fs::OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(index_path)?;

        Ok(Self { git_dir, filename })
    }

    /// Get a reference to the temporary index file name.
    ///
    /// This is not a complete path to the file. The file exists relative to the git
    /// dir.
    pub(crate) fn filename(&self) -> &Path {
        self.filename.as_ref()
    }
}

impl<'repo> Drop for TempIndex<'repo> {
    fn drop(&mut self) {
        let index_path = self.git_dir.join(self.filename());
        assert!(index_path.is_file());
        if let Err(e) = std::fs::remove_file(&index_path) {
            panic!("failed to remove temp index {index_path:?}: {e}");
        }
    }
}
