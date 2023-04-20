use std::path::{Path, PathBuf};

use anyhow::Result;

pub(crate) struct TempIndex<'repo> {
    git_dir: &'repo Path,
    filename: PathBuf,
}

impl<'repo> TempIndex<'repo> {
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
