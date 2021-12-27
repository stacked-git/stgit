use crate::error::Error;

pub(crate) trait TemporaryIndex {
    fn with_temp_index<F, T>(&self, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut git2::Index) -> Result<T, Error>;
}

impl TemporaryIndex for git2::Repository {
    fn with_temp_index<F, T>(&self, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut git2::Index) -> Result<T, Error>,
    {
        let mut temp_index = git2::Index::new()?;
        let mut orig_index = self.index()?;
        self.set_index(&mut temp_index)?;
        let result = f(&mut temp_index);
        self.set_index(&mut orig_index)
            .expect("can reset to original index");
        result
    }
}
