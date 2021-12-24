use crate::error::Error;

pub(crate) fn with_temp_index<F, T>(repo: &git2::Repository, f: F) -> Result<T, Error>
where
    F: FnOnce(&mut git2::Index) -> Result<T, Error>,
{
    let mut temp_index = git2::Index::new()?;
    let mut orig_index = repo.index()?;
    repo.set_index(&mut temp_index)?;
    let result = f(&mut temp_index);
    repo.set_index(&mut orig_index)
        .expect("can reset to original index");
    result
}
