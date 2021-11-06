use git2::Repository;

use crate::error::Error;

pub(crate) fn get_template(repo: &Repository, name: &str) -> Result<Option<String>, Error> {
    let template_path = repo.path().join(name);
    if let Ok(template_bytes) = std::fs::read(&template_path) {
        let template = std::str::from_utf8(&template_bytes).map_err(|_| {
            Error::NonUtf8File(template_path.to_string_lossy().to_string())
        })?;
        Ok(Some(template.into()))
    } else {
        // TODO: Find templates in other places
        Ok(None)
    }
}
