use anyhow::{anyhow, Result};

pub(crate) fn get_template(repo: &git2::Repository, name: &str) -> Result<Option<String>> {
    let template_path = repo.path().join(name);
    if let Ok(template_bytes) = std::fs::read(&template_path) {
        let template = std::str::from_utf8(&template_bytes).map_err(|_| {
            anyhow!(
                "template file `{}` contains non-UTF-8 data",
                template_path.to_string_lossy()
            )
        })?;
        Ok(Some(template.into()))
    } else {
        // TODO: Find templates in other places
        Ok(None)
    }
}
