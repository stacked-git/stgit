// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

pub(crate) struct Branch<'repo> {
    inner: git_repository::Reference<'repo>,
}

impl<'repo> Branch<'repo> {
    pub(crate) fn wrap(reference: git_repository::Reference<'_>) -> Branch<'_> {
        use git_repository::refs::Category;
        assert!(matches!(
            reference.name().category(),
            Some(Category::LocalBranch | Category::RemoteBranch)
        ));
        Branch { inner: reference }
    }

    pub(crate) fn get_reference_name(&self) -> &git_repository::refs::FullNameRef {
        self.inner.name()
    }

    pub(crate) fn get_branch_name(&self) -> Result<&str> {
        let short_name = self.inner.name().shorten();
        short_name.to_str().map_err(|_| {
            anyhow!(
                "Branch name `{}` is not valid UTF-8",
                short_name.to_str_lossy()
            )
        })
    }

    pub(crate) fn get_commit(&self) -> Result<git_repository::Commit<'repo>> {
        Ok(self.inner.id().object()?.try_into_commit()?)
    }

    pub(crate) fn delete(self) -> Result<()> {
        self.inner.delete()?;
        Ok(())
    }
}
