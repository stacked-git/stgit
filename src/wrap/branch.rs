// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

use crate::wrap::PartialRefName;

#[derive(Clone, Debug)]
pub(crate) struct Branch<'repo> {
    inner: gix::Reference<'repo>,
}

impl<'repo> Branch<'repo> {
    pub(crate) fn wrap(reference: gix::Reference<'_>) -> Branch<'_> {
        use gix::refs::Category;
        assert!(matches!(
            reference.name().category(),
            Some(Category::LocalBranch | Category::RemoteBranch)
        ));
        Branch { inner: reference }
    }

    pub(crate) fn get_reference_name(&self) -> &gix::refs::FullNameRef {
        self.inner.name()
    }

    pub(crate) fn get_branch_partial_name(&self) -> Result<PartialRefName> {
        Ok(PartialRefName::from_str(self.get_branch_name()?).expect("valid short branch name"))
    }

    pub(crate) fn get_branch_name(&self) -> Result<&str> {
        let short_name = self.inner.name().shorten();
        short_name.to_str().map_err(|_| {
            anyhow!(
                "branch name `{}` is not valid UTF-8",
                short_name.to_str_lossy()
            )
        })
    }

    pub(crate) fn get_commit(&self) -> Result<gix::Commit<'repo>> {
        Ok(self.inner.id().object()?.try_into_commit()?)
    }

    pub(crate) fn delete(self) -> Result<()> {
        self.inner.delete()?;
        Ok(())
    }
}
