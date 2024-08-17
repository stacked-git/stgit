// SPDX-License-Identifier: GPL-2.0-only

use std::str::FromStr;

use anyhow::{anyhow, Result};
use gix::refs::Category;

use crate::{
    ext::RepositoryExtended,
    stupid::Stupid,
    wrap::{Branch, PartialRefName},
};

/// Locator for an existing branch.
///
/// Branches may be identified by name, or by using the `@{-<n>}` syntax, which is also
/// used by git. N.B. `-` is also supported as a shorthand for `@{-1}`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum BranchLocator {
    /// Locate a branch by name.
    Name(PartialRefName),
    /// Locate N'th previously checked-out HEAD.
    PrevCheckout(usize),
}

impl std::fmt::Display for BranchLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BranchLocator::Name(name) => name.fmt(f),
            BranchLocator::PrevCheckout(n) => write!(f, "@{{-{n}}}"),
        }
    }
}

impl FromStr for BranchLocator {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use winnow::Parser;
        crate::patch::parse::branch_locator
            .parse(s)
            .map_err(|_| anyhow::anyhow!("invalid branch locator `{s}`"))
    }
}

impl BranchLocator {
    pub(crate) fn resolve<'repo>(&self, repo: &'repo gix::Repository) -> Result<Branch<'repo>> {
        match self {
            BranchLocator::Name(name) => repo.get_branch(name),
            BranchLocator::PrevCheckout(_) => {
                let stupid = repo.stupid();
                let spec = self.to_string();
                if let Some(ref_name) = stupid.rev_parse_symbolic_full_name(&spec)? {
                    let fullname = gix::refs::FullName::try_from(ref_name)?;
                    assert!(matches!(fullname.category(), Some(Category::LocalBranch)));
                    let reference = repo.find_reference(&fullname)?;
                    Ok(Branch::wrap(reference))
                } else {
                    Err(anyhow!("`{self}` does not resolve to a local branch"))
                }
            }
        }
    }
}
