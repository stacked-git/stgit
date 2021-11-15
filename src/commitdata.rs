use git2::{Commit, Oid, Tree};

use crate::error::Error;
use crate::wrap::{Repository, Signature};

pub(crate) struct CommitData<'repo> {
    pub author: Signature,
    pub committer: Signature,
    pub message: String,
    pub tree: Tree<'repo>,
    pub parents: Vec<Commit<'repo>>,
}

impl<'repo> CommitData<'repo> {
    pub fn new(
        author: Signature,
        committer: Signature,
        message: String,
        tree: Tree<'repo>,
        parents: Vec<Commit<'repo>>,
    ) -> Self {
        Self {
            author,
            committer,
            message,
            tree,
            parents,
        }
    }

    pub fn replace_message(self, message: String) -> Self {
        Self::new(
            self.author,
            self.committer,
            message,
            self.tree,
            self.parents,
        )
    }

    pub fn commit(self, repo: &Repository) -> Result<Oid, Error> {
        let parents: Vec<&Commit<'_>> = self.parents.iter().collect();
        repo.commit(
            &self.author,
            &self.committer,
            &self.message,
            &self.tree,
            &parents,
        )
    }
}
