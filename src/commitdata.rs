use git2::{Commit, Oid, Tree};

use crate::error::Error;
use crate::signature::CheckedSignature;

pub(crate) struct CommitData<'repo> {
    pub author: CheckedSignature,
    pub committer: CheckedSignature,
    pub message: String,
    pub tree: Tree<'repo>,
    pub parents: Vec<Commit<'repo>>,
}

impl<'repo> CommitData<'repo> {
    pub fn new(
        author: CheckedSignature,
        committer: CheckedSignature,
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

    pub fn parents(&self) -> Vec<&Commit<'repo>> {
        self.parents.iter().collect()
    }

    pub fn commit(self, repo: &git2::Repository) -> Result<Oid, Error> {
        let update_ref = None;
        Ok(repo.commit(
            update_ref,
            &self.author.get_signature()?,
            &self.committer.get_signature()?,
            &self.message,
            &self.tree,
            &self.parents(),
        )?)
    }
}
