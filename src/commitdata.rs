use git2::{Commit, Tree};

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

    pub fn _parents(&self) -> Vec<&Commit<'repo>> {
        self.parents.iter().collect()
    }
}
