use crate::error::Error;

use super::repository::commit_ex;

pub(crate) struct CommitData<'repo> {
    pub author: git2::Signature<'static>,
    pub committer: git2::Signature<'static>,
    pub message: String,
    pub tree: git2::Tree<'repo>,
    pub parents: Vec<git2::Commit<'repo>>,
}

impl<'repo> CommitData<'repo> {
    pub fn new(
        author: git2::Signature<'static>,
        committer: git2::Signature<'static>,
        message: String,
        tree: git2::Tree<'repo>,
        parents: Vec<git2::Commit<'repo>>,
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

    pub fn commit(self, repo: &git2::Repository) -> Result<git2::Oid, Error> {
        let parents: Vec<&git2::Commit<'_>> = self.parents.iter().collect();
        commit_ex(
            repo,
            &self.author,
            &self.committer,
            &self.message,
            &self.tree,
            &parents,
        )
    }
}

// impl<'repo> From<git2::Commit<'repo>> for CommitData<'repo> {
//     fn from(commit: git2::Commit<'repo>) -> Self {
//         Self {
//             author: commit.author().to_owned(),
//             committer: commit.committer().to_owned(),
//             message: commit.message_raw_bytes().collect(),
//             tree: commit.tree(),
//             parents: commit.parents().collect(),
//         }
//     }
// }
