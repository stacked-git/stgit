use crate::error::Error;

use super::repository::commit_ex;

pub(crate) struct CommitData {
    pub author: git2::Signature<'static>,
    pub committer: git2::Signature<'static>,
    pub message: String,
    pub tree_id: git2::Oid,
    pub parent_ids: Vec<git2::Oid>,
}

impl CommitData {
    pub fn new(
        author: git2::Signature<'static>,
        committer: git2::Signature<'static>,
        message: String,
        tree_id: git2::Oid,
        parent_ids: Vec<git2::Oid>,
    ) -> Self {
        Self {
            author,
            committer,
            message,
            tree_id,
            parent_ids,
        }
    }

    pub fn replace_message(self, message: String) -> Self {
        Self { message, ..self }
    }

    pub fn commit(self, repo: &git2::Repository) -> Result<git2::Oid, Error> {
        commit_ex(
            repo,
            &self.author,
            &self.committer,
            &self.message,
            self.tree_id,
            &self.parent_ids,
        )
    }
}

impl From<&git2::Commit<'_>> for CommitData {
    fn from(commit: &git2::Commit<'_>) -> Self {
        Self {
            author: commit.author().to_owned(),
            committer: commit.committer().to_owned(),
            message: commit
                .message_raw()
                .expect("Cannot extract CommitData from non-utf-8 encoded Commit")
                .to_string(),
            tree_id: commit.tree_id(),
            parent_ids: commit.parent_ids().collect(),
        }
    }
}
