use crate::{error::Error, stupid};

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
        repo.commit_ex(
            &self.author,
            &self.committer,
            &self.message,
            self.tree_id,
            self.parent_ids,
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

pub(crate) trait CommitExtended {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &str,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error>;
}

impl CommitExtended for git2::Repository {
    fn commit_ex(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &str,
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
    ) -> Result<git2::Oid, Error> {
        let (gpgsign, commit_encoding) = if let Ok(config) = self.config() {
            let gpgsign = config.get_bool("commit.gpgsign").unwrap_or(false);
            let encoding = config.get_string("i18n.commitencoding").ok();
            (gpgsign, encoding)
        } else {
            (false, None)
        };
        if gpgsign || commit_encoding.is_some() {
            // TODO: encode message
            // Use git for any commit that needs to be signed
            stupid::commit_tree(
                self.path(),
                author,
                committer,
                message.as_bytes(),
                tree_id,
                parent_ids,
                gpgsign,
            )
        } else {
            // Use git2 for all other occasions
            let tree = self.find_tree(tree_id)?;
            let mut parents: Vec<git2::Commit<'_>> = Vec::new();
            for parent_id in parent_ids {
                parents.push(self.find_commit(parent_id)?);
            }
            let parents: Vec<&git2::Commit<'_>> = parents.iter().collect();

            Ok(self.commit(None, author, committer, message, &tree, &parents)?)
        }
    }
}
