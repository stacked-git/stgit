use std::io::Write;

use super::signature::Signature;
use crate::error::Error;

pub(crate) struct Repository(pub git2::Repository);

impl Repository {
    pub fn commit(
        &self,
        author: &Signature,
        committer: &Signature,
        message: &str,
        tree: &git2::Tree<'_>,
        parents: &[&git2::Commit<'_>],
    ) -> Result<git2::Oid, Error> {
        let config = self.0.config().ok();
        if config
            .and_then(|config| config.get_bool("commit.gpgsign").ok())
            .unwrap_or(false)
        {
            // Use git for any commit that needs to be signed
            git_commit_tree(self, author, committer, message, tree, parents)
        } else {
            // Use git2 for all other occasions
            Ok(self
                .0
                .commit(None, author.get(), committer.get(), message, tree, parents)?)
        }
    }

    pub fn get_branch(&self, branch_name: Option<&str>) -> Result<git2::Branch<'_>, Error> {
        if let Some(name) = branch_name {
            let branch = self
                .0
                .find_branch(name, git2::BranchType::Local)
                .map_err(|e| {
                    if e.class() == git2::ErrorClass::Reference {
                        match e.code() {
                            git2::ErrorCode::NotFound => Error::BranchNotFound(name.to_string()),
                            git2::ErrorCode::InvalidSpec => {
                                Error::InvalidBranchName(name.to_string())
                            }
                            git2::ErrorCode::UnbornBranch => {
                                Error::UnbornBranch(format!("`{}`", name))
                            }
                            _ => e.into(),
                        }
                    } else {
                        e.into()
                    }
                })?;
            Ok(branch)
        } else if self.0.head_detached()? {
            Err(Error::HeadDetached)
        } else {
            let head = self.0.head().map_err(|e| {
                if e.code() == git2::ErrorCode::UnbornBranch {
                    Error::UnbornBranch(e.message().to_string())
                } else {
                    e.into()
                }
            })?;
            if head.is_branch() {
                Ok(git2::Branch::wrap(head))
            } else {
                Err(Error::HeadNotBranch(
                    String::from_utf8_lossy(head.name_bytes()).to_string(),
                ))
            }
        }
    }

    #[inline]
    pub fn open_from_env() -> Result<Self, git2::Error> {
        Ok(Self(git2::Repository::open_from_env()?))
    }

    #[inline]
    pub fn index(&self) -> Result<git2::Index, git2::Error> {
        self.0.index()
    }

    #[inline]
    pub fn statuses(
        &self,
        options: Option<&mut git2::StatusOptions>,
    ) -> Result<git2::Statuses<'_>, git2::Error> {
        self.0.statuses(options)
    }

    #[inline]
    pub fn state(&self) -> git2::RepositoryState {
        self.0.state()
    }

    #[inline]
    pub fn head(&self) -> Result<git2::Reference<'_>, git2::Error> {
        self.0.head()
    }

    #[inline]
    pub fn blob(&self, data: &[u8]) -> Result<git2::Oid, git2::Error> {
        self.0.blob(data)
    }

    #[inline]
    pub fn find_commit(&self, oid: git2::Oid) -> Result<git2::Commit<'_>, git2::Error> {
        self.0.find_commit(oid)
    }

    #[inline]
    pub fn find_tree(&self, oid: git2::Oid) -> Result<git2::Tree<'_>, git2::Error> {
        self.0.find_tree(oid)
    }

    #[inline]
    pub fn find_object(
        &self,
        oid: git2::Oid,
        kind: Option<git2::ObjectType>,
    ) -> Result<git2::Object<'_>, git2::Error> {
        self.0.find_object(oid, kind)
    }

    #[inline]
    pub fn find_reference(&self, name: &str) -> Result<git2::Reference<'_>, git2::Error> {
        self.0.find_reference(name)
    }

    #[inline]
    pub fn reference(
        &self,
        name: &str,
        id: git2::Oid,
        force: bool,
        log_message: &str,
    ) -> Result<git2::Reference<'_>, git2::Error> {
        self.0.reference(name, id, force, log_message)
    }

    #[inline]
    pub fn reference_matching(
        &self,
        name: &str,
        id: git2::Oid,
        force: bool,
        current_id: git2::Oid,
        log_message: &str,
    ) -> Result<git2::Reference<'_>, git2::Error> {
        self.0
            .reference_matching(name, id, force, current_id, log_message)
    }

    #[inline]
    pub fn revparse_single(&self, spec: &str) -> Result<git2::Object<'_>, git2::Error> {
        self.0.revparse_single(spec)
    }

    #[inline]
    pub fn treebuilder(
        &self,
        tree: Option<&git2::Tree<'_>>,
    ) -> Result<git2::TreeBuilder<'_>, git2::Error> {
        self.0.treebuilder(tree)
    }

    #[inline]
    pub fn transaction(&self) -> Result<git2::Transaction, git2::Error> {
        self.0.transaction()
    }

    #[inline]
    pub fn checkout_tree(
        &self,
        treeish: &git2::Object<'_>,
        opts: Option<&mut git2::build::CheckoutBuilder<'_>>,
    ) -> Result<(), git2::Error> {
        self.0.checkout_tree(treeish, opts)
    }

    #[inline]
    pub fn diff_tree_to_workdir(
        &self,
        old_tree: Option<&git2::Tree<'_>>,
        opts: Option<&mut git2::DiffOptions>,
    ) -> Result<git2::Diff<'_>, git2::Error> {
        self.0.diff_tree_to_workdir(old_tree, opts)
    }

    #[inline]
    pub fn path(&self) -> &std::path::Path {
        self.0.path()
    }

    #[inline]
    pub fn workdir(&self) -> Option<&std::path::Path> {
        self.0.workdir()
    }
}

#[inline]
fn git_commit_tree(
    repo: &Repository,
    author: &Signature,
    committer: &Signature,
    message: &str,
    tree: &git2::Tree<'_>,
    parents: &[&git2::Commit<'_>],
) -> Result<git2::Oid, Error> {
    let mut command = std::process::Command::new("git");
    command.arg("commit-tree").arg(format!("{}", tree.id()));
    for parent in parents {
        command.arg("-p").arg(format!("{}", parent.id()));
    }
    command
        .arg("-S") // Sign commit
        .env("GIT_DIR", repo.path())
        .env("GIT_AUTHOR_NAME", &author.name())
        .env("GIT_AUTHOR_EMAIL", &author.email())
        .env("GIT_AUTHOR_DATE", author.get_epoch_time_string())
        .env("GIT_COMMITTER_NAME", &committer.name())
        .env("GIT_COMMITTER_EMAIL", &committer.email())
        .env("GIT_COMMITTER_DATE", committer.get_epoch_time_string())
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());

    let mut child = command.spawn()?;
    let mut stdin = child
        .stdin
        .take()
        .expect("failed to open stdin of `git commit-tree`");
    let message = message.to_string();
    std::thread::spawn(move || {
        stdin
            .write_all(message.as_bytes())
            .expect("failed to write stdin of `git commit-tree`");
    });
    let output = child.wait_with_output()?;
    if output.status.success() {
        let output_str =
            std::str::from_utf8(&output.stdout).expect("`git commit-tree` output non-UTF8 oid");
        Ok(git2::Oid::from_str(output_str.trim_end())?)
    } else {
        Err(Error::Generic(format!(
            "`git commit-tree`: {} ({})",
            String::from_utf8_lossy(&output.stderr).trim_end(),
            output.status.code().unwrap_or(-1)
        )))
    }
}
