// SPDX-License-Identifier: GPL-2.0-only

//! Execute commands with git, the stupid content tracker.
//!
//! Each function in this module calls-out to a specific git command that is useful to
//! StGit. This module exists to overcome limitations of libgit2, including:
//!
//! - libgit2 behavior is not automatically affected by git configuration. For
//!   operations where StGit wants to be use/respect git configuration, using git
//!   directly is a fast-path to correct behavior.
//! - libgit2 [`does not always apply patches correctly`]. This is essential behavior
//!   for StGit (but perhaps not so much for other libgit2 applications?).
//! - libgit2 is missing some behaviors that StGit requires. For example, StGit needs to
//!   support the full breadth of commit signing. Running `git commit-tree -s` buys all
//!   this behavior without having to cobble it together with yet more rust dependencies.
//!
//! [`does not always apply patches correctly`]: https://github.com/libgit2/libgit2/issues/5717

use std::{
    ffi::{OsStr, OsString},
    io::Write,
    path::Path,
    process::{Child, Command, ExitStatus, Output, Stdio},
};

use anyhow::{anyhow, Context, Result};
use bstr::{BString, ByteSlice, ByteVec};

use crate::signature::TimeExtended;

const GIT_EXEC_FAIL: &str = "could not execute `git`";

trait StupidCommand {
    /// Spawn command with git error context.
    ///
    /// By default, stdin and stdout are inherited and stderr is piped.
    fn spawn_git(&mut self) -> Result<Child>;

    /// Run git command, wait for completion, and collect output streams.
    ///
    /// By default, stdout and stderr are piped and stdin is null.
    fn output_git(&mut self) -> Result<Output>;

    /// Write input to child process and gather its output.
    ///
    /// The input data is written from a separate thread to avoid potential
    /// deadlock that can occur if the child process's input buffer is filled
    /// without concurrently reading from the child's stdout and stderr.
    ///
    /// By default, stdout is inherited. Stdin and stderr are piped.
    fn in_and_out(&mut self, input: &[u8]) -> Result<Output>;
}

impl StupidCommand for Command {
    fn spawn_git(&mut self) -> Result<Child> {
        self.stderr(Stdio::piped()).spawn().context(GIT_EXEC_FAIL)
    }

    fn output_git(&mut self) -> Result<Output> {
        self.output().context(GIT_EXEC_FAIL)
    }

    fn in_and_out(&mut self, input: &[u8]) -> Result<Output> {
        struct SendSlice(*const u8, usize);
        impl SendSlice {
            fn from(slice: &[u8]) -> Self {
                Self(slice.as_ptr(), slice.len())
            }

            unsafe fn take<'a>(self) -> &'a [u8] {
                let SendSlice(ptr, len) = self;
                std::slice::from_raw_parts(ptr, len)
            }
        }
        unsafe impl Send for SendSlice {}
        unsafe impl Sync for SendSlice {}

        let mut child = self.stdin(Stdio::piped()).spawn_git()?;

        let send_input = SendSlice::from(input);
        let mut stdin = child.stdin.take().unwrap();
        let handle = std::thread::spawn(move || {
            // Safety: the input slice will not outlive the thread because
            // the thread is joined before this function returns.
            let input = unsafe { send_input.take() };
            stdin.write_all(input).unwrap();
        });
        let output_result = child.wait_with_output();
        handle.join().unwrap();
        Ok(output_result?)
    }
}

trait StupidOutput {
    /// Ensure that Child or Output is successful, returning Output.
    fn require_success(self, command: &str) -> Result<Output>;
}

impl StupidOutput for Child {
    fn require_success(self, command: &str) -> Result<Output> {
        let output = self.wait_with_output()?;
        output.require_success(command)
    }
}

impl StupidOutput for Output {
    fn require_success(self, command: &str) -> Result<Output> {
        if self.status.success() {
            Ok(self)
        } else {
            Err(git_command_error(command, &self.stderr))
        }
    }
}

/// Context for running stupid commands.
#[derive(Clone, Debug, Default)]
pub(crate) struct StupidContext<'repo, 'index> {
    pub git_dir: Option<&'repo Path>,
    pub index_path: Option<&'index Path>,
    pub work_dir: Option<&'repo Path>,
}

pub(crate) trait Stupid<'repo, 'index> {
    /// Get StupidContext for running stupid commands.
    fn stupid(&'repo self) -> StupidContext<'repo, 'index>;
}

impl<'repo, 'index> Stupid<'repo, 'index> for git2::Repository {
    fn stupid(&'repo self) -> StupidContext<'repo, 'index> {
        StupidContext {
            git_dir: Some(self.path()),
            work_dir: self.workdir(),
            index_path: None,
        }
    }
}

impl<'repo, 'index> StupidContext<'repo, 'index> {
    pub(crate) fn with_index_path(&self, index_path: &'index Path) -> StupidContext {
        StupidContext {
            git_dir: self.git_dir,
            work_dir: self.work_dir,
            index_path: Some(index_path),
        }
    }

    fn git(&self) -> Command {
        let mut command = Command::new("git");
        self.git_dir.map(|p| command.env("GIT_DIR", p));
        self.work_dir.map(|p| command.env("GIT_WORK_TREE", p));
        self.index_path.map(|p| command.env("GIT_INDEX_FILE", p));
        command
    }

    fn git_in_work_root(&self) -> Command {
        let mut command = self.git();
        command.current_dir(
            self.work_dir
                .expect("work_dir is required for this command"),
        );
        command
    }

    /// Apply a patch (diff) to the specified index using `git apply --cached`.
    pub(crate) fn apply_to_index(&self, diff: &[u8]) -> Result<()> {
        self.git_in_work_root()
            .args(["apply", "--cached"]) // TODO: use --recount?
            .stdout(Stdio::null())
            .in_and_out(diff)?
            .require_success("apply")?;
        Ok(())
    }

    pub(crate) fn apply_to_worktree_and_index(
        &self,
        diff: &[u8],
        reject: bool,
        strip_level: Option<usize>,
        context_lines: Option<usize>,
    ) -> Result<()> {
        let mut command = self.git_in_work_root();
        command.args(["apply", "--index", "--allow-empty"]);
        if reject {
            command.arg("--reject");
        }
        if let Some(strip_level) = strip_level {
            command.arg(format!("-p{strip_level}"));
        }
        if let Some(context_lines) = context_lines {
            command.arg(format!("-C{context_lines}"));
        }
        command
            .stdout(Stdio::null())
            .in_and_out(diff)?
            .require_success("apply --index")?;
        Ok(())
    }

    /// Apply diff between two trees to specified index.
    ///
    /// Pipes `git diff-tree | git apply --cached`.
    ///
    /// Returns `true` if the patch application is successful, `false` otherwise.
    pub(crate) fn apply_treediff_to_index(
        &self,
        tree1: git2::Oid,
        tree2: git2::Oid,
    ) -> Result<bool> {
        let mut diff_tree_child = self
            .git()
            .args(["diff-tree", "--full-index", "--binary", "--patch"])
            .arg(tree1.to_string())
            .arg(tree2.to_string())
            .arg("--")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .spawn_git()?;

        let apply_output = self
            .git_in_work_root()
            .args(["apply", "--cached", "--allow-empty"]) // --3way
            .stdin(diff_tree_child.stdout.take().unwrap())
            .stdout(Stdio::null())
            .output_git()?;

        diff_tree_child.require_success("diff-tree")?;
        Ok(apply_output.status.success())
    }

    /// Apply diff between two trees to worktree and index.
    ///
    /// Pipes `git diff-tree | git apply --index`.
    ///
    /// Returns `true` if the patch application is successful, `false` otherwise.
    pub(crate) fn apply_treediff_to_worktree_and_index<I, S>(
        &self,
        tree1: git2::Oid,
        tree2: git2::Oid,
        pathspecs: Option<I>,
    ) -> Result<bool>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut diff_tree_command = self.git();
        diff_tree_command
            .args(["diff-tree", "--full-index", "--binary", "--patch"])
            .arg(tree1.to_string())
            .arg(tree2.to_string())
            .arg("--");
        if let Some(pathspecs) = pathspecs {
            diff_tree_command.args(pathspecs);
        }

        let mut diff_tree_child = diff_tree_command
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .spawn_git()?;

        let apply_output = self
            .git_in_work_root()
            .args(["apply", "--index", "--allow-empty", "--3way"])
            .stdin(diff_tree_child.stdout.take().unwrap())
            .stdout(Stdio::null())
            .output_git()?;

        diff_tree_child.require_success("diff-tree")?;
        if apply_output.status.success() {
            Ok(true)
        } else if apply_output.status.code() == Some(1) {
            Ok(false)
        } else {
            Err(git_command_error("apply", &apply_output.stderr))
        }
    }

    /// Copy branch
    ///
    /// Copies branch ref, reflog, and 'branch.<name>' config sections.
    pub(crate) fn branch_copy(
        &self,
        old_branchname: Option<&str>,
        new_branchname: &str,
    ) -> Result<()> {
        let mut command = self.git();
        command.args(["branch", "--copy"]);
        if let Some(old_branchname) = old_branchname {
            command.arg(old_branchname);
        }
        command.arg(new_branchname);
        command
            .stdout(Stdio::null())
            .output_git()?
            .require_success("branch --copy")?;
        Ok(())
    }

    /// Move branch
    ///
    /// Moves branch ref, moves reflog, updates HEAD, and
    /// renames 'branch.<name>' config sections.
    pub(crate) fn branch_move(
        &self,
        old_branchname: Option<&str>,
        new_branchname: &str,
    ) -> Result<()> {
        let mut command = self.git();
        command.args(["branch", "--move"]);
        if let Some(old_branchname) = old_branchname {
            command.arg(old_branchname);
        }
        command.arg(new_branchname);
        command
            .stdout(Stdio::null())
            .output_git()?
            .require_success("branch --move")?;
        Ok(())
    }

    /// Checkout a branch.
    pub(crate) fn _checkout(&self, branch_name: &str) -> Result<()> {
        self.git()
            .arg("checkout")
            .arg(branch_name)
            .arg("--")
            .stdout(Stdio::null())
            .output_git()?
            .require_success("checkout")?;
        Ok(())
    }

    /// Create a commit for the specified tree id using `git commit-tree`.
    ///
    /// The newly created commit id is returned.
    pub(crate) fn commit_tree(
        &self,
        author: &git2::Signature,
        committer: &git2::Signature,
        message: &[u8],
        tree_id: git2::Oid,
        parent_ids: impl IntoIterator<Item = git2::Oid>,
        gpgsign: bool,
    ) -> Result<git2::Oid> {
        let mut command = self.git();
        command.arg("commit-tree").arg(tree_id.to_string());
        for parent_id in parent_ids {
            command.arg("-p").arg(parent_id.to_string());
        }
        if gpgsign {
            command.arg("-S");
        }
        let author_name = author
            .name_bytes()
            .to_os_str()
            .context("setting GIT_AUTHOR_NAME from author name")?;
        let author_email = author
            .email_bytes()
            .to_os_str()
            .context("setting GIT_AUTHOR_EMAIL from author email")?;
        let committer_name = committer
            .name_bytes()
            .to_os_str()
            .context("setting GIT_COMMITTER_NAME from author name")?;
        let committer_email = committer
            .email_bytes()
            .to_os_str()
            .context("setting GIT_COMMITTER_EMAIL from author email")?;

        let output = command
            .env("GIT_AUTHOR_NAME", author_name)
            .env("GIT_AUTHOR_EMAIL", author_email)
            .env("GIT_COMMITTER_NAME", committer_name)
            .env("GIT_COMMITTER_EMAIL", committer_email)
            // TODO: reencode dates?
            .env("GIT_AUTHOR_DATE", author.epoch_time_string())
            .env("GIT_COMMITTER_DATE", committer.epoch_time_string())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .in_and_out(message)?
            .require_success("commit-tree")?;

        parse_oid(&output.stdout)
    }

    /// Interactive diff
    pub(crate) fn diff<I, S>(
        &self,
        revspec: &str,
        pathspecs: Option<I>,
        stat: bool,
        color_opt: Option<&str>,
        diff_opts: Option<&str>,
    ) -> Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git();
        command.arg("diff");
        if stat {
            command.args(["--stat", "--summary"]);
        }

        if let Some(color_opt) = map_color_opt(color_opt) {
            command.arg(format!("--color={color_opt}"));
        }

        if let Some(diff_opts) = diff_opts {
            for opt in diff_opts.split_ascii_whitespace() {
                command.arg(opt);
            }
        }

        command.arg(revspec);
        command.arg("--");

        if let Some(pathspecs) = pathspecs {
            command.args(pathspecs);
        }

        command
            .stdout(Stdio::inherit())
            .output_git()?
            .require_success("diff")?;
        Ok(())
    }

    pub(crate) fn diffstat(&self, diff: &[u8]) -> Result<Vec<u8>> {
        let output = self
            .git()
            .args(["apply", "--stat", "--summary", "--allow-empty"])
            .stdout(Stdio::piped())
            .in_and_out(diff)?
            .require_success("apply --stat --summary")?;
        Ok(output.stdout)
    }

    /// Generate diff between specified tree and the working tree or index with `git diff-index`.
    pub(crate) fn diff_index(&self, tree_id: git2::Oid) -> Result<Vec<u8>> {
        let output = self
            .git()
            .args(["diff-index", "-p", "--full-index"])
            .arg(tree_id.to_string())
            .output_git()?
            .require_success("diff-index")?;
        Ok(output.stdout)
    }

    /// Get file names that differ between tree and index.
    pub(crate) fn diff_index_names(
        &self,
        tree_id: git2::Oid,
        relative: Option<&Path>,
    ) -> Result<Vec<u8>> {
        let mut command = self.git();
        command.args(["diff-index", "--name-only", "-z"]);

        if let Some(relative) = relative {
            let mut arg = BString::from("--relative=");
            let relative = <[u8]>::from_os_str(relative.as_os_str())
                .expect("relative path is valid UTF-8 on Windows");
            arg.push_str(relative);
            let arg = arg
                .to_os_str()
                .context("building --relative arg for `git diff-index`")?;
            command.arg(arg);
        }

        let output = command
            .arg(tree_id.to_string())
            .arg("--")
            .output_git()?
            .require_success("diff-index --name-only")?;
        Ok(output.stdout)
    }

    /// Interative diff-tree (for 'stg files').
    pub(crate) fn diff_tree_files(
        &self,
        tree1: git2::Oid,
        tree2: git2::Oid,
        stat: bool,
        name_only: bool,
        color_opt: Option<&str>,
    ) -> Result<Vec<u8>> {
        let mut command = self.git();
        command.args(["diff-tree", "-r"]);
        if stat {
            command.args(["--stat", "--summary"]);
        } else if name_only {
            command.arg("--name-only");
        } else {
            command.arg("--name-status");
        }
        if let Some(color_opt) = map_color_opt(color_opt) {
            command.arg(format!("--color={color_opt}"));
        }
        command.args([tree1.to_string(), tree2.to_string()]);
        let output = command.output_git()?.require_success("diff-tree")?;
        Ok(output.stdout)
    }

    /// Generate diff between two trees using `git diff-tree -p`.
    pub(crate) fn diff_tree_patch<I, S>(
        &self,
        tree1: git2::Oid,
        tree2: git2::Oid,
        pathspecs: Option<I>,
        full_index: bool,
        color: bool,
        diff_opts: Option<&str>,
    ) -> Result<Vec<u8>>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git();
        command.args(["diff-tree", "-p"]);
        if full_index {
            command.arg("--full-index");
        }
        if color {
            command.arg("--color");
        }
        if let Some(diff_opts) = diff_opts {
            command.args(diff_opts.split_ascii_whitespace());
        }
        command.args([tree1.to_string(), tree2.to_string()]);
        if let Some(pathspecs) = pathspecs {
            command.arg("--");
            command.args(pathspecs);
        }
        let output = command.output_git()?.require_success("diff-tree")?;
        Ok(output.stdout)
    }

    /// Get unmerged path list using `git diff --name-only --diff-filter=U`.
    ///
    /// The returned unmerged paths are relative to the work tree root regardless of the
    /// current working dir.
    pub(crate) fn diff_unmerged_names(&self) -> Result<Vec<OsString>> {
        let output = self
            .git()
            .args(["diff", "--name-only", "--diff-filter=U", "-z"])
            .output_git()?
            .require_success("diff --name-only")?;
        let mut paths: Vec<OsString> = Vec::new();
        for path_bytes in output.stdout.split_str(b"\0") {
            if !path_bytes.is_empty() {
                let path = path_bytes.to_os_str().context("getting unmerged path")?;
                paths.push(path.into());
            }
        }
        Ok(paths)
    }

    /// Show log in gitk
    pub(crate) fn gitk<I, S>(&self, commit_id: git2::Oid, pathspecs: Option<I>) -> Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = Command::new("gitk");
        self.git_dir.map(|p| command.env("GIT_DIR", p));
        self.work_dir.map(|p| command.env("GIT_WORK_TREE", p));
        self.index_path.map(|p| command.env("GIT_INDEX_FILE", p));
        command.arg(commit_id.to_string());
        if let Some(pathspecs) = pathspecs {
            command.arg("--");
            command.args(pathspecs);
        }

        let output = command
            .stdout(Stdio::inherit())
            .output()
            .context("could not execute `gitk`")?;
        if output.status.success() {
            Ok(())
        } else {
            let err_str = output.stderr.to_str_lossy();
            let err_str = err_str.trim_end();
            Err(anyhow!(err_str.to_string()).context("`gitk`"))
        }
    }

    /// Add trailers to commit message with `git interpret-trailers`.
    pub(crate) fn interpret_trailers<'a>(
        &self,
        message: &[u8],
        trailers: impl IntoIterator<Item = (&'a str, &'a str)>,
    ) -> Result<Vec<u8>> {
        let output = self
            .git()
            .arg("interpret-trailers")
            .args(
                trailers
                    .into_iter()
                    .map(|(trailer, by)| format!("--trailer={trailer}={by}")),
            )
            .stdout(Stdio::piped())
            .in_and_out(message)?
            .require_success("interpret-trailers")?;
        Ok(output.stdout)
    }

    /// Interactively show log
    pub(crate) fn log<I, S>(
        &self,
        commit_id: git2::Oid,
        pathspecs: Option<I>,
        num_commits: Option<usize>,
        color_opt: Option<&str>,
        full_index: bool,
        show_diff: bool,
    ) -> Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git_in_work_root();
        command.arg("log");
        if let Some(n) = num_commits {
            command.arg(format!("-{n}"));
        }
        if let Some(color_opt) = map_color_opt(color_opt) {
            command.arg(format!("--color={color_opt}"));
        }
        if show_diff {
            command.arg("-p");
        } else if !full_index {
            command.arg("--pretty=tformat:%C(auto)%h   %C(auto,blue)%aD   %C(auto)%s");
        }
        command.arg(commit_id.to_string());
        if let Some(pathspecs) = pathspecs {
            command.arg("--");
            command.args(pathspecs);
        }
        let output = command.stdout(Stdio::inherit()).output_git()?;
        if is_status_signal(&output.status, 13) {
            // `git log` process was killed by SIGPIPE, probably due to pager exiting before
            // all log output could be written. This is normal, but `git log` does not print an
            // error message to stderr, so we inject our own error string.
            return Err(git_command_error("log", b"broken pipe"));
        }
        output.require_success("log")?;
        Ok(())
    }

    pub(crate) fn mailinfo(
        &self,
        input: Option<std::fs::File>,
        copy_message_id: bool,
    ) -> Result<(Vec<u8>, Vec<u8>, Vec<u8>)> {
        let mut command = self.git();
        command.args(["mailinfo", "--scissors", "--encoding=UTF-8"]);
        if copy_message_id {
            command.arg("--message-id");
        }
        let dir = tempfile::tempdir()?;
        let message_path = dir.path().join("message.txt");
        let patch_path = dir.path().join("patch.diff");
        command.args([message_path.as_path(), patch_path.as_path()]);
        if let Some(input) = input {
            command.stdin(input);
        } else {
            command.stdin(Stdio::inherit());
        }
        let output = command
            .stdout(Stdio::piped())
            .output_git()?
            .require_success("mailinfo")?;
        let mailinfo = output.stdout;
        let message = std::fs::read(message_path)?;
        let diff = std::fs::read(patch_path)?;
        Ok((mailinfo, message, diff))
    }

    #[cfg(feature = "import-compressed")]
    pub(crate) fn mailinfo_stream(
        &self,
        mut input: impl std::io::Read + Send + 'static,
        copy_message_id: bool,
    ) -> Result<(Vec<u8>, Vec<u8>, Vec<u8>)> {
        let mut command = self.git();
        command.args(["mailinfo", "--scissors", "--encoding=UTF-8"]);
        if copy_message_id {
            command.arg("--message-id");
        }
        let dir = tempfile::tempdir()?;
        let message_path = dir.path().join("message.txt");
        let patch_path = dir.path().join("patch.diff");
        command.args([message_path.as_path(), patch_path.as_path()]);
        let mut child = command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn_git()?;

        let mut stdin = child.stdin.take().unwrap();

        std::thread::spawn(move || -> Result<()> {
            let mut buf = [0; 8192];
            loop {
                let n = input.read(&mut buf[..])?;
                if n > 0 {
                    stdin.write_all(&buf[..n])?;
                } else {
                    break Ok(());
                }
            }
        });

        let output = child.require_success("mailinfo")?;
        let headers = output.stdout;
        let message = std::fs::read(message_path)?;
        let diff = std::fs::read(patch_path)?;
        Ok((headers, message, diff))
    }

    pub(crate) fn mailsplit(
        &self,
        source_path: Option<&Path>,
        out_dir: &Path,
        keep_cr: bool,
        missing_from_ok: bool,
    ) -> Result<usize> {
        let mut command = self.git();
        command.arg("mailsplit");
        if keep_cr {
            command.arg("--keep-cr");
        }
        if missing_from_ok {
            command.arg("-b");
        }
        let mut out_opt = OsString::from("-o");
        out_opt.push(out_dir.as_os_str());
        command.arg(out_opt);
        if let Some(source_path) = source_path {
            command.arg("--");
            command.arg(source_path);
        } else {
            command.stdin(Stdio::inherit());
        }
        let output = command.output_git()?.require_success("mailsplit")?;
        let s = output.stdout.to_str().context("parsing mailsplit output")?;
        let num_patches = s.trim_end().parse::<usize>()?;
        Ok(num_patches)
    }

    /// Perform three-way merge with `git merge-recursive`.
    ///
    /// Returns `true` if the merge was successful, `false` otherwise.
    pub(crate) fn merge_recursive(
        &self,
        base_tree_id: git2::Oid,
        our_tree_id: git2::Oid,
        their_tree_id: git2::Oid,
    ) -> Result<bool> {
        let output = self
            .git()
            .arg("merge-recursive")
            .arg(base_tree_id.to_string())
            .arg("--")
            .arg(our_tree_id.to_string())
            .arg(their_tree_id.to_string())
            .env(format!("GITHEAD_{base_tree_id}"), "ancestor")
            .env(format!("GITHEAD_{our_tree_id}"), "current")
            .env(format!("GITHEAD_{their_tree_id}"), "patched")
            .output_git()?;

        if output.status.success() {
            Ok(true)
        } else if output.status.code() == Some(1) {
            Ok(false)
        } else {
            Err(git_command_error("merge-recursive", &output.stderr))
        }
    }

    /// Perfom three-way merge, with optional auto-resolution of conflicts with `git merge-tool`.
    pub(crate) fn merge_recursive_or_mergetool(
        &self,
        base_tree_id: git2::Oid,
        our_tree_id: git2::Oid,
        their_tree_id: git2::Oid,
        use_mergetool: bool,
    ) -> Result<bool> {
        if self.merge_recursive(base_tree_id, our_tree_id, their_tree_id)? {
            Ok(true)
        } else if use_mergetool {
            self.mergetool()
        } else {
            Ok(false)
        }
    }

    /// Attempt to resolve outstanding merge conflicts with `git merge-tool`.
    pub(crate) fn mergetool(&self) -> Result<bool> {
        let output = self.git().arg("merge-tool").output_git()?;
        if output.status.success() {
            Ok(true)
        } else if output.status.code() == Some(1) {
            Ok(false)
        } else {
            Err(git_command_error("mergetool", &output.stderr))
        }
    }

    /// Copy notes from one object to another using `git notes copy`.
    pub(crate) fn notes_copy(&self, from_oid: git2::Oid, to_oid: git2::Oid) -> Result<()> {
        self.git()
            .args(["notes", "copy"])
            .arg(from_oid.to_string())
            .arg(to_oid.to_string())
            .stdout(Stdio::null())
            .output_git()?
            .require_success("notes copy")?;
        Ok(())
    }

    /// Read content of a tree into specified index using `git read-tree`.
    pub(crate) fn read_tree(&self, tree_id: git2::Oid) -> Result<()> {
        self.git()
            .arg("read-tree")
            .arg(tree_id.to_string())
            .stdout(Stdio::null())
            .output_git()?
            .require_success("read-tree")?;
        Ok(())
    }

    /// Checkout tree to working tree using `git read-tree`.
    pub(crate) fn read_tree_checkout(
        &self,
        old_tree_id: git2::Oid,
        new_tree_id: git2::Oid,
    ) -> Result<()> {
        self.git()
            .args([
                "read-tree",
                "-m",
                "-u",
                "--exclude-per-directory=.gitignore",
            ])
            .arg(old_tree_id.to_string())
            .arg(new_tree_id.to_string())
            .stdout(Stdio::null())
            .output_git()?
            .require_success("read-tree -m -u")?;
        Ok(())
    }

    /// Hard checkout tree to working tree using `git read-tree`.
    pub(crate) fn read_tree_checkout_hard(&self, tree_id: git2::Oid) -> Result<()> {
        self.git()
            .args(["read-tree", "--reset", "-u"])
            .arg(tree_id.to_string())
            .stdout(Stdio::null())
            .output_git()?
            .require_success("read-tree --reset -u")?;
        Ok(())
    }

    /// Pack unpacked objects
    pub(crate) fn repack(&self) -> Result<()> {
        self.git()
            .args(["repack", "-a", "-d", "-f"])
            .stdout(Stdio::null())
            .output_git()?
            .require_success("repack -a -d -f")?;
        Ok(())
    }

    /// Get list of revisions using `git rev-list`.
    pub(crate) fn rev_list<I, S>(
        &self,
        base: git2::Oid,
        top: git2::Oid,
        pathspecs: Option<I>,
    ) -> Result<Vec<git2::Oid>>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git();
        command.arg("rev-list").arg(format!("{base}..{top}"));

        command.arg("--");
        if let Some(pathspecs) = pathspecs {
            command.args(pathspecs);
        }

        let output = command.output_git()?.require_success("rev-list")?;
        let mut oids: Vec<git2::Oid> = Vec::new();
        for line in output
            .stdout
            .split_str("\n")
            .filter(|line| !line.is_empty())
        {
            oids.push(parse_oid(line)?);
        }
        Ok(oids)
    }

    /// Get cdup for current directory from `git rev-parse --show-cdup`.
    pub(crate) fn rev_parse_cdup(&self) -> Result<OsString> {
        let output = self
            .git()
            .args(["rev-parse", "--show-cdup"])
            .output_git()?
            .require_success("rev-parse --show-cdup")?;
        let mut stdout = output.stdout;
        let last = stdout.pop();
        assert_eq!(last, Some(b'\n'));
        match stdout.into_os_string() {
            Ok(cdup) => Ok(cdup),
            Err(_) => Err(anyhow!("Could not convert cdup to path")),
        }
    }

    /// Show objects using `git show`.
    pub(crate) fn show<I, S>(
        &self,
        oids: impl IntoIterator<Item = git2::Oid>,
        pathspecs: Option<I>,
        stat: bool,
        color_opt: Option<&str>,
        diff_opts: Option<&str>,
    ) -> Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git();
        command.arg("show");
        if stat {
            command.args(["--stat", "--summary"]);
        } else {
            command.arg("--patch");
        }

        if let Some(color_opt) = map_color_opt(color_opt) {
            command.arg(format!("--color={color_opt}"));
        }

        if let Some(diff_opts) = diff_opts {
            for opt in diff_opts.split_ascii_whitespace() {
                command.arg(opt);
            }
        }

        for oid in oids {
            command.arg(oid.to_string());
        }

        command.arg("--");

        if let Some(pathspecs) = pathspecs {
            command.args(pathspecs);
        }

        command
            .stdout(Stdio::inherit())
            .output_git()?
            .require_success("show")?;
        Ok(())
    }

    /// Show object with custom pretty format.
    pub(crate) fn show_pretty(&self, oid: git2::Oid, pretty_format: &str) -> Result<Vec<u8>> {
        let output = self
            .git()
            .args(["show", "--no-patch"])
            .arg(format!("--pretty={pretty_format}"))
            .arg(oid.to_string())
            .output_git()?
            .require_success("show")?;
        Ok(output.stdout)
    }

    /// Pop stashed changes back into working tree and index.
    ///
    /// Returns Ok(true) if stash application is successful, Ok(false) if stash
    /// application results in conflicts, or Err otherwise.
    pub(crate) fn stash_pop(&self) -> Result<bool> {
        let output = self
            .git()
            .args(["stash", "pop"])
            .stdout(Stdio::inherit())
            .output_git()?;

        if output.status.success() {
            Ok(true)
        } else if output.status.code() == Some(1) {
            Ok(false)
        } else {
            Err(git_command_error("stash pop", &output.stderr))
        }
    }

    /// Stash changes from working tree and index.
    pub(crate) fn stash_push(&self) -> Result<()> {
        self.git()
            .args(["stash", "push"])
            .stdout(Stdio::inherit())
            .output_git()?
            .require_success("stash push")?;
        Ok(())
    }

    /// Show short status using `git status`.
    pub(crate) fn status_short<I, S>(&self, pathspecs: Option<I>) -> Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut command = self.git();
        command.args(["status", "-s", "--"]);
        if let Some(pathspecs) = pathspecs {
            command.args(pathspecs);
        }
        command
            .stdout(Stdio::inherit())
            .output_git()?
            .require_success("status -s")?;
        Ok(())
    }

    /// Update default index from working tree with `git update-index`.
    pub(crate) fn update_index_refresh(&self) -> Result<()> {
        self.git()
            .args(["update-index", "-q", "--unmerged", "--refresh"])
            .stdout(Stdio::null())
            .output_git()?
            .require_success("update-index")?;
        Ok(())
    }

    /// Run user-provided fetch command.
    pub(crate) fn user_fetch(&self, user_cmd_str: &str, remote_name: &str) -> Result<()> {
        let mut args = user_cmd_str.split(|c: char| c.is_ascii_whitespace());
        if let Some(command_name) = args.next() {
            let mut command = Command::new(command_name);
            self.git_dir.map(|p| command.env("GIT_DIR", p));
            self.work_dir.map(|p| command.env("GIT_WORK_TREE", p));
            self.index_path.map(|p| command.env("GIT_INDEX_FILE", p));
            let status = command
                .args(args)
                .arg(remote_name)
                .stdin(Stdio::null())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .with_context(|| format!("could not execute `{user_cmd_str}`"))?;

            if status.success() {
                Ok(())
            } else if let Some(code) = status.code() {
                Err(anyhow!("`{user_cmd_str}` exited with code {code}"))
            } else {
                Err(anyhow!("`{user_cmd_str}` failed"))
            }
        } else {
            Err(anyhow!("User-provided fetchcmd is empty"))
        }
    }

    /// Run user-provided pull command.
    ///
    /// Returns true if command returns 0, false if command returns 1, or Err otherwise.
    /// This assumes that 1 indicates the pull resulted in merge conflicts.
    pub(crate) fn user_pull(&self, user_cmd_str: &str, remote_name: &str) -> Result<bool> {
        let mut args = user_cmd_str.split(|c: char| c.is_ascii_whitespace());
        if let Some(command_name) = args.next() {
            let mut command = Command::new(command_name);
            self.git_dir.map(|p| command.env("GIT_DIR", p));
            self.work_dir.map(|p| command.env("GIT_WORK_TREE", p));
            self.index_path.map(|p| command.env("GIT_INDEX_FILE", p));
            let status = command
                .args(args)
                .arg(remote_name)
                .stdin(Stdio::null())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .with_context(|| format!("could not execute `{user_cmd_str}`"))?;

            if status.success() {
                Ok(true)
            } else if Some(1) == status.code() {
                Ok(false)
            } else if let Some(code) = status.code() {
                Err(anyhow!("`{user_cmd_str}` exited with code {code}"))
            } else {
                Err(anyhow!("`{user_cmd_str}` failed"))
            }
        } else {
            Err(anyhow!("User-provided pullcmd is empty"))
        }
    }

    /// Run user-provided rebase command.
    pub(crate) fn user_rebase(&self, user_cmd_str: &str, target: git2::Oid) -> Result<()> {
        let mut args = user_cmd_str.split(|c: char| c.is_ascii_whitespace());
        if let Some(command_name) = args.next() {
            let mut command = Command::new(command_name);
            self.git_dir.map(|p| command.env("GIT_DIR", p));
            self.work_dir.map(|p| command.env("GIT_WORK_TREE", p));
            self.index_path.map(|p| command.env("GIT_INDEX_FILE", p));
            let status = command
                .args(args)
                .arg(target.to_string())
                .stdin(Stdio::null())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .with_context(|| format!("could not execute `{user_cmd_str}`"))?;

            if status.success() {
                Ok(())
            } else if let Some(code) = status.code() {
                Err(anyhow!("`{user_cmd_str}` exited with code {code}"))
            } else {
                Err(anyhow!("`{user_cmd_str}` failed"))
            }
        } else {
            Err(anyhow!("User-provided rebasecmd is empty"))
        }
    }

    /// Get git version with `git version`.
    pub(crate) fn version(&self) -> Result<String> {
        let output = self
            .git()
            .arg("version")
            .output_git()?
            .require_success("version")?;
        let mut version_line =
            String::from_utf8(output.stdout).expect("git version should be utf8");
        if version_line.ends_with('\n') {
            version_line.pop();
        }
        Ok(version_line)
    }

    /// Write tree object from content of specified index using `git write-tree`.
    pub(crate) fn write_tree(&self) -> Result<git2::Oid> {
        let output = self
            .git()
            .arg("write-tree")
            .output_git()?
            .require_success("write-tree")?;
        parse_oid(&output.stdout)
    }
}

fn git_command_error(command: &str, stderr: &[u8]) -> anyhow::Error {
    let err_str = stderr.to_str_lossy();
    let err_str = err_str.trim_end();
    anyhow!(err_str.to_string()).context(format!("`git {command}`"))
}

fn parse_oid(output: &[u8]) -> Result<git2::Oid> {
    let oid_hex = output.to_str().context("parsing oid")?.trim_end();
    git2::Oid::from_str(oid_hex).with_context(|| format!("converting oid `{oid_hex}`"))
}

fn map_color_opt(opt: Option<&str>) -> Option<&str> {
    opt.map(|o| if o == "ansi" { "always" } else { o })
}

#[cfg(unix)]
fn is_status_signal(status: &ExitStatus, signum: i32) -> bool {
    use std::os::unix::process::ExitStatusExt;
    status.signal() == Some(signum)
}

#[cfg(not(unix))]
fn is_status_signal(_status: &ExitStatus, _signum: i32) -> bool {
    false
}
