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
    process::{Child, Command, Output, Stdio},
};

use anyhow::{anyhow, Context, Result};
use bstr::{BString, ByteSlice, ByteVec};
use indexmap::IndexSet;

use crate::signature::TimeExtended;

const GIT_EXEC_FAIL: &str = "could not execute `git`";

/// Apply a patch (diff) to the specified index using `git apply --cached`.
pub(crate) fn apply_to_index(diff: &[u8], worktree: &Path, index_path: &Path) -> Result<()> {
    let child = Command::new("git")
        .args(["apply", "--cached"])
        // TODO: use --recount?
        .env("GIT_INDEX_FILE", index_path)
        .current_dir(worktree)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context(GIT_EXEC_FAIL)?;
    let output = in_and_out(child, diff)?;
    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("apply", &output.stderr))
    }
}

/// Apply diff between two trees to specified index.
///
/// Pipes `git diff-tree | git apply --cached`.
///
/// Returns `true` if the patch application is successful, `false` otherwise.
pub(crate) fn apply_treediff_to_index(
    tree1: git2::Oid,
    tree2: git2::Oid,
    worktree: &Path,
    index_path: &Path,
) -> Result<bool> {
    let mut diff_tree_child = Command::new("git")
        .args(["diff-tree", "--full-index", "--binary", "--patch"])
        .arg(tree1.to_string())
        .arg(tree2.to_string())
        .arg("--")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context(GIT_EXEC_FAIL)?;

    let apply_output = Command::new("git")
        .args(["apply", "--cached"]) // --3way
        .env("GIT_INDEX_FILE", index_path)
        .current_dir(worktree)
        .stdin(diff_tree_child.stdout.take().unwrap())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    let diff_tree_output = diff_tree_child.wait_with_output()?;
    if !diff_tree_output.status.success() {
        Err(make_cmd_err("diff-tree", &diff_tree_output.stderr))
    } else if apply_output.status.success() {
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Create a commit for the specified tree id using `git commit-tree`.
///
/// The newly created commit id is returned.
pub(crate) fn commit_tree(
    repo_path: &std::path::Path,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &[u8],
    tree_id: git2::Oid,
    parent_ids: impl IntoIterator<Item = git2::Oid>,
    gpgsign: bool,
) -> Result<git2::Oid> {
    let mut command = Command::new("git");
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

    command
        .env("GIT_AUTHOR_NAME", author_name)
        .env("GIT_AUTHOR_EMAIL", author_email)
        .env("GIT_COMMITTER_NAME", committer_name)
        .env("GIT_COMMITTER_EMAIL", committer_email)
        // TODO: reencode dates?
        .env("GIT_AUTHOR_DATE", author.epoch_time_string())
        .env("GIT_COMMITTER_DATE", committer.epoch_time_string())
        .env("GIT_DIR", repo_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = command.spawn().context(GIT_EXEC_FAIL)?;

    {
        child.stdin.take().unwrap().write_all(message)?;
    }

    let output = child.wait_with_output()?;
    if output.status.success() {
        parse_oid(&output.stdout)
    } else {
        Err(make_cmd_err("commit-tree", &output.stderr))
    }
}

/// Interactive diff
pub(crate) fn diff<I, S>(
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
    let mut command = Command::new("git");
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

    let output = command
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("diff", &output.stderr))
    }
}

/// Generate diff between specified tree and the working tree or index with `git diff-index`.
pub(crate) fn diff_index(tree_id: git2::Oid) -> Result<Vec<u8>> {
    let output = Command::new("git")
        .args(["diff-index", "-p", "--full-index"])
        .arg(tree_id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-index", &output.stderr))
    }
}

pub(crate) fn diff_index_names(tree_id: git2::Oid, relative: Option<&Path>) -> Result<Vec<u8>> {
    let mut command = Command::new("git");
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
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-index", &output.stderr))
    }
}

/// Interative diff-tree (for 'stg files').
pub(crate) fn diff_tree_files(
    tree1: git2::Oid,
    tree2: git2::Oid,
    stat: bool,
    name_only: bool,
    color_opt: Option<&str>,
) -> Result<Vec<u8>> {
    let mut command = Command::new("git");
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
    let output = command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-tree", &output.stderr))
    }
}

/// Generate diff between two trees using `git diff-tree -p`.
pub(crate) fn diff_tree_patch<I, S>(
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
    let mut command = Command::new("git");
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
    let output = command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-tree", &output.stderr))
    }
}

/// Add trailers to commit message with `git interpret-trailers`.
pub(crate) fn interpret_trailers<'a>(
    message: &[u8],
    trailers: impl IntoIterator<Item = (&'a str, &'a str)>,
) -> Result<Vec<u8>> {
    let child = Command::new("git")
        .arg("interpret-trailers")
        .args(
            trailers
                .into_iter()
                .map(|(trailer, by)| format!("--trailer={}={}", trailer, by)),
        )
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .context(GIT_EXEC_FAIL)?;

    let output = in_and_out(child, message)?;
    Ok(output.stdout)
}

/// Perform three-way merge with `git merge-recursive`.
///
/// Returns `true` if the merge was successful, `false` otherwise.
pub(crate) fn merge_recursive(
    base_tree_id: git2::Oid,
    our_tree_id: git2::Oid,
    their_tree_id: git2::Oid,
    index_path: &Path, // TODO: does this matter?
) -> Result<bool> {
    let output = Command::new("git")
        .arg("merge-recursive")
        .arg(base_tree_id.to_string())
        .arg("--")
        .arg(our_tree_id.to_string())
        .arg(their_tree_id.to_string())
        .env("GIT_INDEX_FILE", index_path)
        .env(format!("GITHEAD_{}", base_tree_id), "ancestor")
        .env(format!("GITHEAD_{}", our_tree_id), "current")
        .env(format!("GITHEAD_{}", their_tree_id), "patched")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(true)
    } else if output.status.code() == Some(1) {
        Ok(false)
    } else {
        Err(make_cmd_err("merge-recursive", &output.stderr))
    }
}

/// Perfom three-way merge, with optional auto-resolution of conflicts with `git merge-tool`.
pub(crate) fn merge_recursive_or_mergetool(
    base_tree_id: git2::Oid,
    our_tree_id: git2::Oid,
    their_tree_id: git2::Oid,
    worktree: &Path,
    index_path: &Path, // TODO: does this matter?
    use_mergetool: bool,
) -> Result<Vec<OsString>> {
    if merge_recursive(base_tree_id, our_tree_id, their_tree_id, index_path)? {
        return Ok(vec![]);
    } else if use_mergetool {
        mergetool(index_path)?;
    }
    ls_files_unmerged(worktree, index_path)
}

/// Attempt to resolve outstanding merge conflicts with `git merge-tool`.
pub(crate) fn mergetool(index_path: &Path) -> Result<bool> {
    let output = Command::new("git")
        .arg("merge-tool")
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        Ok(true)
    } else if output.status.code() == Some(1) {
        Ok(false)
    } else {
        Err(make_cmd_err("mergetool", &output.stderr))
    }
}

/// Gather list of unmerged files using `git ls-files --unmerged`.
pub(crate) fn ls_files_unmerged(worktree: &Path, index_path: &Path) -> Result<Vec<OsString>> {
    let output = Command::new("git")
        .args(["ls-files", "--unmerged", "-z"])
        .current_dir(worktree)
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        let mut conflicts: IndexSet<OsString> = output
            .stdout
            .split_str(b"\0")
            .filter_map(|line| {
                line.split_str(b"\t").nth(1).map(|name_bytes| {
                    name_bytes
                        .to_os_str()
                        .expect("git file names must be utf-8 on Windows")
                        .into()
                })
            })
            .collect();
        Ok(conflicts.drain(..).collect())
    } else {
        Err(make_cmd_err("ls-files --unmerged", &output.stderr))
    }
}

/// Copy notes from one object to another using `git notes copy`.
pub(crate) fn notes_copy(from_oid: git2::Oid, to_oid: git2::Oid) -> Result<()> {
    let output = Command::new("git")
        .args(["notes", "copy"])
        .arg(from_oid.to_string())
        .arg(to_oid.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("notes copy", &output.stderr))
    }
}

/// Read content of a tree into specified index using `git read-tree`.
pub(crate) fn read_tree(tree_id: git2::Oid, index_path: &Path) -> Result<()> {
    let output = Command::new("git")
        .arg("read-tree")
        .arg(tree_id.to_string())
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("read-tree", &output.stderr))
    }
}

/// Checkout tree to working tree using `git read-tree`.
pub(crate) fn read_tree_checkout(old_tree_id: git2::Oid, new_tree_id: git2::Oid) -> Result<()> {
    let output = Command::new("git")
        .args([
            "read-tree",
            "-m",
            "-u",
            "--exclude-per-directory=.gitignore",
        ])
        .arg(old_tree_id.to_string())
        .arg(new_tree_id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("read-tree -m -u", &output.stderr))
    }
}

/// Get list of revisions using `git rev-list`.
pub(crate) fn rev_list<I, S>(
    base: git2::Oid,
    top: git2::Oid,
    worktree: Option<&Path>,
    pathspecs: Option<I>,
) -> Result<Vec<git2::Oid>>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut command = Command::new("git");
    command.arg("rev-list").arg(format!("{base}..{top}"));

    if let Some(worktree) = worktree {
        command.current_dir(worktree);
    }

    command.arg("--");
    if let Some(pathspecs) = pathspecs {
        command.args(pathspecs);
    }

    let output = command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        let mut oids: Vec<git2::Oid> = Vec::new();
        for line in output
            .stdout
            .split_str("\n")
            .filter(|line| !line.is_empty())
        {
            let oid_str = line.to_str().context("converting oid")?;
            let oid = git2::Oid::from_str(oid_str)
                .with_context(|| format!("converting oid `{oid_str}`"))?;
            oids.push(oid)
        }
        Ok(oids)
    } else {
        Err(make_cmd_err("rev-list", &output.stderr))
    }
}

/// Show objects using `git show`.
pub(crate) fn show<I, S>(
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
    let mut command = Command::new("git");
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

    let output = command
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("show", &output.stderr))
    }
}

/// Update default index from working tree with `git update-index`.
pub(crate) fn update_index_refresh() -> Result<()> {
    let output = Command::new("git")
        .args(["update-index", "-q", "--unmerged", "--refresh"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("update-index", &output.stderr))
    }
}

/// Get git version with `git version`.
pub(crate) fn version() -> Result<String> {
    let output = Command::new("git")
        .arg("version")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;
    if output.status.success() {
        let mut version_line =
            String::from_utf8(output.stdout).expect("git version should be utf8");
        if version_line.ends_with('\n') {
            version_line.pop();
        }
        Ok(version_line)
    } else {
        Err(make_cmd_err("version", &output.stderr))
    }
}

/// Write tree object from content of specified index using `git write-tree`.
pub(crate) fn write_tree(index_path: &Path) -> Result<git2::Oid> {
    let output = Command::new("git")
        .arg("write-tree")
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context(GIT_EXEC_FAIL)?;

    if output.status.success() {
        parse_oid(&output.stdout)
    } else {
        Err(make_cmd_err("write-tree", &output.stderr))
    }
}

fn make_cmd_err(command_name: &str, stderr: &[u8]) -> anyhow::Error {
    let err_str = String::from_utf8_lossy(stderr);
    let err_str = err_str.trim_end().to_string();
    anyhow!(err_str).context(format!("`git {command_name}`"))
}

fn parse_oid(output: &[u8]) -> Result<git2::Oid> {
    let oid_hex = std::str::from_utf8(output)
        .expect("object name must be utf8")
        .trim_end(); // Trim trailing newline
    Ok(git2::Oid::from_str(oid_hex)?)
}

/// Write input to child process and gather its output.
///
/// The input data is written from a separate thread to avoid potential
/// deadlock that can occur if the child process's input buffer is filled
/// without concurrently reading from the child's stdout and stderr.
fn in_and_out(mut child: Child, input: &[u8]) -> Result<Output> {
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
    let output = output_result?;
    Ok(output)
}

fn map_color_opt(opt: Option<&str>) -> Option<&str> {
    opt.map(|o| if o == "ansi" { "always" } else { o })
}
