use std::{
    ffi::{OsStr, OsString},
    io::Write,
    path::Path,
    process::{Command, Stdio},
    sync::Arc,
};

use indexmap::IndexSet;

use crate::error::Error;
use crate::signature::TimeExtended;

pub(crate) fn apply_to_index(diff: Arc<Vec<u8>>, index_path: &Path) -> Result<(), Error> {
    let mut child = Command::new("git")
        .args(["apply", "--cached"])
        // TODO: use --recount?
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(Error::GitExecute)?;

    let mut stdin = child.stdin.take().unwrap();
    let handle = std::thread::spawn(move || {
        stdin
            .write_all(diff.as_ref())
            .expect("failed to write stdin for `git apply`");
    });
    let output = child.wait_with_output()?;
    handle.join().unwrap();
    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("apply", &output.stderr))
    }
}

pub(crate) fn version() -> Result<String, Error> {
    let output = Command::new("git")
        .arg("version")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;
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

pub(crate) fn show<I, S>(
    oids: impl IntoIterator<Item = git2::Oid>,
    pathspecs: Option<I>,
    stat: bool,
    diff_opts: Option<&str>,
) -> Result<(), Error>
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
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("show", &output.stderr))
    }
}

pub(crate) fn commit_tree(
    repo_path: &std::path::Path,
    author: &git2::Signature,
    committer: &git2::Signature,
    message: &[u8],
    tree_id: git2::Oid,
    parent_ids: impl IntoIterator<Item = git2::Oid>,
    gpgsign: bool,
) -> Result<git2::Oid, Error> {
    let mut command = Command::new("git");
    command.arg("commit-tree").arg(tree_id.to_string());
    for parent_id in parent_ids {
        command.arg("-p").arg(parent_id.to_string());
    }
    if gpgsign {
        command.arg("-S");
    }
    let author_name = osstr_from_bytes(author.name_bytes());
    let author_email = osstr_from_bytes(author.email_bytes());
    let committer_name = osstr_from_bytes(committer.name_bytes());
    let committer_email = osstr_from_bytes(committer.email_bytes());

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

    let mut child = command.spawn().map_err(Error::GitExecute)?;

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

pub(crate) fn diff_index(tree_id: git2::Oid) -> Result<Vec<u8>, Error> {
    let output = Command::new("git")
        .args(["diff-index", "-p", "--full-index"])
        .arg(tree_id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-tree", &output.stderr))
    }
}

pub(crate) fn diff_tree_patch(tree1: git2::Oid, tree2: git2::Oid) -> Result<Vec<u8>, Error> {
    let output = Command::new("git")
        .args(["diff-tree", "-p", "--full-index"])
        .arg(tree1.to_string())
        .arg(tree2.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(make_cmd_err("diff-tree", &output.stderr))
    }
}

pub(crate) fn apply_treediff_to_index(
    tree1: git2::Oid,
    tree2: git2::Oid,
    worktree: &Path,
    index_path: &Path,
) -> Result<bool, Error> {
    let mut diff_tree_child = Command::new("git")
        .args(["diff-tree", "--full-index", "--binary", "--patch"])
        .arg(tree1.to_string())
        .arg(tree2.to_string())
        .arg("--")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(Error::GitExecute)?;

    let apply_output = Command::new("git")
        .args(["apply", "--cached"]) // --3way
        .env("GIT_INDEX_FILE", index_path)
        .current_dir(worktree)
        .stdin(diff_tree_child.stdout.take().unwrap())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;

    let diff_tree_output = diff_tree_child.wait_with_output()?;
    if !diff_tree_output.status.success() {
        Err(make_cmd_err("diff-tree", &diff_tree_output.stderr))
    } else if apply_output.status.success() {
        Ok(true)
    } else {
        Ok(false)
    }
}

pub(crate) fn merge_recursive_or_mergetool(
    base_tree_id: git2::Oid,
    our_tree_id: git2::Oid,
    their_tree_id: git2::Oid,
    worktree: &Path,
    index_path: &Path, // TODO: does this matter?
    use_mergetool: bool,
) -> Result<Vec<OsString>, Error> {
    if merge_recursive(base_tree_id, our_tree_id, their_tree_id, index_path)? {
        return Ok(vec![]);
    } else if use_mergetool {
        mergetool(index_path)?;
    }
    ls_files_unmerged(worktree, index_path)
}

pub(crate) fn merge_recursive(
    base_tree_id: git2::Oid,
    our_tree_id: git2::Oid,
    their_tree_id: git2::Oid,
    index_path: &Path, // TODO: does this matter?
) -> Result<bool, Error> {
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
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        Ok(true)
    } else if output.status.code() == Some(1) {
        Ok(false)
    } else {
        Err(make_cmd_err("merge-recursive", &output.stderr))
    }
}

pub(crate) fn mergetool(index_path: &Path) -> Result<bool, Error> {
    let output = Command::new("git")
        .arg("merge-tool")
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;
    if output.status.success() {
        Ok(true)
    } else if output.status.code() == Some(1) {
        Ok(false)
    } else {
        Err(make_cmd_err("mergetool", &output.stderr))
    }
}

pub(crate) fn ls_files_unmerged(
    worktree: &Path,
    index_path: &Path,
) -> Result<Vec<OsString>, Error> {
    let output = Command::new("git")
        .args(["ls-files", "--unmerged", "-z"])
        .current_dir(worktree)
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;
    if output.status.success() {
        let mut conflicts: IndexSet<OsString> = output
            .stdout
            .split(|&c| c == b'\0')
            .filter_map(|line| line.split(|&c| c == b'\t').nth(1).map(osstring_from_bytes))
            .collect();
        Ok(conflicts.drain(..).collect())
    } else {
        Err(make_cmd_err("ls-files --unmerged", &output.stderr))
    }
}

pub(crate) fn read_tree(tree_id: git2::Oid, index_path: &Path) -> Result<(), Error> {
    let output = Command::new("git")
        .arg("read-tree")
        .arg(tree_id.to_string())
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("read-tree", &output.stderr))
    }
}

pub(crate) fn read_tree_checkout(
    old_tree_id: git2::Oid,
    new_tree_id: git2::Oid,
) -> Result<(), Error> {
    let output = Command::new("git")
        .args(["read-tree", "-u", "-m", "--exclude-per-directory=.gitignore"])
        .arg(old_tree_id.to_string())
        .arg(new_tree_id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        Ok(())
    } else {
        let e = String::from_utf8(output.stderr).unwrap();
        Err(Error::CheckoutConflicts(e))
    }
}

pub(crate) fn update_index_refresh() -> Result<(), Error> {
    let output = Command::new("git")
        .args(["update-index", "-q", "--unmerged", "--refresh"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        Ok(())
    } else {
        Err(make_cmd_err("update-index", &output.stderr))
    }
}

pub(crate) fn write_tree(index_path: &Path) -> Result<git2::Oid, Error> {
    let output = Command::new("git")
        .arg("write-tree")
        .env("GIT_INDEX_FILE", index_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(Error::GitExecute)?;

    if output.status.success() {
        parse_oid(&output.stdout)
    } else {
        Err(make_cmd_err("write-tree", &output.stderr))
    }
}

pub(crate) fn interpret_trailers<'a>(
    message: String,
    trailers: impl IntoIterator<Item = (&'a str, &'a str)>,
) -> Result<String, Error> {
    let mut child = Command::new("git")
        .arg("interpret-trailers")
        .args(
            trailers
                .into_iter()
                .map(|(trailer, by)| format!("--trailer={}={}", trailer, by)),
        )
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(Error::GitExecute)?;

    let mut stdin = child.stdin.take().unwrap();
    std::thread::spawn(move || {
        stdin
            .write_all(message.as_bytes())
            .expect("failed to write stdin for `git interpret-trailers`");
    });

    let output = child.wait_with_output()?;
    let message = unsafe { String::from_utf8_unchecked(output.stdout) };
    Ok(message)
}

fn make_cmd_err(command_name: &str, stderr: &[u8]) -> Error {
    let err_str = String::from_utf8_lossy(stderr);
    Error::GitCommand(command_name.to_string(), err_str.trim_end().to_string())
}

fn parse_oid(output: &[u8]) -> Result<git2::Oid, Error> {
    let oid_hex = std::str::from_utf8(output)
        .expect("object name must be utf8")
        .trim_end(); // Trim trailing newline
    Ok(git2::Oid::from_str(oid_hex)?)
}

#[cfg(unix)]
pub(crate) fn osstr_from_bytes(b: &[u8]) -> &OsStr {
    use std::os::unix::ffi::OsStrExt;
    OsStr::from_bytes(b)
}

#[cfg(windows)]
pub(crate) fn osstr_from_bytes(b: &[u8]) -> &OsStr {
    std::str::from_utf8(b).expect("paths on Windows must be utf8")
}

#[cfg(unix)]
pub(crate) fn osstring_from_bytes(b: &[u8]) -> OsString {
    use std::os::unix::ffi::OsStrExt;
    OsString::from(OsStr::from_bytes(b))
}

#[cfg(windows)]
pub(crate) fn osstring_from_bytes(b: &[u8]) -> OsString {
    OsString::from(std::str::from_utf8(b).expect("paths on Windows must be utf8"))
}
