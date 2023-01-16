// SPDX-License-Identifier: GPL-2.0-only

//! Functions for conducting interactive patch edit session.

use std::{
    ffi::OsString,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use anyhow::{anyhow, Result};

use super::description::{EditablePatchDescription, EditedPatchDescription};

pub(crate) static EDIT_INSTRUCTION: &str = "\
    # Please enter the message for your patch. Lines starting with\n\
    # '#' will be ignored. An empty message aborts the new patch.\n\
    # The patch name and author information may also be modified.\n";

pub(crate) static EDIT_INSTRUCTION_READ_ONLY_DIFF: &str =
    "# The diff below is for reference. Any edits will be ignored.\n";

pub(crate) static EDIT_INSTRUCTION_EDITABLE_DIFF: &str = "# The diff below may be edited.\n";

/// Default file name for interactively editable patch description.
static EDIT_FILE_NAME: &str = ".stgit-edit.txt";

/// Default file name for interactively editable patch description with diff.
static EDIT_FILE_NAME_DIFF: &str = ".stgit-edit.patch";

/// Conduct interactive patch edit session.
///
/// The patch description is written to a file, the user's editor of choice is invoked,
/// and the modified description is read-back and parsed.
pub(super) fn edit_interactive(
    patch_desc: &EditablePatchDescription,
    config: &git_repository::config::Snapshot,
) -> Result<EditedPatchDescription> {
    let filename = if patch_desc.diff.is_some() {
        EDIT_FILE_NAME_DIFF
    } else {
        EDIT_FILE_NAME
    };

    {
        let file = File::create(filename)?;
        let mut stream = BufWriter::new(file);
        patch_desc.write(&mut stream)?;
    }

    let buf = call_editor(filename, config)?;
    let edited_desc = EditedPatchDescription::try_from(buf.as_slice())?;
    Ok(edited_desc)
}

/// Make determination about whether terminal is dumb.
///
/// Dumb terminals do not allow moving the cursor backwards.
fn is_terminal_dumb() -> bool {
    if let Some(value) = std::env::var_os("TERM") {
        value == *"dumb"
    } else {
        true
    }
}

/// Run the user's editor to edit the file at `path`.
///
/// Upon successfully reading back the file's content, the file is deleted.
pub(crate) fn call_editor<P: AsRef<Path>>(
    path: P,
    config: &git_repository::config::Snapshot,
) -> Result<Vec<u8>> {
    let editor = get_editor(config)?;

    if editor != *":" {
        let use_advice = config.boolean("advice.waitingForEditor").unwrap_or(true);
        let is_dumb = cfg!(target_os = "windows") || is_terminal_dumb();

        if use_advice {
            let mut stderr = std::io::stderr();
            write!(
                stderr,
                "hint: Waiting for your editor to close the file...{}",
                if is_dumb { '\n' } else { ' ' }
            )?;
            stderr.flush()?;
        }

        let mut subcommand = OsString::new();
        subcommand.push(&editor);
        subcommand.push(" ");
        subcommand.push(path.as_ref().as_os_str());

        let shell = if cfg!(target_os = "windows") {
            "sh"
        } else {
            "/bin/sh"
        };
        let mut child = std::process::Command::new(shell)
            .arg("-c")
            .arg(subcommand)
            .spawn()?;

        let status = child.wait()?;

        if !status.success() {
            return Err(anyhow!(
                "problem with the editor `{}`",
                editor.to_string_lossy()
            ));
        }

        if use_advice && !is_dumb {
            let mut stderr = std::io::stderr();
            stderr.write_all("\r\x1b[K".as_bytes()).unwrap_or(());
            stderr.flush()?;
        }
    }

    let buf = std::fs::read(&path)?;
    std::fs::remove_file(&path)?;
    Ok(buf)
}

/// Determine user's editor of choice based on config and environment.
fn get_editor(config: &git_repository::config::Snapshot) -> Result<OsString> {
    let editor = if let Some(editor) = std::env::var_os("GIT_EDITOR") {
        editor
    } else if let Some(editor) = config
        .trusted_path("stgit.editor")
        .transpose()?
        .map(|p| p.as_os_str().to_os_string())
    {
        editor
    } else if let Some(editor) = config
        .trusted_path("core.editor")
        .transpose()?
        .map(|p| p.as_os_str().to_os_string())
    {
        editor
    } else if let Some(editor) = std::env::var_os("VISUAL") {
        editor
    } else if let Some(editor) = std::env::var_os("EDITOR") {
        editor
    } else {
        OsString::from("vi")
    };
    Ok(editor)
}
