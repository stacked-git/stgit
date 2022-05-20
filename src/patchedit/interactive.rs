// SPDX-License-Identifier: GPL-2.0-only

//! Functions for conducting interactive patch edit session.

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use anyhow::{anyhow, Result};

use super::description::{EditablePatchDescription, EditedPatchDescription};

// static EDIT_INSTRUCTION: &'static str = "\
//     # Everything here is editable! You can modify the patch name,\n\
//     # author, date, commit message, and the diff (if --diff was given).\n\
//     # Lines starting with '#' will be ignored, and an empty message\n\
//     # aborts the edit.\n";

pub(crate) static EDIT_INSTRUCTION: &str = "\
    # Please enter the message for your patch. Lines starting with\n\
    # '#' will be ignored. An empty message aborts the new patch.\n\
    # The patch name and author information may also be modified.\n";

pub(crate) static EDIT_INSTRUCTION_READ_ONLY_DIFF: &str =
    "# The diff below is for reference. Any edits will be ignored.\n";

pub(crate) static EDIT_INSTRUCTION_EDITABLE_DIFF: &str = "# The diff below may be edited.\n";

/// Default file name for interactively editable patch description.
static EDIT_FILE_NAME: &str = ".stgit-edit.txt";

/// Conduct interactive patch edit session.
///
/// The patch description is written to a file, the user's editor of choice is invoked,
/// and the modified description is read-back and parsed.
pub(super) fn edit_interactive(
    patch_desc: &EditablePatchDescription,
    config: &git2::Config,
) -> Result<EditedPatchDescription> {
    {
        let file = File::create(EDIT_FILE_NAME)?;
        let mut stream = BufWriter::new(file);
        patch_desc.write(&mut stream)?;
    }

    let buf = call_editor(EDIT_FILE_NAME, config)?;
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
pub(crate) fn call_editor<P: AsRef<Path>>(path: P, config: &git2::Config) -> Result<Vec<u8>> {
    let editor = get_editor(config);

    if editor != *":" {
        let use_advice = config.get_bool("advice.waitingForEditor").unwrap_or(true);
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
                "Problem with the editor `{}`",
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
fn get_editor(config: &git2::Config) -> OsString {
    if let Some(editor) = std::env::var_os("GIT_EDITOR") {
        editor
    } else if let Ok(editor) = config.get_path("stgit.editor") {
        editor.into()
    } else if let Ok(editor) = config.get_path("core.editor") {
        editor.into()
    } else if let Some(editor) = std::env::var_os("VISUAL") {
        editor
    } else if let Some(editor) = std::env::var_os("EDITOR") {
        editor
    } else {
        OsString::from("vi")
    }
}
