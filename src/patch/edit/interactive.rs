// SPDX-License-Identifier: GPL-2.0-only

//! Functions for conducting interactive patch edit session.

use std::{
    ffi::{OsStr, OsString},
    fs::File,
    io::{BufWriter, Read, Write},
    path::Path,
};

use anyhow::{anyhow, Context, Result};
use bstr::{BString, ByteSlice};

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
    config: &gix::config::Snapshot,
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
    config: &gix::config::Snapshot,
) -> Result<BString> {
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

        let result = run_editor(&editor, path.as_ref());

        if use_advice && !is_dumb {
            let mut stderr = std::io::stderr();
            stderr.write_all("\r\x1b[K".as_bytes()).unwrap_or(());
            stderr.flush()?;
        }

        let status = result?;

        if !status.success() {
            return Err(anyhow!(
                "problem with the editor `{}`",
                editor.to_string_lossy()
            ));
        }
    }

    let buf = std::fs::read(&path)?.into();
    std::fs::remove_file(&path)?;
    Ok(buf)
}

fn run_editor<P: AsRef<Path>>(editor: &OsStr, path: P) -> Result<std::process::ExitStatus> {
    let prep = gix_command::prepare(editor)
        .arg(path.as_ref())
        .with_shell_allow_argument_splitting()
        .stdout(std::process::Stdio::inherit());

    let mut command = if cfg!(windows) && !prep.use_shell {
        if let Some(interpreter) = parse_interpreter(&prep.command) {
            let mut cmd = std::process::Command::new(interpreter);
            cmd.arg(prep.command).arg(path.as_ref());
            cmd
        } else {
            std::process::Command::from(prep)
        }
    } else {
        std::process::Command::from(prep)
    };
    let mut child = command
        .spawn()
        .with_context(|| format!("running editor: {}", editor.to_string_lossy()))?;

    child
        .wait()
        .with_context(|| format!("waiting editor: {}", editor.to_string_lossy()))
}

/// Determine user's editor of choice based on config and environment.
fn get_editor(config: &gix::config::Snapshot) -> Result<OsString> {
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

fn parse_interpreter(command: &OsStr) -> Option<OsString> {
    let command_path = Path::new(command);
    if command_path.extension().and_then(|ext| ext.to_str()) == Some("exe") {
        return None;
    }

    let mut buffer = [0; 128];
    if let Some(n) = std::fs::File::open(command_path)
        .ok()
        .and_then(|mut file| file.read(&mut buffer).ok())
    {
        parse_shebang(&buffer[..n])
            .and_then(|bytes| bytes.to_os_str().ok())
            .map(|osstr| osstr.to_os_string())
    } else {
        None
    }
}

fn parse_shebang(buffer: &[u8]) -> Option<&[u8]> {
    buffer
        .as_bstr()
        .lines()
        .next()
        .and_then(|line| line.strip_prefix(b"#!"))
        .and_then(|shebang| {
            shebang.rfind_byteset(b"/\\").map(|index| {
                if let Some(space_index) = shebang[index..].find_byte(b' ') {
                    &shebang[..index + space_index]
                } else {
                    shebang
                }
            })
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_shebang() {
        assert_eq!(parse_shebang(b"#!/bin/sh\nsome stuff").unwrap(), b"/bin/sh");
    }

    #[test]
    fn shebang_with_options() {
        assert_eq!(
            parse_shebang(b"#!/bin/sh -i -o -u\nsome stuff").unwrap(),
            b"/bin/sh"
        );
    }

    #[test]
    fn shebang_with_backslashes() {
        assert_eq!(
            parse_shebang(b"#!C:\\Program Files\\Imashell.exe\nsome stuff").unwrap(),
            b"C:\\Program Files\\Imashell.exe"
        );
    }

    #[test]
    fn shebang_with_trailing_space() {
        assert_eq!(
            parse_shebang(b"#!/bin/sh         \nsome stuff").unwrap(),
            b"/bin/sh"
        );
    }

    #[test]
    fn not_a_shebang() {
        assert!(parse_shebang(b"/bin/sh\nsome stuff").is_none());
    }
}
