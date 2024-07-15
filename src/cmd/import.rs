// SPDX-License-Identifier: GPL-2.0-only

//! `stg import` implementation.

use std::{
    io::Read,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use bstr::{BStr, BString, ByteSlice, ByteVec};
use clap::{Arg, ArgGroup};

use crate::{
    color::get_color_stdout,
    ext::{RepositoryExtended, TimeExtended},
    patch::{patchedit, PatchName},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "import",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    let app = clap::Command::new("import")
        .about("Import patches to stack")
        .long_about(
            "Import patches from various sources to the stack.\n\
             \n\
             The simplest usage is to import a diff/patch file into the stack from a \
             local file. By default, the file name is used as the patch name, but this \
             can be overridden with '--name'. The patch can either be a normal file \
             with the description at the top, or it can have standard mail format. The \
             \"Subject\", \"From\", and \"Date\" headers will be used for the imported \
             patch's author details.\n\
             \n\
             Patches may also be imported from a mail file (-m/--mail), an mbox \
             (-M/--mbox), or a series (-S/--series). Furthermore, the -u/--url option \
             allows the patches source to be fetched from a url instead of from a \
             local file.\n\
             \n\
             If a patch does not apply cleanly import is aborted unless '--reject' \
             is specified, in which case it will apply to the work tree the parts \
             of the patch that are  applicable, leave the rejected hunks in \
             corresponding *.rej files, and add an empty patch to the stack.\n\
             \n\
             The patch description must be separated from the diff with a \"---\" line.",
        )
        .override_usage(super::make_usage(
            "stg import",
            if cfg!(feature = "import-url") {
                &[
                    "[OPTIONS] <diff-path>",
                    "[OPTIONS] -m [<mail-path>|<Maildir-path>]",
                    "[OPTIONS] -M [<mbox-path>]",
                    "[OPTIONS] -S [<series-path>]",
                    "[OPTIONS] -u <diff-url>",
                    "[OPTIONS] -u -m <mail-url>",
                    "[OPTIONS] -u -M <mbox-url>",
                    "[OPTIONS] -u -S <series-url>",
                ]
            } else {
                &[
                    "[OPTIONS] <diff-path>",
                    "[OPTIONS] -m [<mail-path>|<Maildir-path>]",
                    "[OPTIONS] -M [<mbox-path>]",
                    "[OPTIONS] -S [<series-path>]",
                ]
            },
        ))
        .arg(
            Arg::new("source")
                .help("Source of patches to import")
                .long_help(
                    "Source of patches to import. May be a path to a local file or a \
                     URL if the '--url' option is provided. The default is to read \
                     from stdin if no source argument is provided.",
                )
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(clap::ValueHint::AnyPath),
        )
        .next_help_heading("Source Options")
        .arg(
            Arg::new("mail")
                .long("mail")
                .short('m')
                .help("Import patch from an email file")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("mbox")
                .long("mbox")
                .short('M')
                .help("Import patch series from an mbox file")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("series")
                .long("series")
                .short('S')
                .help("Import patch series")
                .long_help("Import patch series from a series file are tar archive.")
                .action(clap::ArgAction::SetTrue),
        )
        .group(ArgGroup::new("whence").args(["mail", "mbox", "series"]));

    let app = if cfg!(feature = "import-url") {
        app.arg(
            Arg::new("url")
                .long("url")
                .short('u')
                .help("Retrieve source from a url instead of local file")
                .action(clap::ArgAction::SetTrue)
                .requires("source"),
        )
    } else {
        app
    };

    let app = app
        .next_help_heading("Import Options")
        .arg(
            Arg::new("name")
                .long("name")
                .short('n')
                .help("Use <name> as the patch name")
                .value_name("name")
                .value_parser(clap::value_parser!(PatchName)),
        )
        .arg(
            Arg::new("strip")
                .long("strip")
                .short('p')
                .help("Remove <n> leading components from diff paths (default 1)")
                .value_name("n")
                .value_parser(crate::argset::parse_usize),
        )
        .arg(
            Arg::new("directory")
                .long("directory")
                .help("Prepend <root> to all filenames")
                .long_help(
                    "Prepend <root> to all filenames. If a \"-p\" argument is also \
                    passed, it is applied before prepending the new root.",
                )
                .value_name("root")
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(clap::ValueHint::DirPath),
        )
        .arg(
            Arg::new("stripname")
                .long("stripname")
                .short('t')
                .help("Strip number and extension from patch name")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("context-lines")
                .short('C')
                .help("Ensure <n> lines of matching context for each change")
                .value_name("n")
                .value_parser(crate::argset::parse_usize),
        )
        .arg(
            Arg::new("3way")
                .long("3way")
                .short('3')
                .help("Attempt three-way merge")
                .long_help(
                    "Attempt 3-way merge if the patch records the identity of blobs it \
                    is supposed to apply to and those blobs are available locally.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("ignore")
                .long("ignore")
                .short('i')
                .help("Ignore the applied patches in the series")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("replace")
                .long("replace")
                .help("Replace the unapplied patches in the series")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("base")
                .long("base")
                .short('b')
                .help("Use <committish> instead of HEAD for file importing")
                .value_name("committish"),
        )
        .arg(
            Arg::new("reject")
                .long("reject")
                .help("Leave rejected hunks in \".rej\" files")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("keep-cr")
                .long("keep-cr")
                .help("Do not remove \"\\r\" from email lines ending with \"\\r\\n\"")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("message-id")
                .long("message-id")
                .help("Create Message-ID trailer from Message-ID header")
                .long_help(
                    "Create Message-ID trailer in patch description based on the \
                    Message-ID email header. This option is applicable when importing \
                    with '--mail' or '--mbox'. This behavior may also be enabled via \
                    the \"stgit.import.message-id\" configuration option.",
                )
                .action(clap::ArgAction::SetTrue),
        );
    patchedit::add_args(app, false, false)
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AutoInitialize)?;
    let stupid = repo.stupid();

    let source_path = if cfg!(feature = "import-url") && matches.get_flag("url") {
        None
    } else if let Some(path) = matches.get_one::<PathBuf>("source") {
        let abs_path = gix::path::realpath(path)?;
        Some(abs_path)
    } else {
        None
    };

    let statuses = stupid.statuses(None)?;
    statuses.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;
    //stupid.update_index_refresh()?;

    if cfg!(feature = "import-url") && matches.get_flag("url") {
        import_url(stack, matches)
    } else if matches.get_flag("series") {
        import_series(stack, matches, source_path.as_deref())
    } else if matches.get_flag("mail") || matches.get_flag("mbox") {
        import_mail(stack, matches, source_path.as_deref())
    } else {
        import_file(stack, matches, source_path.as_deref(), None)?;
        Ok(())
    }
}

#[cfg(not(feature = "import-url"))]
fn import_url(_stack: Stack, _matches: &clap::ArgMatches) -> Result<()> {
    Err(anyhow!(
        "StGit not built with support for downloading imports"
    ))
}

#[cfg(feature = "import-url")]
fn import_url(stack: Stack, matches: &clap::ArgMatches) -> Result<()> {
    use std::io::Write;

    let url_osstr = matches
        .get_one::<PathBuf>("source")
        .expect("source url must be present")
        .clone()
        .into_os_string();
    let url_str = url_osstr
        .to_str()
        .ok_or_else(|| anyhow!("source url is not UTF-8 encoded"))?;
    let mut handle = curl::easy::Easy::new();
    handle.url(url_str)?;
    let url_decoded = handle.url_decode(url_str);
    let filename = url_decoded
        .rsplit_str(b"/")
        .next()
        .and_then(|b| b.to_str().ok())
        .unwrap_or("patch");
    let download_dir = tempfile::tempdir()?;
    let download_path = download_dir.path().join(filename);
    let mut download_file = std::fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&download_path)?;
    let mut transfer = handle.transfer();
    transfer.write_function(|data| {
        download_file.write_all(data).unwrap();
        Ok(data.len())
    })?;

    match transfer.perform() {
        Ok(()) => {}
        Err(e) if e.is_url_malformed() && url_str.starts_with("file://") => {
            let source_path = Path::new(url_str.strip_prefix("file://").unwrap());
            std::fs::copy(source_path, &download_path)
                .with_context(|| format!("copying {url_str}"))?;
        }
        e @ Err(_) => e?,
    }

    if matches.get_flag("series") {
        import_series(stack, matches, Some(download_path.as_path()))
    } else if matches.get_flag("mail") || matches.get_flag("mbox") {
        import_mail(stack, matches, Some(download_path.as_path()))
    } else {
        import_file(stack, matches, Some(download_path.as_path()), None)?;
        Ok(())
    }
}

fn import_tgz_series(stack: Stack, matches: &clap::ArgMatches, source_path: &Path) -> Result<()> {
    let source_file = std::fs::File::open(source_path)?;
    let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(source_file));
    let temp_dir = tempfile::tempdir()?;
    archive.unpack(temp_dir.path())?;
    let series_path = find_series_path(temp_dir.path())?;
    return import_series(stack, matches, Some(series_path.as_path()));
}

fn import_tbz2_series(stack: Stack, matches: &clap::ArgMatches, source_path: &Path) -> Result<()> {
    let source_file = std::fs::File::open(source_path)?;
    let mut archive = tar::Archive::new(bzip2_rs::DecoderReader::new(source_file));
    let temp_dir = tempfile::tempdir()?;
    archive.unpack(temp_dir.path())?;
    let series_path = find_series_path(temp_dir.path())?;
    return import_series(stack, matches, Some(series_path.as_path()));
}

fn import_tar_series(stack: Stack, matches: &clap::ArgMatches, source_path: &Path) -> Result<()> {
    let source_file = std::fs::File::open(source_path)?;
    let mut archive = tar::Archive::new(source_file);
    let temp_dir = tempfile::tempdir()?;
    archive.unpack(temp_dir.path())?;
    let series_path = find_series_path(temp_dir.path())?;
    return import_series(stack, matches, Some(series_path.as_path()));
}

fn import_series(
    stack: Stack,
    matches: &clap::ArgMatches,
    source_path: Option<&Path>,
) -> Result<()> {
    let series = if let Some(source_path) = source_path {
        if let Some(filename) = source_path.file_name() {
            let filename = filename.to_string_lossy().to_ascii_lowercase();
            if filename.ends_with(".tar.gz") || filename.ends_with(".tgz") {
                return import_tgz_series(stack, matches, source_path);
            } else if filename.ends_with(".tar.bz2") {
                return import_tbz2_series(stack, matches, source_path);
            } else if filename.ends_with(".tar") {
                return import_tar_series(stack, matches, source_path);
            }
        }
        std::fs::read(source_path)?
    } else {
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let mut buf = Vec::new();
        stdin.read_to_end(&mut buf)?;
        buf
    };

    let mut stack = stack;

    for line in series.lines() {
        let line = line
            .find_char('#')
            .map_or(line, |pos| &line[..pos])
            .trim_with(|c| c.is_ascii_whitespace());
        if line.is_empty() {
            continue;
        }

        let mut fields = line.fields_with(|c| c.is_ascii_whitespace());
        let raw_patchname = fields
            .next()
            .expect("non-empty line must have first field")
            .to_os_str()
            .context("converting patch name from series to file name")?;
        let patch_path = source_path.map_or_else(
            || PathBuf::from(raw_patchname),
            |p| p.with_file_name(raw_patchname),
        );

        let strip_level = if let Some(extra) = fields.next() {
            if extra.starts_with_str("-p") {
                if extra == b"-p0" {
                    Some(0)
                } else {
                    return Err(anyhow!(
                        "patch `{}` has unsupported strip level \"{}\"",
                        raw_patchname.to_string_lossy(),
                        extra.to_str_lossy()
                    ));
                }
            } else {
                None
            }
        } else {
            None
        };

        stack = import_file(stack, matches, Some(patch_path.as_path()), strip_level)?;
    }
    Ok(())
}

fn find_series_path(base: &Path) -> Result<PathBuf> {
    for entry in base.read_dir()? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            if let Ok(path) = find_series_path(&entry.path()) {
                return Ok(path);
            }
        } else if file_type.is_file() && entry.file_name() == std::ffi::OsStr::new("series") {
            return Ok(entry.path());
        }
    }
    Err(anyhow!("series file not found"))
}

fn use_message_id(matches: &clap::ArgMatches, config: &gix::config::Snapshot) -> bool {
    matches.get_flag("message-id") || config.boolean("stgit.import.message-id").unwrap_or(false)
}

fn import_mail(stack: Stack, matches: &clap::ArgMatches, source_path: Option<&Path>) -> Result<()> {
    let out_dir = tempfile::tempdir()?;
    let missing_from_ok = matches.get_flag("mail");
    let keep_cr = matches.get_flag("keep-cr");
    let message_id = use_message_id(matches, &stack.repo.config_snapshot());
    let stupid = stack.repo.stupid();
    let num_patches = stupid.mailsplit(source_path, out_dir.path(), keep_cr, missing_from_ok)?;
    let mut stack = stack;
    for i in 1..=num_patches {
        let patch_path = out_dir.path().join(format!("{i:04}"));
        let patch_file = std::fs::File::open(patch_path)?;
        let (mailinfo, message, diff) = stupid.mailinfo(Some(patch_file), message_id)?;
        let headers = Headers::parse_mailinfo(mailinfo.as_bstr()).unwrap_or_default();
        stack = create_patch(
            stack,
            matches,
            None,
            headers,
            message.as_bstr(),
            diff.as_bstr(),
            None,
        )?;
    }
    Ok(())
}

fn read_gz(source_file: std::fs::File, content: &mut Vec<u8>) -> Result<()> {
    flate2::read::GzDecoder::new(source_file).read_to_end(content)?;
    Ok(())
}

fn read_bz2(source_file: std::fs::File, content: &mut Vec<u8>) -> Result<()> {
    bzip2_rs::DecoderReader::new(source_file).read_to_end(content)?;
    Ok(())
}

fn import_file<'repo>(
    stack: Stack<'repo>,
    matches: &clap::ArgMatches,
    source_path: Option<&Path>,
    strip_level: Option<usize>,
) -> Result<Stack<'repo>> {
    let mut content = Vec::with_capacity(4096);
    if let Some(source_path) = source_path {
        let source_file = std::fs::File::open(source_path)?;
        match source_path.extension().and_then(std::ffi::OsStr::to_str) {
            Some("gz") => read_gz(source_file, &mut content)?,
            Some("bz2") => read_bz2(source_file, &mut content)?,
            _ => {
                let mut source_file = source_file;
                source_file.read_to_end(&mut content)?;
            }
        }
    } else {
        let stdin = std::io::stdin();
        stdin.lock().read_to_end(&mut content)?;
    };

    let (message, diff) = split_patch(content)?;
    let (headers, mut message) = Headers::parse_message(message.as_ref())?;

    if let Some(message_id) = headers
        .message_id
        .as_ref()
        .filter(|_| use_message_id(matches, &stack.repo.config_snapshot()))
    {
        if message.last() != Some(&b'\n') {
            message.push(b'\n');
        }
        if message.len() > 1 && &message[message.len() - 2..] != b"\n\n" {
            message.push(b'\n');
        }
        message.push_str("Message-ID: ");
        message.push_str(message_id);
        message.push(b'\n');
    }

    create_patch(
        stack,
        matches,
        source_path,
        headers,
        message.as_bstr(),
        diff.as_bstr(),
        strip_level,
    )
}

fn create_patch<'repo>(
    stack: Stack<'repo>,
    matches: &clap::ArgMatches,
    source_path: Option<&Path>,
    headers: Headers,
    message: &BStr,
    diff: &BStr,
    strip_level: Option<usize>,
) -> Result<Stack<'repo>> {
    let config = stack.repo.config_snapshot();

    let Headers {
        patchname,
        author_name,
        author_email,
        author_date,
        subject,
        message_id: _message_id,
    } = headers;

    let message = if let Some(mut subject) = subject {
        subject.push_str("\n\n");
        subject.push_str(&message.to_str_lossy());
        subject
    } else {
        message.to_str_lossy().to_string()
    };

    let patchname = if patchname.is_some() {
        patchname.as_deref()
    } else if let Some(name) = matches.get_one::<PatchName>("name") {
        Some(name.as_ref())
    } else if let Some(source_path) = source_path {
        source_path.file_name().and_then(std::ffi::OsStr::to_str)
    } else {
        None
    };

    let patchname = if matches.get_flag("stripname") {
        patchname.map(stripname)
    } else {
        patchname
    };

    let name_len_limit = PatchName::get_length_limit(&config);

    let patchname = if let Some(patchname) = patchname {
        PatchName::make(patchname, false, name_len_limit)
    } else {
        PatchName::make(&message, true, name_len_limit)
    };

    let ignore_flag = matches.get_flag("ignore");
    let replace_flag = matches.get_flag("replace");

    let patchname = if !ignore_flag && !replace_flag {
        let disallow_patchnames: Vec<&PatchName> = stack.all_patches().collect();
        patchname.uniquify(&[], &disallow_patchnames)
    } else if ignore_flag && stack.applied().contains(&patchname) {
        eprintln!("info: ignoring already applied patch `{patchname}`");
        return Ok(stack);
    } else {
        patchname
    };

    let author_date = author_date.and_then(|date| gix::date::Time::parse_time(&date).ok());
    let author = if let (Some(name), Some(email), Some(time)) =
        (author_name.as_deref(), author_email.as_deref(), author_date)
    {
        gix::actor::Signature {
            name: BString::from(name),
            email: BString::from(email),
            time,
        }
    } else {
        let default_author = stack.repo.get_author()?;
        if let (Some(name), Some(email)) = (author_name.as_deref(), author_email.as_deref()) {
            gix::actor::Signature {
                name: BString::from(name),
                email: BString::from(email),
                time: default_author.time,
            }
        } else {
            default_author.to_owned()
        }
    };

    let strip_level = strip_level.or_else(|| matches.get_one::<usize>("strip").copied());

    let trimmed_diff = diff.trim_end_with(|c| c.is_ascii_whitespace());

    let tree_id = if trimmed_diff.is_empty() || trimmed_diff == b"---" {
        stack.get_branch_head().tree_id()?.detach()
    } else {
        let stupid = stack.repo.stupid();
        stupid.apply_to_worktree_and_index(
            diff,
            matches.get_flag("reject"),
            matches.get_flag("3way"),
            strip_level,
            matches
                .get_one::<PathBuf>("directory")
                .map(|path_buf| path_buf.as_path()),
            matches.get_one::<usize>("context-lines").copied(),
        )?;

        stupid.write_tree()?
    };

    let (new_patchname, commit_id) = match crate::patch::edit::EditBuilder::default()
        .original_patchname(Some(&patchname))
        .override_tree_id(tree_id)
        .override_parent_id(stack.get_branch_head().id)
        .default_author(author)
        .default_message(message)
        .allow_autosign(true)
        .allow_implicit_edit(false)
        .allow_diff_edit(true)
        .allow_template_save(false)
        .edit(&stack, stack.repo, matches)?
    {
        patchedit::EditOutcome::TemplateSaved(_) => unreachable!(),
        patchedit::EditOutcome::Edited {
            new_patchname,
            new_commit_id,
        } => (
            new_patchname.unwrap_or(patchname),
            new_commit_id.expect("must have new commit id because no original patch commit"),
        ),
    };

    stack
        .setup_transaction()
        .with_output_stream(get_color_stdout(matches))
        .use_index_and_worktree(false)
        .allow_conflicts(false)
        .transact(|trans| {
            if replace_flag && trans.unapplied().contains(&new_patchname) {
                trans.delete_patches(|pn| pn == &new_patchname)?;
            }
            trans.new_applied(&new_patchname, commit_id)
        })
        .execute(&format!("import: {new_patchname}"))
}

fn stripname(name: &str) -> &str {
    let name = name.trim_start_matches(|c: char| c.is_ascii_digit() || c == '-');
    name.strip_suffix(".diff")
        .or_else(|| name.strip_suffix(".patch"))
        .unwrap_or(name)
}

fn split_patch(content: Vec<u8>) -> Result<(BString, BString)> {
    let mut content = content;
    let mut pos = 0;
    for line in content.lines_with_terminator() {
        if line.starts_with(b"diff -") || line.starts_with(b"Index: ") {
            break;
        } else if let Some(rem) = line.strip_prefix(b"---") {
            if rem.trim_with(|c| c.is_ascii_whitespace()).is_empty()
                || (rem.len() >= 2 && rem[0] == b' ' && !rem[1].is_ascii_whitespace())
            {
                break;
            }
        }

        pos += line.len();
    }

    let diff = content.split_off(pos).into();
    let message = content.into();

    Ok((message, diff))
}

#[cfg(test)]
mod test {
    use bstr::B;

    use super::{split_patch, stripname};

    #[test]
    fn patch_without_message() {
        let patch = B(b"\
        diff --git a/bar.txt b/bar.txt\n\
        new file mode 100644\n\
        index 0000000..ce01362\n\
        --- /dev/null\n\
        +++ b/bar.txt\n\
        @@ -0,0 +1 @@\n\
        +hello\n");

        let content = Vec::from(patch);
        let (message, diff) = split_patch(content).unwrap();
        assert!(message.is_empty());
        assert_eq!(diff, patch);
    }

    #[test]
    fn patch_with_message() {
        let patch = B(b"\
        subject\n\
        \n\
        body\n\
        \n\
        ---  \n\
        \n\
        diff --git a/bar.txt b/bar.txt\n\
        new file mode 100644\n\
        index 0000000..ce01362\n\
        --- /dev/null\n\
        +++ b/bar.txt\n\
        @@ -0,0 +1 @@\n\
        +hello\n");

        let content = Vec::from(patch);
        let (message, diff) = split_patch(content).unwrap();
        assert_eq!(message, "subject\n\nbody\n\n");
        assert!(diff.starts_with(b"---"));
    }

    #[test]
    fn patch_no_diff_separator() {
        let patch = B(b"\
        subject\n\
        \n\
        body\n\
        \n\
        diff --git a/bar.txt b/bar.txt\n\
        new file mode 100644\n\
        index 0000000..ce01362\n\
        --- /dev/null\n\
        +++ b/bar.txt\n\
        @@ -0,0 +1 @@\n\
        +hello\n");

        let content = Vec::from(patch);
        let (message, diff) = split_patch(content).unwrap();
        assert_eq!(message, "subject\n\nbody\n\n");
        assert!(diff.starts_with(b"diff --git"));
    }

    #[test]
    fn just_separator() {
        let patch = B(b"\
        --- filename.txt\n\
        diff --git a/bar.txt b/bar.txt\n\
        new file mode 100644\n\
        index 0000000..ce01362\n\
        --- /dev/null\n\
        +++ b/bar.txt\n\
        @@ -0,0 +1 @@\n\
        +hello\n");

        let content = Vec::from(patch);
        let (message, diff) = split_patch(content).unwrap();
        assert_eq!(message, "");
        assert!(diff.starts_with(b"--- filename.txt"));
    }

    #[test]
    fn patch_name_with_numbered_order_and_patch_extension() {
        let name = String::from("01-patch-name.patch");
        assert_eq!(stripname(&name), "patch-name");
    }

    #[test]
    fn patch_name_with_numbered_order_and_diff_extension() {
        let name = String::from("01-patch-name.diff");
        assert_eq!(stripname(&name), "patch-name");
    }

    #[test]
    fn patch_name_with_numbered_order_and_diff_patch_extension() {
        let name = String::from("01-patch-name.diff.patch");
        assert_eq!(stripname(&name), "patch-name.diff");
    }

    #[test]
    fn patch_name_with_numbered_order_and_patch_diff_extension() {
        let name = String::from("01-patch-name.patch.diff");
        assert_eq!(stripname(&name), "patch-name.patch");
    }
}

#[derive(Default, Debug)]
struct Headers {
    patchname: Option<String>,
    author_name: Option<String>,
    author_email: Option<String>,
    author_date: Option<String>,
    subject: Option<String>,
    message_id: Option<String>,
}

impl Headers {
    fn parse_mailinfo(headers: &BStr) -> Option<Headers> {
        let mut author_name = None;
        let mut author_email = None;
        let mut author_date = None;
        let mut subject = None;

        for line in headers.lines().filter(|line| !line.is_empty()) {
            let mut parts = line.splitn_str(2, b": ");
            let header = parts.next().expect("mailinfo header line has header");
            let value = parts.next().expect("mailinfo header line has value");
            if let Ok(value) = value.to_str() {
                let value = value.to_string();
                match header {
                    b"Author" => author_name = Some(value),
                    b"Email" => author_email = Some(value),
                    b"Date" => author_date = Some(value),
                    b"Subject" => subject = Some(value),
                    _ => panic!(
                        "unexpected mailinfo header \"{}\" with value \"{value}\"",
                        header.to_str_lossy()
                    ),
                }
            }
        }

        if author_name.is_some()
            || author_email.is_some()
            || author_date.is_some()
            || subject.is_some()
        {
            Some(Headers {
                patchname: None,
                author_name,
                author_email,
                author_date,
                subject,
                message_id: None,
            })
        } else {
            None
        }
    }

    fn parse_message(message: &BStr) -> Result<(Headers, BString)> {
        let mut headers = Headers::default();
        let mut dedent = "";
        let mut split_message = BString::from(Vec::with_capacity(message.len()));
        let mut lines = message.lines_with_terminator();

        while let Some(line) = lines
            .next()
            .map(|line| line.trim_with(|c| c.is_ascii_whitespace()).as_bstr())
        {
            if line.is_empty() {
                continue;
            }
            let parts: Vec<_> = line.splitn_str(2, b":").map(BStr::new).collect();
            if parts.len() == 2 {
                let header = parts[0];
                let value = parts[1].trim_start_with(|c| c.is_ascii_whitespace());
                if header.eq_ignore_ascii_case(b"patch") && !value.is_empty() {
                    headers.patchname = Some(
                        value
                            .to_str()
                            .map_err(|_| anyhow!("patch name is not UTF-8"))
                            .context("parsing Patch header")?
                            .to_string(),
                    );
                    continue;
                }

                if header.eq_ignore_ascii_case(b"from") || header.eq_ignore_ascii_case(b"author") {
                    let (name, email) = value
                        .to_str()
                        .map_err(|_| anyhow!("From/Author is not UTF-8"))
                        .and_then(patchedit::parse_name_email)
                        .context("parsing From/Author header")?;
                    headers.author_name = Some(name.to_string());
                    headers.author_email = Some(email.to_string());
                    continue;
                }

                if header.eq_ignore_ascii_case(b"date") {
                    headers.author_date = value.to_str().map(ToString::to_string).ok();
                    continue;
                }

                if header.eq_ignore_ascii_case(b"subject") {
                    headers.subject = value.to_str().map(ToString::to_string).ok();
                    continue;
                }

                if header.eq_ignore_ascii_case(b"message-id") {
                    headers.message_id = value.to_str().map(ToString::to_string).ok();
                    continue;
                }
            }

            if headers.subject.is_some() {
                split_message.push_str(line.strip_prefix(dedent.as_bytes()).unwrap_or(line));
                split_message.push_char('\n');
                break;
            }

            if line
                .strip_prefix(b"commit ")
                .map_or(false, |rest| rest.iter().all(u8::is_ascii_hexdigit))
            {
                // Looks like this patch came from `git show`. Remaining message lines
                // need to be stripped of indentation.
                dedent = "    ";
                continue;
            }

            // Once the subject is determined, more contiguous headers may be parsed.
            headers.subject = Some(
                line.to_str()
                    .map_err(|_| anyhow!("message is not UTF-8"))
                    .context("parsing patch message")?
                    .to_string(),
            );
        }

        for line in lines {
            split_message.push_str(line.strip_prefix(dedent.as_bytes()).unwrap_or(line));
        }

        Ok((headers, split_message))
    }
}
