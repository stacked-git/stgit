// SPDX-License-Identifier: GPL-2.0-only

//! `stg export` implementation.

use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::OsString,
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Arg;

use crate::{
    commit::CommitExtended,
    patchrange,
    signature::TimeExtended,
    stack::{Error, Stack, StackStateAccess},
    stupid::Stupid,
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    (
        "export",
        StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackInspection,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("export")
        .about("Export patches to a directory")
        .long_about(
            "Export a range of patches to a given directory in unified diff format.\
             All applied patches are exported by default.\n\
             \n\
             Patches are exported to 'patches-<branch>' by default. The --dir option \
             may be used to specify a different output directory.\n\
             \n\
             The patch file output may be customized via a template file found at \
             \"$GIT_DIR/patchexport.tmpl\", \"~/.stgit/templates/patchexport.tmpl\", \
             or \"$(prefix)/share/stgit/templates\". The following variables are \
             supported in the template file:\n\
             \n    %(description)s - patch description\
             \n    %(shortdescr)s  - the first line of the patch description\
             \n    %(longdescr)s   - the rest of the patch description, after the first line\
             \n    %(diffstat)s    - the diff statistics\
             \n    %(authname)s    - author name\
             \n    %(authemail)s   - author email\
             \n    %(authdate)s    - patch creation date (ISO-8601 format)\
             \n    %(commname)s    - committer name\
             \n    %(commemail)s   - committer email",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to export")
                .long_help(
                    "Patches to export.\n\
                     \n\
                     A patch name or patch range of the form \
                     '[begin-patch]..[end-patch]' may be specified.",
                )
                .value_name("patch")
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification)),
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("dir")
                .long("dir")
                .short('d')
                .help("Export patches to <dir> instead of the default")
                .value_name("dir")
                .value_hint(clap::ValueHint::DirPath)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("patch")
                .long("patch")
                .short('p')
                .help("Suffix patch file names with \".patch\""),
        )
        .arg(
            Arg::new("extension")
                .long("extension")
                .short('e')
                .help("Suffix patch file names with \".<ext>\"")
                .conflicts_with("patch")
                .takes_value(true)
                .value_name("ext"),
        )
        .arg(
            Arg::new("numbered")
                .long("numbered")
                .short('n')
                .help("Prefix patch file names with order numbers."),
        )
        .arg(
            Arg::new("template")
                .long("template")
                .short('t')
                .help("Use <file> as template")
                .value_name("file")
                .value_hint(clap::ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("stdout")
                .long("stdout")
                .short('s')
                .help("Export to stdout instead of directory")
                .conflicts_with("dir"),
        )
        .arg(&*crate::argset::DIFF_OPTS_ARG)
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = crate::argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;
    let stupid = repo.stupid();
    let config = repo.config()?;

    if opt_branch.is_none()
        && repo
            .stupid()
            .statuses(None)?
            .check_worktree_clean()
            .is_err()
    {
        crate::print_warning_message(
            matches,
            "Local changes in the tree; you might want to commit them first",
        )
    }

    let patches =
        if let Some(range_specs) = matches.get_many::<patchrange::Specification>("patchranges") {
            patchrange::patches_from_specs(
                range_specs,
                &stack,
                patchrange::Allow::VisibleWithAppliedBoundary,
            )?
        } else {
            stack.applied().to_vec()
        };

    if patches.is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let default_output_dir;
    let output_dir = if let Some(dir) = matches.get_one::<PathBuf>("dir").map(|p| p.as_path()) {
        dir
    } else {
        default_output_dir = format!("patches-{}", stack.branch_name);
        Path::new(default_output_dir.as_str())
    };

    let custom_extension;
    let extension = if let Some(custom_ext) = matches.get_one::<String>("extension") {
        custom_extension = format!(".{custom_ext}");
        custom_extension.as_str()
    } else if matches.contains_id("patch") {
        ".patch"
    } else {
        ""
    };

    let numbered = matches.contains_id("numbered");
    let num_width = std::cmp::max(patches.len().to_string().len(), 2);

    let diff_opts = crate::argset::get_diff_opts(matches, &config, false, true);

    let template = if let Some(template_file) = matches.get_one::<PathBuf>("template") {
        Cow::Owned(std::fs::read_to_string(template_file)?)
    } else {
        match crate::templates::get_template(&repo, "patchexport.tmpl") {
            Ok(Some(template)) => Cow::Owned(template),
            Ok(None) => Cow::Borrowed(crate::templates::PATCHEXPORT_TMPL),
            Err(e) => return Err(e),
        }
    };

    let need_diffstat = template.contains("%(diffstat)");

    let opt_stdout = matches.contains_id("stdout");
    let mut series = format!(
        "# This series applies on Git commit {}\n",
        stack.base().id()
    );

    if !opt_stdout {
        std::fs::create_dir_all(output_dir).with_context(|| format!("creating {output_dir:?}"))?;
    }

    for (i, patchname) in patches.iter().enumerate() {
        let patchfile_name = if numbered {
            let patch_number = i + 1;
            format!("{patch_number:0num_width$}-{patchname}{extension}")
        } else {
            format!("{patchname}{extension}")
        };

        series.push_str(&patchfile_name);
        series.push('\n');

        let patch_commit = stack.get_patch_commit(patchname);
        let parent_commit = patch_commit.parent(0)?;

        let mut replacements: HashMap<&str, Cow<'_, [u8]>> = HashMap::new();
        let message = patch_commit.message_ex();
        let description = message.decode()?;
        let description = description.as_ref();
        let (shortdescr, longdescr) = if let Some((shortdescr, rest)) = description.split_once('\n')
        {
            let longdescr = rest.trim_start_matches('\n').trim_end();
            (shortdescr, longdescr)
        } else {
            (description, "")
        };
        replacements.insert("description", Cow::Borrowed(description.as_bytes()));
        replacements.insert("shortdescr", Cow::Borrowed(shortdescr.as_bytes()));
        replacements.insert("longdescr", Cow::Borrowed(longdescr.as_bytes()));
        let author = patch_commit.author();
        replacements.insert("authname", Cow::Borrowed(author.name_bytes()));
        replacements.insert("authemail", Cow::Borrowed(author.email_bytes()));
        replacements.insert(
            "authdate",
            Cow::Owned(
                author
                    .datetime()
                    .format("%F %T %z")
                    .to_string()
                    .into_bytes(),
            ),
        );
        let committer = patch_commit.committer();
        replacements.insert("commname", Cow::Borrowed(committer.name_bytes()));
        replacements.insert("commemail", Cow::Borrowed(committer.email_bytes()));
        replacements.insert(
            "commdate",
            Cow::Owned(
                committer
                    .datetime()
                    .format("%F %T %z")
                    .to_string()
                    .into_bytes(),
            ),
        );

        let diff = stupid.diff_tree_patch(
            parent_commit.tree_id(),
            patch_commit.tree_id(),
            <Option<Vec<OsString>>>::None,
            false,
            &diff_opts,
        )?;

        if need_diffstat {
            replacements.insert(
                "diffstat",
                if parent_commit.tree_id() == patch_commit.tree_id() {
                    Cow::Borrowed(b"")
                } else {
                    Cow::Owned(stupid.diffstat(&diff)?)
                },
            );
        }

        let specialized = crate::templates::specialize_template(&template, &replacements)?;

        if opt_stdout {
            let stdout = std::io::stdout();
            let mut stdout = stdout.lock();
            if patches.len() > 1 {
                write!(
                    stdout,
                    "{0:->79}\n\
                     {patchfile_name}\n\
                     {0:->79}\n",
                    '-'
                )?;
            }
            stdout.write_all(&specialized)?;
            stdout.write_all(&diff)?;
        } else {
            let mut file = std::fs::File::options()
                .write(true)
                .create(true)
                .open(output_dir.join(&patchfile_name))
                .with_context(|| format!("opening {patchfile_name}"))?;
            file.write_all(&specialized)?;
            file.write_all(&diff)?;
        }
    }

    if !opt_stdout {
        let series_path = output_dir.join("series");
        std::fs::write(&series_path, series.as_str())
            .with_context(|| format!("writing {series_path:?}"))?;
    }

    Ok(())
}
