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
use bstr::BStr;
use clap::Arg;

use crate::{
    argset,
    branchloc::BranchLocator,
    ext::{CommitExtended, RepositoryExtended},
    patch::{patchrange, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "export",
    category: super::CommandCategory::StackInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Export patches to a directory")
        .long_about(
            "Export a range of patches to a given directory in unified diff format. \
             All applied patches are exported by default.\n\
             \n\
             Patches are exported to 'patches-<branch>' by default. The '--dir' option \
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
                .num_args(1..)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchRange)),
        )
        .arg(argset::branch_arg())
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
                .help("Suffix patch file names with \".patch\"")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("extension")
                .long("extension")
                .short('e')
                .help("Suffix patch file names with \".<ext>\"")
                .conflicts_with("patch")
                .num_args(1)
                .value_name("ext"),
        )
        .arg(
            Arg::new("numbered")
                .long("numbered")
                .short('n')
                .help("Prefix patch file names with order numbers.")
                .action(clap::ArgAction::SetTrue),
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
                .conflicts_with("dir")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::diff_opts_arg())
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let opt_branch = matches.get_one::<BranchLocator>("branch");
    let stack =
        Stack::from_branch_locator(&repo, opt_branch, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

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
        );
    }

    let patches = if let Some(range_specs) = matches.get_many::<PatchRange>("patchranges") {
        patchrange::resolve_names(
            &stack,
            range_specs,
            RangeConstraint::VisibleWithAppliedBoundary,
        )?
    } else {
        stack.applied().to_vec()
    };

    if patches.is_empty() {
        return Err(super::Error::NoAppliedPatches.into());
    }

    let default_output_dir;
    let output_dir = if let Some(dir) = matches.get_one::<PathBuf>("dir").map(PathBuf::as_path) {
        dir
    } else {
        default_output_dir = format!("patches-{}", stack.get_branch_name());
        Path::new(default_output_dir.as_str())
    };

    let custom_extension;
    let extension = if let Some(custom_ext) = matches.get_one::<String>("extension") {
        custom_extension = format!(".{custom_ext}");
        custom_extension.as_str()
    } else if matches.get_flag("patch") {
        ".patch"
    } else {
        ""
    };

    let numbered_flag = matches.get_flag("numbered");
    let num_width = std::cmp::max(patches.len().to_string().len(), 2);

    let diff_opts = argset::get_diff_opts(matches, &repo.config_snapshot(), false, true);

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

    let stdout_flag = matches.get_flag("stdout");
    let mut series = format!(
        "# This series applies on Git commit {}\n",
        stack.base().id()
    );

    if !stdout_flag {
        std::fs::create_dir_all(output_dir).with_context(|| format!("creating {output_dir:?}"))?;
    }

    for (i, patchname) in patches.iter().enumerate() {
        let patchfile_name = if numbered_flag {
            let patch_number = i + 1;
            format!("{patch_number:0num_width$}-{patchname}{extension}")
        } else {
            format!("{patchname}{extension}")
        };

        series.push_str(&patchfile_name);
        series.push('\n');

        let patch_commit = stack.get_patch_commit(patchname);
        let parent_commit = patch_commit.get_parent_commit()?;

        let mut replacements: HashMap<&str, Cow<'_, BStr>> = HashMap::new();
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
        replacements.insert("description", Cow::Borrowed(description.into()));
        replacements.insert("shortdescr", Cow::Borrowed(shortdescr.into()));
        replacements.insert("longdescr", Cow::Borrowed(longdescr.into()));
        let author = patch_commit.author()?;
        replacements.insert("authname", Cow::Borrowed(author.name));
        replacements.insert("authemail", Cow::Borrowed(author.email));
        replacements.insert(
            "authdate",
            Cow::Owned(
                author
                    .time()?
                    .format(gix::date::time::format::ISO8601)
                    .into(),
            ),
        );
        let committer = patch_commit.committer()?;
        replacements.insert("commname", Cow::Borrowed(committer.name));
        replacements.insert("commemail", Cow::Borrowed(committer.email));
        replacements.insert(
            "commdate",
            Cow::Owned(
                committer
                    .time()?
                    .format(gix::date::time::format::ISO8601)
                    .into(),
            ),
        );

        let diff = stupid.diff_tree_patch(
            parent_commit.tree_id()?.detach(),
            patch_commit.tree_id()?.detach(),
            <Option<Vec<OsString>>>::None,
            false,
            diff_opts.iter(),
        )?;

        if need_diffstat {
            replacements.insert(
                "diffstat",
                if parent_commit.tree_id()? == patch_commit.tree_id()? {
                    Cow::Borrowed("".into())
                } else {
                    Cow::Owned(stupid.diffstat(diff.as_ref())?)
                },
            );
        }

        let specialized = crate::templates::specialize_template(&template, &replacements);

        if stdout_flag {
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
                .truncate(true)
                .open(output_dir.join(&patchfile_name))
                .with_context(|| format!("opening {patchfile_name}"))?;
            file.write_all(&specialized)?;
            file.write_all(&diff)?;
        }
    }

    if !stdout_flag {
        let series_path = output_dir.join("series");
        std::fs::write(&series_path, series.as_str())
            .with_context(|| format!("writing {series_path:?}"))?;
    }

    Ok(())
}
