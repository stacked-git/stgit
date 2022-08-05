// SPDX-License-Identifier: GPL-2.0-only

//! `stg patches` implementation.

use std::{
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgMatches, ValueHint};

use crate::{
    argset,
    stack::{Error, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "patches",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackInspection,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("patches")
        .about("Show patches that modify files")
        .long_about(
            "Show the applied patches modifying the given paths. Without path \
             arguments, the files modified in the working tree are used as the \
             paths.",
        )
        .arg(
            Arg::new("pathspecs")
                .help("Show patches that modify these paths")
                .value_name("path")
                .multiple_values(true)
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(ValueHint::AnyPath),
        )
        .arg(&*argset::BRANCH_ARG)
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show the diff for the given paths"),
        )
        .arg(&*argset::DIFF_OPTS_ARG)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;
    let opt_diff = matches.contains_id("diff");

    if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let stupid = repo.stupid();

    let pathsbuf;
    let pathspecs: Vec<&Path> = if let Some(pathspecs) = matches.get_many::<PathBuf>("pathspecs") {
        pathspecs.map(|p| p.as_path()).collect()
    } else {
        let curdir = std::env::current_dir()?;
        let workdir = repo.workdir().expect("not a bare repository");
        let prefix = curdir
            .strip_prefix(workdir)
            .context("determining Git prefix")?;

        let mut paths: Vec<&Path> = Vec::new();
        pathsbuf = stupid
            .diff_index_names(stack.branch_head.tree_id(), Some(prefix))
            .context("getting modified files")?;

        for path_bytes in pathsbuf.split_str(b"\0") {
            if !path_bytes.is_empty() {
                let path = Path::new(
                    path_bytes
                        .to_os_str()
                        .context("getting modified file list")?,
                );
                paths.push(path);
            }
        }
        paths
    };

    if pathspecs.is_empty() {
        return Err(anyhow!("No local changes and no paths specified"));
    }

    let revs = stupid.rev_list(stack.base().id(), stack.top().id(), Some(&pathspecs))?;

    if opt_diff {
        // TODO: pager?
        let config = repo.config()?;
        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();
        let diff_opts = argset::get_diff_opts(matches, &config, false, false);
        for patchname in stack.applied() {
            let patch_commit = stack.get_patch_commit(patchname);
            let parent_commit = patch_commit.parent(0)?;
            if revs.contains(&patch_commit.id()) {
                write!(
                    stdout,
                    "--------------------------------------------------\n\
                     {patchname}\n\
                     --------------------------------------------------\n"
                )?;
                stdout.write_all(patch_commit.message_raw_bytes())?;
                write!(stdout, "\n---\n")?;
                let diff = stupid.diff_tree_patch(
                    parent_commit.tree_id(),
                    patch_commit.tree_id(),
                    Some(&pathspecs),
                    crate::color::use_color(matches),
                    &diff_opts,
                )?;
                stdout.write_all(&diff)?;
            }
        }
    } else {
        for patchname in stack.applied() {
            let patch_commit = stack.get_patch_commit(patchname);
            if revs.contains(&patch_commit.id()) {
                println!("{patchname}");
            }
        }
    }

    Ok(())
}
