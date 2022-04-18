//! `stg patches` implementation.

use std::ffi::OsStr;
use std::io::Write;

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgMatches, ValueHint};

use crate::{
    stack::{Error, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("patches", super::StGitCommand { make, run })
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
                .allow_invalid_utf8(true)
                .forbid_empty_values(true)
                .value_hint(ValueHint::AnyPath),
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show the diff for the given paths"),
        )
        .arg(&*crate::argset::DIFF_OPTS_ARG)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;
    let opt_diff = matches.is_present("diff");

    if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    }

    let stupid = repo.stupid();

    let pathsbuf;
    let pathspecs: Vec<&OsStr> = if let Some(pathspecs) = matches.values_of_os("pathspecs") {
        pathspecs.collect()
    } else {
        let curdir = std::env::current_dir()?;
        let workdir = repo.workdir().expect("not a bare repository");
        let prefix = curdir
            .strip_prefix(workdir)
            .context("determining Git prefix")?;

        let mut paths: Vec<&OsStr> = Vec::new();
        pathsbuf = stupid
            .diff_index_names(stack.branch_head.tree_id(), Some(prefix))
            .context("getting modified files")?;

        for path_bytes in pathsbuf.split_str(b"\0") {
            if !path_bytes.is_empty() {
                let path = path_bytes
                    .to_os_str()
                    .context("getting modified file list")?;
                paths.push(path);
            }
        }
        paths
    };

    if pathspecs.is_empty() {
        return Err(anyhow!("no local changes and no paths specified"));
    }

    let revs = stupid.rev_list(stack.base().id(), stack.top().id(), Some(&pathspecs))?;

    if opt_diff {
        // TODO: pager?
        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();
        let use_color = match crate::color::get_color_choice(Some(matches)) {
            termcolor::ColorChoice::Always => true,
            termcolor::ColorChoice::AlwaysAnsi => true,
            termcolor::ColorChoice::Auto => atty::is(atty::Stream::Stdout),
            termcolor::ColorChoice::Never => false,
        };
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
                    false,
                    use_color,
                    matches.value_of("diff-opts"),
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
