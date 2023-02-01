// SPDX-License-Identifier: GPL-2.0-only

//! `stg top` implementation.

use std::io::Write;

use anyhow::Result;
use termcolor::WriteColor;

use crate::{
    argset,
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "top",
    category: super::CommandCategory::StackInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Print the name of the top patch")
        .long_about(
            "Print the name of the top patch.\n\
             \n\
             The topmost patch is the currently applied patch. An error \
             message will be printed if no patches are applied.",
        )
        .arg(argset::branch_arg())
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(
        &repo,
        argset::get_one_str(matches, "branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    if let Some(patchname) = stack.applied().last() {
        let mut stdout = crate::color::get_color_stdout(matches);
        let mut color_spec = termcolor::ColorSpec::new();
        color_spec.set_bold(true);
        stdout.set_color(&color_spec)?;
        write!(stdout, "{patchname}")?;
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
        Ok(())
    } else {
        Err(super::Error::NoAppliedPatches.into())
    }
}
