// SPDX-License-Identifier: GPL-2.0-only

//! `stg next` implementation.

use std::io::Write;

use anyhow::{anyhow, Result};
use termcolor::WriteColor;

use crate::{
    argset,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    (
        "next",
        StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackInspection,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("next")
        .about("Print the name of the next patch")
        .long_about(
            "Print the name of the next patch.\n\
             \n\
             The next patch is the unapplied patch that follows the current, \
             topmost patch. An error message will be printed if there are no \
             unapplied patches.",
        )
        .arg(&*argset::BRANCH_ARG)
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    if let Some(patchname) = stack.unapplied().first() {
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
        Err(anyhow!("No unapplied patches"))
    }
}
