//! `stg top` implementation.

use std::io::Write;

use anyhow::Result;
use termcolor::WriteColor;

use crate::stack::{Error, Stack, StackStateAccess};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("top", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("top")
        .about("Print the name of the top patch")
        .long_about(
            "Print the name of the top patch.\n\
             \n\
             The topmost patch is the currently applied patch. An error \
             message will be printed if no patches are applied.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    if let Some(patchname) = stack.applied().last() {
        let mut stdout = crate::color::get_color_stdout(matches);
        let mut color_spec = termcolor::ColorSpec::new();
        color_spec.set_bold(true);
        stdout.set_color(&color_spec)?;
        write!(stdout, "{}", patchname)?;
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
        Ok(())
    } else {
        Err(Error::NoAppliedPatches.into())
    }
}
