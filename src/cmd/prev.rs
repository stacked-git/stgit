use std::io::Write;

use clap::App;
use termcolor::WriteColor;

use crate::{
    error::Error,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("prev", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("prev")
        .about("Print the name of the previous patch")
        .long_about(
            "Print the name of the previous patch.\n\
             \n\
             The previous patch is the applied patch preceding the current, \
             topmost patch. An error message will be printed if not enough \
             patches are applied.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
}

fn run(matches: &clap::ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    if let Some(patchname) = stack.applied().iter().nth_back(1) {
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
        Err(Error::NotEnoughPatchesApplied)
    }
}
