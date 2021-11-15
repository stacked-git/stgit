use std::io::Write;

use clap::App;
use termcolor::WriteColor;

use crate::wrap::Repository;
use crate::{error::Error, stack::Stack};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("top", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("top")
        .about("Print the name of the top patch")
        .long_about(
            "Print the name of the top patch.\n\
             \n\
             The topmost patch is the currently applied patch. An error \
             message will be printed if no patches are applied.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(&*crate::color::COLOR_ARG)
}

fn run(matches: &clap::ArgMatches) -> super::Result {
    let repo = Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    if let Some(patchname) = stack.state.applied.last() {
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
        Err(Error::NoAppliedPatches)
    }
}
