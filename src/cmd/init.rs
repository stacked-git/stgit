use clap::{App, ArgMatches};
use git2::Repository;

use crate::stack::Stack;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("init", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("init").about("Initialize StGit stack on current branch")
}

fn run(_: &ArgMatches) -> super::Result {
    let repo = Repository::open_from_env()?;
    let branch_name = None;
    Stack::initialize(&repo, branch_name)?;
    Ok(())
}
