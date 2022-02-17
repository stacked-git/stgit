use anyhow::Result;
use clap::ArgMatches;

use crate::stack::Stack;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("init", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("init").about("Initialize StGit stack on current branch")
}

fn run(_: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let branch_name = None;
    Stack::initialize(&repo, branch_name)?;
    Ok(())
}
