use anyhow::Result;
use clap::ArgMatches;

use crate::stupid::StupidContext;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("version", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("version").about("Print version information and exit")
}

fn run(_: &ArgMatches) -> Result<()> {
    println!("Stacked Git {}", env!("CARGO_PKG_VERSION"));
    println!("{}", StupidContext::default().version()?);
    Ok(())
}
