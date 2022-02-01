use anyhow::Result;
use clap::{App, ArgMatches};

use crate::stupid;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("version", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("version").about("Print version information and exit")
}

fn run(_: &ArgMatches) -> Result<()> {
    println!("Stacked Git {}", env!("CARGO_PKG_VERSION"));
    println!("{}", stupid::version()?);
    Ok(())
}
