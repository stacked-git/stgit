// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion` implementation

mod bash;

use anyhow::Result;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("completion", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("completion")
        .about("Support for shell completions")
        .subcommand_required(true)
        .subcommand(bash::command())
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("bash", sub_matches)) => bash::dispatch(sub_matches),
        _ => panic!("valid subcommand is required"),
    }
}
