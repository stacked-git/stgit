// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion` implementation

mod bash;
mod fish;
mod list;
mod man;
mod shstream;
mod zsh;

use std::path::PathBuf;

use anyhow::Result;

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "completion",
    category: super::CommandCategory::Administration,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Support for shell completions")
        .long_about(
            "Support completions for bash, fish, and zsh. Also provides 'stg \
             completion list' command for dynamically introspecting StGit's \
             commands and aliases.",
        )
        .subcommand_required(true)
        .subcommand(bash::command())
        .subcommand(fish::command())
        .subcommand(zsh::command())
        .subcommand(list::command())
        .subcommand(man::command())
        .arg(
            clap::Arg::new("output")
                .long("output")
                .short('o')
                .help("Output to <path>")
                .global(true)
                .value_name("path")
                .value_hint(clap::ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf)),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("bash", sub_matches)) => bash::dispatch(sub_matches),
        Some(("fish", sub_matches)) => fish::dispatch(sub_matches),
        Some(("zsh", sub_matches)) => zsh::dispatch(sub_matches),
        Some(("list", sub_matches)) => list::dispatch(sub_matches),
        Some(("man", sub_matches)) => man::dispatch(sub_matches),
        _ => panic!("valid subcommand is required"),
    }
}

pub(self) fn get_output_stream(matches: &clap::ArgMatches) -> Result<Box<dyn std::io::Write>> {
    Ok(match matches.get_one::<PathBuf>("output") {
        Some(path) => Box::new(
            std::fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(path)?,
        ) as Box<dyn std::io::Write>,
        None => Box::new(std::io::stdout()),
    })
}
