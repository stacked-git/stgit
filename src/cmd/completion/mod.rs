// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion` implementation

mod bash;
mod fish;
mod list;
mod man;
mod shstream;
mod zsh;

use anyhow::Result;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "completion",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::Administration,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("completion")
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
                .help("Output to PATH")
                .global(true)
                .value_name("PATH")
                .value_hint(clap::ValueHint::FilePath)
                .allow_invalid_utf8(true),
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
    Ok(match matches.value_of_os("output") {
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
