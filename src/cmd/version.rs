// SPDX-License-Identifier: GPL-2.0-only

//! `stg version` implementation.

use anyhow::Result;
use clap::ArgMatches;

use crate::stupid::StupidContext;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "version",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::Administration,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("version")
        .about("Print version information and exit")
        .arg(
            clap::Arg::new("short")
                .long("short")
                .short('s')
                .help("Show abbreviated version information"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    if matches.contains_id("short") {
        println!("{} {}", env!("CARGO_BIN_NAME"), env!("CARGO_PKG_VERSION"));
    } else {
        println!(
            "Stacked Git {}\n\
             Copyright (C) 2005-2022 StGit authors\n\
             This is free software: you are free to change and redistribute it.\n\
             There is NO WARRANTY, to the extent permitted by law.\n\
             SPDX-License-Identifier: {}",
            env!("CARGO_PKG_VERSION"),
            env!("CARGO_PKG_LICENSE"),
        );
        println!("{}", StupidContext::default().version()?);
    }
    Ok(())
}
