// SPDX-License-Identifier: GPL-2.0-only

//! `stg version` implementation.

use anyhow::Result;
use clap::ArgMatches;

use crate::stupid::StupidContext;

pub(crate) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "version",
    category: super::CommandCategory::Administration,
    make,
    run,
};

fn make() -> clap::Command<'static> {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Print version information and exit")
        .arg(
            clap::Arg::new("short")
                .long("short")
                .short('s')
                .help("Show abbreviated version information"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let pkg_version = env!("CARGO_PKG_VERSION");
    let hash_suffix = option_env!("STGIT_BUILD_GIT_HASH")
        .and_then(|rev_hash| Some(format!(" ({rev_hash})")))
        .unwrap_or_default();
    if matches.contains_id("short") {
        let bin_name = env!("CARGO_BIN_NAME");
        println!("{bin_name} {pkg_version}{hash_suffix}");
    } else {
        let license_id = env!("CARGO_PKG_LICENSE");
        println!(
            "Stacked Git {pkg_version}{hash_suffix}\n\
             Copyright (C) 2005-2022 StGit authors\n\
             This is free software: you are free to change and redistribute it.\n\
             There is NO WARRANTY, to the extent permitted by law.\n\
             SPDX-License-Identifier: {license_id}",
        );
        println!("{}", StupidContext::default().version()?);
    }
    Ok(())
}
