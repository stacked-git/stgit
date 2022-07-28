// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion zsh` implementation

use anyhow::Result;

pub(super) fn command() -> clap::Command<'static> {
    clap::Command::new("zsh")
        .about("Generate zsh completion script")
        .arg(
            clap::Arg::new("output")
                .long("output")
                .short('o')
                .help("Output completion script to <path>")
                .value_name("path")
                .value_hint(clap::ValueHint::FilePath)
                .allow_invalid_utf8(true),
        )
}

pub(super) fn dispatch(matches: &clap::ArgMatches) -> Result<()> {
    let mut stream = super::get_output_stream(matches)?;
    let script = include_str!("../../../completion/stgit.zsh");
    stream.write_all(script.as_bytes())?;
    Ok(())
}
