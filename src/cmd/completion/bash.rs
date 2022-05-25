// SPDX-License-Identifier: GPL-2.0-only

use anyhow::Result;

pub(super) fn command() -> clap::Command<'static> {
    clap::Command::new("bash").about("Generate bash completion script")
}

pub(super) fn dispatch(_matches: &clap::ArgMatches) -> Result<()> {
    todo!()
}
