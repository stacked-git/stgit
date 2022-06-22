// SPDX-License-Identifier: GPL-2.0-only

//! `stg email` implementation.

mod format;
mod send;

use anyhow::Result;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("email", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("email")
        .about("Format and send patches as email")
        .long_about(
            "Format and send patches as email.\n\
             \n\
             A typical workflow is to first generate email files for each patch along \
             with an optional cover letter using `stg email format`. Then, after \
             checking the email files' contents, sending the emails using `stg email \
             send`. This workflow may be condensed to one step by specifying patch \
             names to `stg email send` instead of email files.\n\
             \n\
             The `format` and `send` subcommands are thin wrappers over `git \
             format-patch` and `git send-email`, respectively. Refer to the \
             git-format-patch(1) and git-send-email(1) manpages for more details about \
             configuration and options.",
        )
        .subcommand_required(true)
        .subcommand(format::command())
        .subcommand(send::command())
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("format", sub_matches)) => format::dispatch(sub_matches),
        Some(("send", sub_matches)) => send::dispatch(sub_matches),
        _ => panic!("valid subcommand is expected"),
    }
}
