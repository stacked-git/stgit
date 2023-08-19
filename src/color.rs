// SPDX-License-Identifier: GPL-2.0-only

//! Functions for handling `--color` option.

use std::ffi::OsString;

use clap::{Arg, ArgMatches};
use is_terminal::IsTerminal;
use termcolor::StandardStream;

pub(crate) fn get_color_arg() -> Arg {
    Arg::new("color")
        .long("color")
        .help("When to colorize output: auto, always, ansi, never")
        .long_help(
            "Specify when to colorize the output.\n\
             \n\
             `auto` (the default) enables colored output only when outputting to a \
             terminal or TTY. The `NO_COLOR` environment variable is respected.\n\
             \n\
             `always` and `never` unconditionlly enable/disable colored output, \
             respectively.\n\
             \n\
             `ansi` forces color to be output using ANSI escape sequences, even in a \
             Windows console.",
        )
        .hide_default_value(true)
        .hide_possible_values(true)
        .value_name("when")
        .value_parser(["auto", "always", "ansi", "never"])
        .num_args(1)
        .default_value("auto")
}

/// Map color choice string from command line to [`termcolor::ColorChoice`].
fn str_choice_to_termcolor(color_choice: &str) -> Option<termcolor::ColorChoice> {
    match color_choice {
        "always" => Some(termcolor::ColorChoice::Always),
        "ansi" => Some(termcolor::ColorChoice::AlwaysAnsi),
        "auto" => Some(termcolor::ColorChoice::Auto),
        "never" => Some(termcolor::ColorChoice::Never),
        _ => None,
    }
}

/// Map [`termcolor::ColorChoice`] to [`clap::ColorChoice`].
pub(crate) fn termcolor_choice_to_clap(color_choice: termcolor::ColorChoice) -> clap::ColorChoice {
    match color_choice {
        termcolor::ColorChoice::Always | termcolor::ColorChoice::AlwaysAnsi => {
            clap::ColorChoice::Always
        }
        termcolor::ColorChoice::Auto => clap::ColorChoice::Auto,
        termcolor::ColorChoice::Never => clap::ColorChoice::Never,
    }
}

/// Get [`termcolor::StandardStream`] for stdout based on `--color` option.
pub(crate) fn get_color_stdout(matches: &ArgMatches) -> StandardStream {
    let mut choice = get_color_choice(Some(matches));
    if choice == termcolor::ColorChoice::Auto && !std::io::stdout().is_terminal() {
        choice = termcolor::ColorChoice::Never;
    }
    StandardStream::stdout(choice)
}

/// Get [`termcolor::StandardStream`] for stderr based on `--color` option.
pub(crate) fn get_color_stderr(matches: &ArgMatches) -> StandardStream {
    let mut choice = get_color_choice(Some(matches));
    if choice == termcolor::ColorChoice::Auto && !std::io::stderr().is_terminal() {
        choice = termcolor::ColorChoice::Never;
    }
    StandardStream::stderr(choice)
}

/// Get [`termcolor::ColorChoice`] from argument matches.
pub(crate) fn get_color_choice(maybe_matches: Option<&ArgMatches>) -> termcolor::ColorChoice {
    str_choice_to_termcolor(
        maybe_matches
            .and_then(|matches| matches.get_one::<String>("color"))
            .map_or("auto", String::as_str),
    )
    .expect("clap already validated color choice string")
}

/// Determine if color should be used based on `--color` and if terminal is a tty.
pub(crate) fn use_color(matches: &ArgMatches) -> bool {
    match crate::color::get_color_choice(Some(matches)) {
        termcolor::ColorChoice::Always | termcolor::ColorChoice::AlwaysAnsi => true,
        termcolor::ColorChoice::Auto => std::io::stdout().is_terminal(),
        termcolor::ColorChoice::Never => false,
    }
}

/// Parse `argv` for `--color` option.
///
/// This is done outside of [`clap`] in order to be able to setup the [`clap::Command`]
/// to use or not use color based on the option.
///
/// If an invalid value is provided to `--color`, this function will return `None`. It
/// is expected that full-blown command line parsing done by clap will catch and report
/// invalid uses of `--color`.
pub(crate) fn parse_color_choice(argv: &[OsString]) -> Option<termcolor::ColorChoice> {
    let mut choice = None;
    let mut args = argv.iter().map(|osstr| osstr.to_str());
    while let Some(arg) = args.next() {
        if arg == Some("--") {
            break;
        } else if let Some(arg) = arg {
            let choice_str = if arg == "--color" {
                if let Some(Some(next_arg)) = args.next() {
                    next_arg
                } else {
                    continue;
                }
            } else if let Some(("--color", choice_str)) = arg.split_once('=') {
                choice_str
            } else {
                continue;
            };

            if let Some(parsed_choice) = str_choice_to_termcolor(choice_str) {
                choice = Some(parsed_choice);
            }
        }
    }
    choice
}
