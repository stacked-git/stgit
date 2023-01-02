// SPDX-License-Identifier: GPL-2.0-only

use std::path::PathBuf;

use clap::{
    builder::{self, ValueParser},
    Arg, ValueHint,
};

use crate::{argset, ext::TimeExtended};

use super::parse::{parse_email, parse_name, parse_name_email2};

/// Add patch editing options to a StGit command.
pub(crate) fn add_args(
    command: clap::Command,
    add_message_opts: bool,
    add_save_template: bool,
) -> clap::Command {
    let command = command
        .next_help_heading("Patch Edit Options")
        .arg(
            Arg::new("edit")
                .long("edit")
                .short('e')
                .help("Invoke editor for patch description")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show diff when editing patch description")
                .action(clap::ArgAction::SetTrue),
        );
    let command = if add_message_opts {
        command
            .arg(
                Arg::new("message")
                    .long("message")
                    .short('m')
                    .help("Use message for patch")
                    .long_help("Use message instead of invoking the editor")
                    .value_name("message")
                    .num_args(1)
                    .value_parser(builder::NonEmptyStringValueParser::new())
                    .value_hint(ValueHint::Other)
                    .conflicts_with("file"),
            )
            .arg(
                Arg::new("file")
                    .long("file")
                    .short('f')
                    .help("Get message from file")
                    .long_help(
                        "Use the contents of file instead of invoking the editor. \
                         Use \"-\" to read from stdin.",
                    )
                    .value_name("path")
                    .num_args(1)
                    .value_parser(clap::value_parser!(PathBuf))
                    .value_hint(ValueHint::FilePath),
            )
    } else {
        // These dummy/hidden --message and --file arguments are added to allow the
        // ArgMatches to be dynamically interrogated. If these args weren't defined,
        // then testing their presence, e.g. with ArgMatches.get_one() or
        // ArgMatches.get_flag(), would cause a panic.

        fn no_message(_: &str) -> std::result::Result<(), String> {
            Err("--message is not a valid option for this command".to_string())
        }

        fn no_file(_: &str) -> std::result::Result<(), String> {
            Err("--file is not a valid option for this command".to_string())
        }

        command
            .arg(
                Arg::new("message")
                    .long("message")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("message")
                    .value_parser(ValueParser::new(no_message)),
            )
            .arg(
                Arg::new("file")
                    .long("file")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("path")
                    .num_args(1)
                    .value_parser(no_file),
            )
    };
    let command = command
        .arg(
            Arg::new("no-verify")
                .long("no-verify")
                .help("Disable commit-msg hook")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("signoff")
                .long("signoff")
                .alias("sign")
                .short('s')
                .help("Add Signed-off-by message trailer")
                .long_help(
                    "Add \"Signed-off-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
        )
        .arg(
            Arg::new("ack")
                .long("ack")
                .help("Add Acked-by message trailer")
                .long_help(
                    "Add \"Acked-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer's name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
        )
        .arg(
            Arg::new("review")
                .long("review")
                .help("Add Reviewed-by message trailer")
                .long_help(
                    "Add \"Reviewed-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer's name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
        )
        .arg(
            Arg::new("sign-by")
                .long("sign-by")
                .help("DEPRECATED: use --sign=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("ack-by")
                .long("ack-by")
                .help("DEPRECATED: use --ack=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("review-by")
                .long("review-by")
                .help("DEPRECATED: use --review=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .help("Set the author \"name <email>\"")
                .value_name("name-and-email")
                .num_args(1)
                .value_parser(ValueParser::new(parse_name_email2))
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("authname")
                .long("authname")
                .help("Set the author name")
                .value_name("name")
                .num_args(1)
                .value_hint(ValueHint::Other)
                .value_parser(ValueParser::new(parse_name))
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authemail")
                .long("authemail")
                .help("Set the author email")
                .value_name("email")
                .num_args(1)
                .value_hint(ValueHint::EmailAddress)
                .value_parser(ValueParser::new(parse_email))
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authdate")
                .long("authdate")
                .help("Set the author date")
                .long_help(
                    "Set the date the patch was authored.\n\
                     \n\
                     Use \"now\" to use the current time and date.",
                )
                .value_name("date")
                .num_args(1)
                .value_parser(ValueParser::new(git_repository::actor::Time::parse_time))
                .value_hint(ValueHint::Other),
        )
        .arg(argset::committer_date_is_author_date_arg());
    if add_save_template {
        command.arg(
            Arg::new("save-template")
                .long("save-template")
                .help("Save the patch description to FILE and exit")
                .long_help(
                    "Instead of running the command, just write the patch description \
                     to FILE, and exit. (If FILE is \"-\", write to stdout.)\n\
                     \n\
                     When driving StGit from another program, it may be useful to \
                     first call a command with '--save-template', then let the user \
                     edit the message, and then call the same command with '--file'.",
                )
                .num_args(1)
                .value_name("file")
                .value_hint(ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf))
                .conflicts_with_all(["message", "file"]),
        )
    } else {
        command
    }
}
