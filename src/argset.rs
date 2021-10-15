use clap::{Arg, ArgSettings, ValueHint};

lazy_static! {
    pub(crate) static ref AUTHOR_ARGS: [Arg<'static>; 4] = [
        Arg::new("author")
            .long("author")
            .about("Set the author \"NAME <EMAIL>\"")
            .setting(ArgSettings::TakesValue)
            .value_name("NAME_AND_EMAIL")
            .value_hint(ValueHint::Other),
        Arg::new("authname")
            .long("authname")
            .about("Set the author name")
            .setting(ArgSettings::TakesValue)
            .value_name("NAME")
            .value_hint(ValueHint::Other),
        Arg::new("authemail")
            .long("authemail")
            .about("Set the author email")
            .setting(ArgSettings::TakesValue)
            .value_name("EMAIL")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("authdate")
            .long("authdate")
            .about("Set the author date")
            .long_about(
                "Set the DATE the patch was authored.\n\
                 \n\
                 Use \"now\" to use the current time and date."
            )
            .setting(ArgSettings::TakesValue)
            .value_name("DATE")
            .value_hint(ValueHint::Other),
    ];
    pub(crate) static ref TRAILER_ARGS: [Arg<'static>; 6] = [
        Arg::new("sign")
            .long("sign")
            .about("Add \"Signed-off-by:\" trailer")
            .long_about("Long about for --sign"),
        Arg::new("sign-by")
            .long("sign-by")
            .about("Add \"Signed-off-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("ack")
            .long("ack")
            .about("Add \"Acked-by:\" trailer")
            .long_about("Long about for --ack"),
        Arg::new("ack-by")
            .long("ack-by")
            .about("Add \"Acked-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("review")
            .long("review")
            .about("Add \"Reviewed-by:\" trailer")
            .long_about("Long about for --review"),
        Arg::new("review-by")
            .long("review-by")
            .about("Add \"Reviewed-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
    ];
    pub(crate) static ref MESSAGE_ARGS: [Arg<'static>; 2] = [
        Arg::new("message")
            .long("message")
            .short('m')
            .about("Use MESSAGE for patch")
            .long_about("Use MESSAGE instead of invoking the editor")
            .setting(ArgSettings::TakesValue)
            .value_name("MESSAGE")
            .value_hint(ValueHint::Other),
        Arg::new("file")
            .long("file")
            .short('f')
            .about("Get message from FILE")
            .long_about("Use the contents of FILE instead of invoking the editor")
            .setting(ArgSettings::TakesValue)
            .value_name("FILE")
            .value_hint(ValueHint::FilePath),
    ];
    pub(crate) static ref MESSAGE_TEMPLATE_ARG: Arg<'static> = Arg::new("save-template")
        .long("save-template")
        .about("Save the message template to FILE and exit")
        .long_about(
            "Instead of running the command, just write the message template \
             to FILE, and exit. (If FILE is \"-\", write to stdout.)\n\
             \n\
             When driving StGit from another program, it is often useful to \
             first call a command with '--save-template', then let the user \
             edit the message, and then call the same command with '--file'."
        )
        .setting(ArgSettings::TakesValue)
        .value_name("FILE")
        .value_hint(ValueHint::FilePath);
    pub(crate) static ref EDIT_DIFF_ARGS: [Arg<'static>; 2] = [
        Arg::new("edit")
            .long("edit")
            .short('e')
            .about("Invoke editor for patch description"),
        Arg::new("diff")
            .long("diff")
            .short('d')
            .about("Show diff when editing patch description"),
    ];
    pub(crate) static ref HOOK_ARG: Arg<'static> = Arg::new("no-verify")
        .long("no-verify")
        .about("Disable commit-msg hook");
    pub(crate) static ref KEEP_ARG: Arg<'static> = Arg::new("keep")
        .long("keep")
        .about("Keep the local changes");
    pub(crate) static ref MERGED_ARG: Arg<'static> = Arg::new("merged")
        .long("merged")
        .short('m')
        .about("Check for patches merged upstream");
    pub(crate) static ref DIFF_OPTS_ARG: Arg<'static> = Arg::new("diff-opts")
        .long("diff-opts")
        .short('O')
        .about("Extra options to pass to \"git diff\"")
        .setting(ArgSettings::TakesValue)
        .value_name("OPTIONS")
        .value_hint(ValueHint::Other);
    pub(crate) static ref COLOR_ARG: Arg<'static> = Arg::new("color")
        .long("color")
        .about("Colorize the output")
        .long_about("Specify WHEN to colorize the output.")
        .value_name("WHEN")
        .possible_values(&["auto", "always", "ansi", "never"])
        .default_value("auto")
        .default_missing_value("always")
        .min_values(0)
        .overrides_with("color");
}
