use clap::{Arg, ArgMatches};
use termcolor::{ColorChoice, StandardStream};

pub(crate) fn get_color_arg() -> Arg<'static> {
    Arg::new("color")
        .long("color")
        .help("When to colorize output: auto, always, ansi, never")
        .long_help(
            "Specify WHEN to colorize the output.\n\
             \n\
             'auto' (the default) enables colored output only when \
             outputting to a terminal or TTY. The NO_COLOR environment \
             variable is respected.\n\
             \n\
             'always' and 'never' unconditionlly enable/disable \
             colored output, respectively.\n\
             \n\
             'ansi' forces color to be output using ANSI escape sequences, \
             even in a Windows console.",
        )
        .hide_default_value(true)
        .hide_possible_values(true)
        .value_name("WHEN")
        .possible_values(&["auto", "always", "ansi", "never"])
        .takes_value(true)
        .default_value("auto")
        .overrides_with("color")
}

pub(crate) fn get_color_stdout(matches: &ArgMatches) -> StandardStream {
    StandardStream::stdout(get_color_choice(matches))
}

pub(crate) fn get_color_choice(matches: &ArgMatches) -> ColorChoice {
    match matches.value_of("color").unwrap_or("auto") {
        "always" => ColorChoice::Always,
        "ansi" => ColorChoice::AlwaysAnsi,
        "auto" => {
            if atty::is(atty::Stream::Stdout) {
                ColorChoice::Auto
            } else {
                ColorChoice::Never
            }
        }
        _ => ColorChoice::Never,
    }
}
