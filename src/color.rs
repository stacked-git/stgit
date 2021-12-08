use clap::{Arg, ArgMatches};
use termcolor::{ColorChoice, StandardStream};

lazy_static! {
    pub(crate) static ref COLOR_ARG: Arg<'static> = Arg::new("color")
        .long("color")
        .help("Colorize the output")
        .long_help("Specify WHEN to colorize the output.")
        .value_name("WHEN")
        .possible_values(&["auto", "always", "ansi", "never"])
        .default_value("auto")
        .default_missing_value("always")
        .min_values(0)
        .overrides_with("color");
}

pub(crate) fn get_color_stdout(matches: &ArgMatches) -> StandardStream {
    let color_choice = match matches.value_of("color").unwrap_or("auto") {
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
    };
    StandardStream::stdout(color_choice)
}
