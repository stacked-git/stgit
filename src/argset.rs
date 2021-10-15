use clap::{Arg, ArgSettings, ValueHint};

lazy_static! {
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
