use clap::{Arg, ArgSettings, ValueHint};

lazy_static! {
    pub(crate) static ref BRANCH_ARG: Arg<'static> = Arg::new("branch")
        .long("branch")
        .short('b')
        .help("Use BRANCH instead of current branch")
        .setting(ArgSettings::TakesValue)
        .value_name("BRANCH")
        .value_hint(ValueHint::Other);
    pub(crate) static ref HOOK_ARG: Arg<'static> = Arg::new("no-verify")
        .long("no-verify")
        .help("Disable commit-msg hook");
    pub(crate) static ref KEEP_ARG: Arg<'static> =
        Arg::new("keep").long("keep").help("Keep the local changes");
    pub(crate) static ref MERGED_ARG: Arg<'static> = Arg::new("merged")
        .long("merged")
        .short('m')
        .help("Check for patches merged upstream");
    pub(crate) static ref DIFF_OPTS_ARG: Arg<'static> = Arg::new("diff-opts")
        .long("diff-opts")
        .short('O')
        .help("Extra options to pass to \"git diff\"")
        .setting(ArgSettings::TakesValue)
        .setting(ArgSettings::AllowHyphenValues)
        .value_name("OPTIONS")
        .value_hint(ValueHint::Other);
}
