use clap::{App, Arg, ArgGroup, ArgSettings, ValueHint};

pub(crate) fn get_subcommand() -> App<'static> {
    App::new("series")
        .about("Print the patch series")
        .long_about(
            "Show all the patches in the series, or just those in the \
             given range, ordered from top to bottom.\n\
             \n\
             The applied patches are prefixed with a '+' (except the \
             current patch, which is prefixed with a '>'), the \
             unapplied patches with a '-', and the hidden patches with \
             a '!'.\n\
             \n\
             Empty patches are prefixed with a '0'.",
        )
        .arg(
            Arg::new("branch")
                .long("branch")
                .short('b')
                .about("Use BRANCH instead of current branch")
                .setting(ArgSettings::TakesValue)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .about("Show all patches, including hidden patches"),
        )
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .about("Show the applied patches only"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .about("Show the unapplied patches only"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .about("Show the hidden patches only"),
        )
        .arg(
            Arg::new("missing")
                .long("missing")
                .short('m')
                .about("Show patches in BRANCH missing in current")
                .setting(ArgSettings::TakesValue)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("count")
                .long("count")
                .short('c')
                .about("Print the number of patches in the series"),
        )
        .arg(
            Arg::new("description")
                .long("description")
                .short('d')
                .about("Show short description for each patch"),
        )
        .arg(
            Arg::new("no-description")
                .long("no-description")
                .about("Disable description"),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .about("Show author name for each patch"),
        )
        .arg(
            Arg::new("empty")
                .long("empty")
                .short('e')
                .about("Show whether patches are empty")
                .long_about(
                    "Before the '+', '>', '-', and '!' prefixes, print \
                     a column that contains either '0' (for empty \
                     patches) or a space (for non-empty patches).",
                ),
        )
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .about("Append the branch name to the listed patches"),
        )
        .arg(
            Arg::new("no-prefix")
                .long("noprefix")
                // TODO: add short option; maybe 'N'
                .about("Do not show the patch status prefix"),
        )
        .arg(
            Arg::new("short")
                .long("short")
                .short('s')
                .about("Only show patches around the topmost patch"),
        )
        .group(ArgGroup::new("description-group").args(&["description", "no-description"]))
        .group(ArgGroup::new("all-short-group").args(&["all", "short"]))
}

pub(crate) fn run() -> super::Result {
    println!("series!");
    Ok(())
}
