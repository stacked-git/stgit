use clap::{App, Arg, ArgMatches};

pub(crate) fn get_subcommand<'help>() -> App<'help> {
    App::new("id")
        .about("Print git hash of StGit reference")
        .long_about("Long about for 'id'.")
        .arg(
            Arg::new("reference")
                .about("StGit reference")
                .default_value("HEAD"),
        )
}

pub(crate) fn run(matches: &ArgMatches) -> super::Result {
    println!("id! {}", matches.value_of("reference").unwrap());
    Ok(())
}
