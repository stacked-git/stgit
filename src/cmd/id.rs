use clap::{App, Arg, ArgMatches, ArgSettings, ValueHint};
use git2::Repository;

use crate::revspec::parse_stgit_revision;

pub(crate) fn get_subcommand<'help>() -> App<'help> {
    App::new("id")
        .about("Print git hash of StGit revision")
        .long_about("Long about for 'id'.") // TODO
        .arg(
            Arg::new("branch")
                .long("branch")
                .short('b')
                .about("Use BRANCH instead of current branch")
                .setting(ArgSettings::TakesValue)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(Arg::new("revision").about("StGit revision"))
}

pub(crate) fn run(matches: &ArgMatches) -> super::Result {
    let opt_branch = matches.value_of("branch");
    let opt_spec = matches.value_of("revision");

    let repo = Repository::open_from_env()?;
    let oid = parse_stgit_revision(&repo, opt_spec, opt_branch)?;
    println!("{}", oid.to_string());
    Ok(())
}
