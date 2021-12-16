use clap::{App, Arg, ArgMatches};

use crate::revspec::parse_stgit_revision;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("id", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("id")
        .about("Print git hash of StGit revision")
        .long_about(
            "Print the hash (object id) of a StGit revision.\n\
             \n\
             In addition to standard Git revision specifiers (revspecs), \
             patches may be specified in the form '[<branch>:]<patch>' or \
             '[<branch>:]{base}' for the base of a stack. If no branch is \
             specified, the current branch is used by default. The parent \
             of a patch may be specified with '[<branch>:]<patch>^'.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(Arg::new("revision").help("StGit revision"))
}

fn run(matches: &ArgMatches) -> super::Result {
    let opt_branch = matches.value_of("branch");
    let opt_spec = matches.value_of("revision");

    let repo = git2::Repository::open_from_env()?;
    let oid = parse_stgit_revision(&repo, opt_spec, opt_branch)?;
    println!("{}", oid);
    Ok(())
}
