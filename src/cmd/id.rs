// SPDX-License-Identifier: GPL-2.0-only

//! `stg id` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::revspec::parse_stgit_revision;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("id", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("id")
        .about("Print git hash of a StGit revision")
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
        .arg(
            Arg::new("stgit-revision")
                .value_name("revision")
                .help("StGit revision"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let opt_branch = matches.value_of("branch");
    let opt_spec = matches.value_of("stgit-revision");

    let repo = git2::Repository::open_from_env()?;
    let oid = parse_stgit_revision(&repo, opt_spec, opt_branch)?.id();
    println!("{oid}");
    Ok(())
}
