// SPDX-License-Identifier: GPL-2.0-only

//! `stg id` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{argset, ext::RepositoryExtended, revspec::parse_stgit_revision};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "id",
    category: super::CommandCategory::PatchInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
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
        .arg(argset::branch_arg())
        .arg(
            Arg::new("stgit-revision")
                .value_name("revision")
                .value_parser(clap::value_parser!(String))
                .help("StGit revision"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let opt_branch = argset::get_one_str(matches, "branch");
    let opt_spec = argset::get_one_str(matches, "stgit-revision");

    let repo = gix::Repository::open()?;
    let oid = parse_stgit_revision(&repo, opt_spec, opt_branch)?.id();
    println!("{oid}");
    Ok(())
}
