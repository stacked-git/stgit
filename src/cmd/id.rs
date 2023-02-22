// SPDX-License-Identifier: GPL-2.0-only

//! `stg id` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    ext::RepositoryExtended,
    patch::SingleRevisionSpec,
    stack::{InitializationPolicy, Stack, StackAccess},
    wrap::PartialRefName,
};

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
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(SingleRevisionSpec))
                .help("StGit revision"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(
        &repo,
        matches.get_one::<PartialRefName>("branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    let oid = matches
        .get_one::<SingleRevisionSpec>("stgit-revision")
        .map(|spec| spec.resolve_object(&repo, &stack).map(|object| object.id))
        .transpose()?
        .unwrap_or_else(|| stack.get_branch_head().id);

    println!("{oid}");
    Ok(())
}
