// SPDX-License-Identifier: GPL-2.0-only

//! `stg name` implementation.

use std::io::Write;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};
use termcolor::WriteColor;

use crate::{
    argset,
    branchloc::BranchLocator,
    ext::RepositoryExtended,
    patch::SingleRevisionSpec,
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "name",
    category: super::CommandCategory::PatchInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Print patch name of a StGit revision")
        .long_about(
            "Print the patch name of a StGit revision.\n\
             \n\
             Try to get the name of the patch in the current \
             branch as specified by a StGit revision. Revisions \
             can be specified in the all the forms accepted by \
             \"stg id\" command.",
        )
        .arg(argset::branch_arg())
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .help("Display the branch name with the patch")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-show-branch")
                .long("no-showbranch")
                .help("Do not display branch name")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("show-branch"),
        )
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

    let stack = Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::RequireInitialized,
    )?;

    let oid = matches
        .get_one::<SingleRevisionSpec>("stgit-revision")
        .map(|spec| spec.resolve_object(&repo, &stack).map(|object| object.id))
        .transpose()?
        .unwrap_or_else(|| stack.get_branch_head().id);

    let Some(patch_name) = (oid == stack.base().id).then_some("{base}").or_else(|| {
        stack
            .all_patches()
            .find(|name| oid == stack.get_patch_commit_id(name))
            .map(|name| name.as_ref())
    }) else {
        return Err(anyhow!("patch name not found for revision `{oid}`"));
    };

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();
    color_spec.set_bold(true);
    stdout.set_color(&color_spec)?;

    if matches.get_flag("show-branch") {
        write!(stdout, "{}:", stack.get_branch_name())?;
    }

    write!(stdout, "{patch_name}")?;
    color_spec.clear();
    stdout.set_color(&color_spec)?;
    writeln!(stdout)?;

    Ok(())
}
