// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch` implementation.

mod cleanup;
mod clone;
mod create;
mod delete;
mod describe;
mod list;
mod protect;
mod rename;
mod reset;
mod unprotect;

use anyhow::Result;
use bstr::ByteSlice;

use crate::{
    branchloc::BranchLocator, ext::RepositoryExtended, stupid::Stupid, wrap::PartialRefName,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "branch",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Branch operations: switch, list, create, rename, delete, ...")
        .long_about(
            "Create, clone, switch, rename, or delete StGit-enabled branches.\n\
             \n\
             With no arguments, the current branch is printed to stdout.\n\
             \n\
             With a single argument, switch to the named branch.\n\
             \n\
             StGit supports specifying a branch using the `@{-<n>}` syntax supported \
             by git, including `-` as a synonym for `@{-1}`. Thus `stg branch -` may \
             be used to switch to the last checked-out HEAD. Note that `@{-<n>}` \
             refers to the <n>th last HEAD, which is not necessarily a local branch. \
             Using an `@{-<n>}` value that refers to anything but a local branch will \
             result in an error.",
        )
        .disable_help_subcommand(true)
        .args_conflicts_with_subcommands(true)
        .override_usage(super::make_usage(
            "stg branch",
            &[
                "",
                "[--merge] <branch>",
                "{--list,-l}",
                "{--create,-c} <new-branch> [committish]",
                "{--clone,-C} [new-branch]",
                "{--rename,-r} [old-name] <new-name>",
                "{--protect,-p} [branch]",
                "{--unprotect,-u} [branch]",
                "{--delete,-D} [--force] [branch]",
                "--cleanup [--force] [branch]",
                "{--describe,-d} <description> [branch]",
                "--reset [branch]",
            ],
        ))
        .subcommand(self::list::command())
        .subcommand(self::create::command())
        .subcommand(self::clone::command())
        .subcommand(self::rename::command())
        .subcommand(self::protect::command())
        .subcommand(self::unprotect::command())
        .subcommand(self::delete::command())
        .subcommand(self::cleanup::command())
        .subcommand(self::describe::command())
        .subcommand(self::reset::command())
        .arg(
            clap::Arg::new("merge")
                .long("merge")
                .help("Merge work tree changes into the other branch")
                .action(clap::ArgAction::SetTrue)
                .requires("branch-any"),
        )
        .arg(
            clap::Arg::new("branch-any")
                .help("Branch to switch to")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    if let Some((subname, submatches)) = matches.subcommand() {
        match subname {
            "--list" => self::list::dispatch(&repo, submatches),
            "--create" => self::create::dispatch(&repo, submatches),
            "--clone" => self::clone::dispatch(&repo, submatches),
            "--rename" => self::rename::dispatch(&repo, submatches),
            "--protect" => self::protect::dispatch(&repo, submatches),
            "--unprotect" => self::unprotect::dispatch(&repo, submatches),
            "--delete" => self::delete::dispatch(&repo, submatches),
            "--cleanup" => self::cleanup::dispatch(&repo, submatches),
            "--describe" => self::describe::dispatch(&repo, submatches),
            "--reset" => self::reset::dispatch(&repo, submatches),
            s => panic!("unhandled branch subcommand {s}"),
        }
    } else if let Some(target_branch_loc) = matches.get_one::<BranchLocator>("branch-any") {
        switch(&repo, matches, target_branch_loc)
    } else if let Ok(branch) = repo.get_current_branch() {
        println!("{}", branch.get_branch_name()?);
        Ok(())
    } else {
        // Print nothing if HEAD is detached; same as `git branch --show-current`.
        Ok(())
    }
}

fn switch(
    repo: &gix::Repository,
    matches: &clap::ArgMatches,
    target_branch_loc: &BranchLocator,
) -> Result<()> {
    let current_branch = repo.get_current_branch().ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_partial_name().ok());
    let target_branch = target_branch_loc.resolve(repo)?;
    let target_branchname = target_branch.get_branch_partial_name()?;

    if Some(&target_branchname) == current_branchname.as_ref() {
        return Err(anyhow::anyhow!(
            "{target_branchname} is already the current branch"
        ));
    }

    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    if !matches.get_flag("merge") {
        statuses.check_worktree_clean()?;
    }
    statuses.check_conflicts()?;
    stupid.checkout(target_branchname.as_ref())
}

fn set_description(
    repo: &gix::Repository,
    branchname: &PartialRefName,
    description: &str,
) -> Result<()> {
    let mut local_config_file = repo.local_config_file()?;
    if description.is_empty() {
        if let Ok(mut value) =
            local_config_file.raw_value_mut_by("branch", Some(branchname.into()), "description")
        {
            value.delete();
        }
        if let Ok(section) = local_config_file.section("branch", Some(branchname.into())) {
            if section.num_values() == 0 {
                local_config_file.remove_section_by_id(section.id());
            }
        }
    } else {
        local_config_file.set_raw_value_by(
            "branch",
            Some(branchname.into()),
            "description",
            description,
        )?;
    }

    repo.write_local_config(local_config_file)?;

    Ok(())
}

fn get_stgit_parent(config: &gix::config::Snapshot, branchname: &PartialRefName) -> Option<String> {
    config
        .string_by(
            "branch",
            Some(format!("{branchname}.stgit").as_str().into()),
            "parentbranch",
        )
        .and_then(|bs| bs.to_str().ok().map(str::to_string))
}

fn set_stgit_parent(
    repo: &gix::Repository,
    branchname: &PartialRefName,
    parent_branchname: Option<&PartialRefName>,
) -> Result<()> {
    let subsection = format!("{branchname}.stgit");
    let mut local_config_file = repo.local_config_file()?;
    if let Some(parent_branchname) = parent_branchname {
        local_config_file.set_raw_value_by(
            "branch",
            Some(subsection.as_str().into()),
            "parentbranch",
            parent_branchname.as_ref(),
        )?;
    } else {
        if let Ok(mut value) = local_config_file.raw_value_mut_by(
            "branch",
            Some(subsection.as_str().into()),
            "parentbranch",
        ) {
            value.delete();
        }
        if let Ok(section) = local_config_file.section("branch", Some(subsection.as_str().into())) {
            if section.num_values() == 0 {
                local_config_file.remove_section_by_id(section.id());
            }
        }
    }

    repo.write_local_config(local_config_file)?;

    Ok(())
}
