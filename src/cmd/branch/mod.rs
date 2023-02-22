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
mod unprotect;

use anyhow::Result;
use bstr::ByteSlice;

use crate::{
    argset::{self, get_one_str},
    ext::RepositoryExtended,
    stupid::Stupid,
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
             With a single argument, switch to the named branch.",
        )
        .disable_help_subcommand(true)
        .override_usage(
            "stg branch\
             \n       stg branch [--merge] <branch>\
             \n       stg branch {--list,-l}\
             \n       stg branch {--create,-c} <new-branch> [committish]\
             \n       stg branch --clone [new-branch]\
             \n       stg branch {--rename,-r} [old-name] <new-name>\
             \n       stg branch {--protect,-p} [branch]\
             \n       stg branch {--unprotect,-u} [branch]\
             \n       stg branch --delete [--force] <branch>\
             \n       stg branch --cleanup [--force] [branch]\
             \n       stg branch {--describe,-d} <description> [branch]",
        )
        .subcommand(self::list::command())
        .subcommand(self::create::command())
        .subcommand(self::clone::command())
        .subcommand(self::rename::command())
        .subcommand(self::protect::command())
        .subcommand(self::unprotect::command())
        .subcommand(self::delete::command())
        .subcommand(self::cleanup::command())
        .subcommand(self::describe::command())
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
                .value_parser(argset::parse_branch_name),
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
            s => panic!("unhandled branch subcommand {s}"),
        }
    } else if let Some(target_branchname) = get_one_str(matches, "branch-any") {
        switch(&repo, matches, target_branchname)
    } else if let Ok(branch) = repo.get_branch(None) {
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
    target_branchname: &str,
) -> Result<()> {
    let current_branch = repo.get_branch(None).ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_name().ok());
    if Some(target_branchname) == current_branchname {
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
    let target_branch = repo.get_branch(Some(target_branchname))?;
    stupid.checkout(target_branch.get_branch_name().unwrap())
}

fn set_description(repo: &gix::Repository, branchname: &str, description: &str) -> Result<()> {
    let mut local_config_file = repo.local_config_file()?;
    if description.is_empty() {
        if let Ok(mut value) =
            local_config_file.raw_value_mut("branch", Some(branchname.into()), "description")
        {
            value.delete();
        }
        if let Ok(section) = local_config_file.section("branch", Some(branchname.into())) {
            if section.num_values() == 0 {
                local_config_file.remove_section_by_id(section.id());
            }
        }
    } else {
        local_config_file.set_raw_value(
            "branch",
            Some(branchname.into()),
            "description",
            description,
        )?;
    }

    repo.write_local_config(local_config_file)?;

    Ok(())
}

fn get_stgit_parent(config: &gix::config::Snapshot, branchname: &str) -> Option<String> {
    config
        .plumbing()
        .string(
            "branch",
            Some(format!("{branchname}.stgit").as_str().into()),
            "parentbranch",
        )
        .and_then(|bs| bs.to_str().ok().map(str::to_string))
}

fn set_stgit_parent(
    repo: &gix::Repository,
    branchname: &str,
    parent_branchname: Option<&str>,
) -> Result<()> {
    let subsection = format!("{branchname}.stgit");
    let mut local_config_file = repo.local_config_file()?;
    if let Some(parent_branchname) = parent_branchname {
        local_config_file.set_raw_value(
            "branch",
            Some(subsection.as_str().into()),
            "parentbranch",
            parent_branchname,
        )?;
    } else {
        if let Ok(mut value) = local_config_file.raw_value_mut(
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
