// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch` implementation.

use std::io::Write;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgMatches};
use termcolor::WriteColor;

use crate::{
    argset::{self, get_one_str},
    print_info_message,
    repo::RepositoryExtended,
    stack::{
        get_branch_name, state_refname_from_branch_name, Stack, StackAccess, StackStateAccess,
    },
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
        .subcommand(
            clap::Command::new("--list")
                .short_flag('l')
                .override_usage("stg branch {--list,-l}")
                .about("List branches in this repository")
                .long_about(
                    "List each branch in the current repository along with its description, if \
                     any. The current branch is prefixed with '>'. Branches initialized with \
                     StGit stacks are prefixed with 's'. Protected branches are prefixed with \
                     'p'.",
                ),
        )
        .subcommand(
            clap::Command::new("--create")
                .short_flag('c')
                .override_usage("stg branch {--create,-c} <new-branch> [committish]")
                .about("Create and switch to a new branch")
                .long_about(
                    "Create and switch to a new branch. The new branch is initialized as a StGit \
                     patch stack. The new branch will be based on the current HEAD, by default, \
                     unless an optional committish provided for the base.\n\
                     \n\
                     StGit attempts to detect the branch from which the new branch forked, as \
                     well as the remote repository of that parent branch such that 'stg pull' \
                     will pull from the correct remote branch. A warning will be printed if the \
                     parent branch cannot be determined.",
                )
                .arg(
                    Arg::new("new-branch")
                        .help("New branch name")
                        .required(true)
                        .value_parser(argset::parse_branch_name),
                )
                .arg(
                    Arg::new("committish")
                        .help("Base commit for new branch")
                        .value_parser(clap::builder::NonEmptyStringValueParser::new()),
                ),
        )
        .subcommand(
            clap::Command::new("--clone")
                .override_usage("stg branch --clone [new-branch]")
                .about("Clone the contents of the current branch")
                .long_about(
                    "Clone the current branch as <new-branch>, if specified, or using the \
                     current branch name with a timestamp.\n\
                     \n\
                     The description of the new branch will indicate it is a clone of the \
                     current branch. The parent information of the new branch is copied \
                     from the current branch.",
                )
                .arg(
                    Arg::new("new-branch")
                        .help("New branch name")
                        .value_parser(argset::parse_branch_name),
                ),
        )
        .subcommand(
            clap::Command::new("--rename")
                .short_flag('r')
                .override_usage("stg branch {--rename,-r} [old-name] <new-name>")
                .about("Rename an existing branch")
                .arg(
                    Arg::new("branch-any")
                        .help("Optional name of branch to rename and new branch name")
                        .hide_short_help(true)
                        .required(true)
                        .num_args(1..=2)
                        .value_parser(argset::parse_branch_name),
                ),
        )
        .subcommand(
            clap::Command::new("--protect")
                .short_flag('p')
                .override_usage("stg branch {--protect,-p} [branch]")
                .about("Prevent StGit from modifying a branch")
                .arg(
                    Arg::new("branch")
                        .help("Branch to protect")
                        .value_name("branch")
                        .value_parser(argset::parse_branch_name),
                ),
        )
        .subcommand(
            clap::Command::new("--unprotect")
                .short_flag('u')
                .override_usage("stg branch {--unprotect,-u} [branch]")
                .about("Allow StGit to modify a previously protected branch")
                .arg(
                    Arg::new("branch")
                        .help("Branch to unprotect")
                        .value_name("branch")
                        .value_parser(argset::parse_branch_name),
                ),
        )
        .subcommand(
            clap::Command::new("--delete")
                .override_usage("stg branch --delete [--force] <branch>")
                .about("Delete a branch")
                .long_about(
                    "Delete a branch.\n\
                     \n\
                     The branch will not be deleted if there are any patches remaining \
                     unless the '--force' option is provided.\n\
                     \n\
                     A protected branch may not be deleted; it must be unprotected first.",
                )
                .arg(
                    Arg::new("branch-any")
                        .help("Branch to delete")
                        .value_name("branch")
                        .required(true)
                        .value_parser(argset::parse_branch_name),
                )
                .arg(
                    Arg::new("force")
                        .long("force")
                        .help("Force deletion even if branch has patches")
                        .action(clap::ArgAction::SetTrue),
                ),
        )
        .subcommand(
            clap::Command::new("--cleanup")
                .override_usage("stg branch --cleanup [--force] [branch]")
                .about("Remove StGit patch stack from branch")
                .long_about(
                    "Remove StGit patch stack from branch. The operation will be refused if \
                     any patches remain, unless the '--force' option is provided.\n\
                     \n\
                     A protected branch will not be cleaned up; it must be unprotected first.\n\
                     \n\
                     A cleaned up branch may be reinitialized using 'stg init'.",
                )
                .arg(
                    Arg::new("branch")
                        .help("Branch to clean up")
                        .value_name("branch")
                        .value_parser(argset::parse_branch_name),
                )
                .arg(
                    Arg::new("force")
                        .long("force")
                        .help("Force clean up even if branch has patches")
                        .action(clap::ArgAction::SetTrue),
                ),
        )
        .subcommand(
            clap::Command::new("--describe")
                .short_flag('d')
                .override_usage("stg branch {--describe,-d} <description> [branch]")
                .alias("--description")
                .about("Set the branch description")
                .arg(
                    Arg::new("description")
                        .help("Description string for branch")
                        .required(true),
                )
                .arg(
                    Arg::new("branch-any")
                        .help("Branch to describe")
                        .value_name("branch")
                        .value_parser(argset::parse_branch_name),
                ),
        )
        .arg(
            Arg::new("merge")
                .long("merge")
                .help("Merge work tree changes into the other branch")
                .action(clap::ArgAction::SetTrue)
                .requires("branch-any"),
        )
        .arg(
            Arg::new("branch-any")
                .help("Branch to switch to")
                .value_name("branch")
                .value_parser(argset::parse_branch_name),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    if let Some((subname, submatches)) = matches.subcommand() {
        match subname {
            "--list" => list(&repo, submatches),
            "--create" => create(&repo, submatches),
            "--clone" => clone(&repo, submatches),
            "--rename" => rename(&repo, submatches),
            "--protect" => protect(&repo, submatches),
            "--unprotect" => unprotect(&repo, submatches),
            "--delete" => delete(&repo, submatches),
            "--cleanup" => cleanup(&repo, submatches),
            "--describe" => describe(&repo, submatches),
            s => panic!("unhandled branch subcommand {s}"),
        }
    } else {
        let current_branch = repo.get_branch(None)?;
        let current_branchname = get_branch_name(&current_branch)?;
        if let Some(target_branchname) = get_one_str(matches, "branch-any") {
            if target_branchname == current_branchname {
                Err(anyhow!("{target_branchname} is already the current branch"))
            } else {
                let stupid = repo.stupid();
                let statuses = stupid.statuses(None)?;
                if !matches.get_flag("merge") {
                    statuses.check_worktree_clean()?;
                }
                statuses.check_conflicts()?;
                let target_branch = repo.get_branch(Some(target_branchname))?;
                stupid.checkout(target_branch.name().unwrap().unwrap())
            }
        } else {
            println!("{current_branchname}");
            Ok(())
        }
    }
}

fn get_description(config: &git2::Config, branchname: &str) -> String {
    config
        .get_string(&format!("branch.{branchname}.description"))
        .unwrap_or_default()
}

fn set_description(config: &mut git2::Config, branchname: &str, description: &str) -> Result<()> {
    let key = format!("branch.{branchname}.description");
    if description.is_empty() {
        match config.remove(&key) {
            Ok(()) => Ok(()),
            Err(e)
                if e.class() == git2::ErrorClass::Config
                    && e.code() == git2::ErrorCode::NotFound =>
            {
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    } else {
        Ok(config.set_str(&key, description)?)
    }
}

fn get_stgit_parent(config: &git2::Config, branchname: &str) -> Option<String> {
    let key = format!("branch.{branchname}.stgit.parentbranch");
    config.get_string(&key).ok()
}

fn set_stgit_parent(
    config: &mut git2::Config,
    branchname: &str,
    parent_branchname: Option<&str>,
) -> Result<()> {
    let key = format!("branch.{branchname}.stgit.parentbranch");
    if let Some(parent_branchname) = parent_branchname {
        Ok(config.set_str(&key, parent_branchname)?)
    } else {
        match config.remove(&key) {
            Ok(()) => Ok(()),
            Err(e)
                if e.class() == git2::ErrorClass::Config
                    && e.code() == git2::ErrorCode::NotFound =>
            {
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    }
}

fn list(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let mut branchnames = Vec::new();
    for branch_result in repo.branches(Some(git2::BranchType::Local))? {
        let (branch, _branch_type) = branch_result?;
        if let Some(branchname) = branch.name()? {
            if !branchname.ends_with(".stgit") {
                branchnames.push(branchname.to_owned());
            }
        }
    }

    branchnames.sort();
    let branchname_width = branchnames.iter().map(String::len).max();

    let current_branchname = repo.get_branch(None).ok().and_then(|branch| {
        branch
            .name()
            .ok()
            .and_then(|name| name.map(ToString::to_string))
    });

    let config = repo.config()?;

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    for branchname in &branchnames {
        let is_current = Some(branchname) == current_branchname.as_ref();

        if is_current {
            stdout.set_color(color_spec.set_intense(true))?;
            write!(stdout, "> ")?;
            color_spec.clear();
            stdout.set_color(&color_spec)?;
        } else {
            write!(stdout, "  ")?;
        };

        if let Ok(stack) = Stack::from_branch(repo, Some(branchname)) {
            color_spec.set_fg(Some(termcolor::Color::Cyan));
            stdout.set_color(&color_spec)?;
            write!(stdout, "s")?;
            color_spec.clear();
            stdout.set_color(&color_spec)?;
            if stack.is_protected(&config) {
                color_spec.set_fg(Some(termcolor::Color::Yellow));
                stdout.set_color(&color_spec)?;
                write!(stdout, "p\t")?;
                color_spec.clear();
                stdout.set_color(&color_spec)?;
            } else {
                write!(stdout, " \t")?;
            }
        } else {
            write!(stdout, "  \t")?;
        }

        if is_current {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Green)))?;
        }
        let branchname_width = branchname_width.expect("max is Some when !branchnames.is_empty()");
        write!(stdout, "{branchname:branchname_width$}")?;
        if is_current {
            color_spec.clear();
            stdout.set_color(&color_spec)?;
        }

        color_spec.set_dimmed(true);
        stdout.set_color(&color_spec)?;
        write!(stdout, "  |")?;
        color_spec.clear();
        stdout.set_color(&color_spec)?;

        let description = get_description(&config, branchname);

        if description.is_empty() {
            writeln!(stdout)?;
        } else {
            writeln!(stdout, " {description}")?;
        }
    }

    Ok(())
}

fn create(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let new_branchname = get_one_str(matches, "new-branch").expect("required argument");

    repo.check_repository_state()?;
    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;

    let parent_branch = if let Some(committish) = get_one_str(matches, "committish") {
        statuses.check_worktree_clean()?;
        let mut parent_branch = None;
        if let Ok(local_parent_branch) = repo.find_branch(committish, git2::BranchType::Local) {
            parent_branch = Some(local_parent_branch);
        } else if let Ok(remote_parent_branch) =
            repo.find_branch(committish, git2::BranchType::Remote)
        {
            parent_branch = Some(remote_parent_branch);
        } else {
            print_info_message(
                matches,
                &format!("Do not know how to determine parent branch from `{committish}`"),
            );
        }
        if let Some(parent_branch) = parent_branch.as_ref() {
            let parent_branchname = parent_branch.name_bytes()?.to_str_lossy();
            print_info_message(
                matches,
                &format!("Recording `{parent_branchname}` as parent branch"),
            );
        }
        parent_branch
    } else if let Ok(current_branch) = repo.get_branch(None) {
        Some(current_branch)
    } else {
        None
    };

    let target_commit = if let Some(parent_branch) = parent_branch.as_ref() {
        parent_branch.get().peel_to_commit()?
    } else if let Some(committish) = get_one_str(matches, "committish") {
        crate::revspec::parse_stgit_revision(repo, Some(committish), None)?.peel_to_commit()?
    } else {
        repo.head()?.peel_to_commit()?
    };

    let parent_branchname = if let Some(parent_branch) = parent_branch.as_ref() {
        Some(get_branch_name(parent_branch)?)
    } else {
        None
    };

    let mut config = repo.config()?;
    let mut new_branch = repo.branch(new_branchname, &target_commit, false)?;
    let stack = match Stack::initialize(repo, Some(new_branchname)) {
        Ok(stack) => stack,
        Err(e) => {
            new_branch.delete()?;
            return Err(e);
        }
    };

    if let Some(parent_branch) = parent_branch.as_ref() {
        set_stgit_parent(&mut config, new_branchname, parent_branchname.as_deref())?;

        let mut has_upstream = false;
        if let Ok(upstream_branch) = parent_branch.upstream() {
            if let Ok(upstream_name) = get_branch_name(&upstream_branch) {
                new_branch.set_upstream(Some(&upstream_name))?;
                has_upstream = true;
                print_info_message(
                    matches,
                    &format!("Using remote `{upstream_name}` to pull parent from"),
                );
            }
        }
        if !has_upstream {
            print_info_message(matches, "Recording as a local branch");
        }
    }

    match stupid.checkout(new_branch.name().unwrap().unwrap()) {
        Ok(()) => Ok(()),
        Err(e) => {
            new_branch.delete()?;
            repo.find_reference(stack.get_stack_refname())
                .and_then(|mut reference| reference.delete())
                .ok();
            Err(e)
        }
    }
}

fn clone(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let current_branch = repo.get_branch(None)?;
    let current_branchname = get_branch_name(&current_branch)?;

    let new_branchname = if let Some(new_branchname) = get_one_str(matches, "new-branch") {
        new_branchname.to_string()
    } else {
        let suffix = chrono::Local::now().format("%Y%m%d-%H%M%S");
        format!("{current_branchname}-{suffix}")
    };

    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_worktree_clean()?;
    repo.check_repository_state()?;
    statuses.check_conflicts()?;

    if let Ok(stack) = Stack::from_branch(repo, None) {
        stack.check_head_top_mismatch()?;
        let state_ref = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference");
        let state_commit = state_ref.peel_to_commit()?;
        repo.reference(
            &state_refname_from_branch_name(&new_branchname),
            state_commit.id(),
            false,
            &format!("clone from {current_branchname}"),
        )?;
        stupid.branch_copy(None, &new_branchname)?;
    } else {
        stupid.branch_copy(None, &new_branchname)?;
        Stack::initialize(repo, Some(&new_branchname))?;
    };

    let mut config = repo.config()?;

    set_stgit_parent(&mut config, &new_branchname, Some(&current_branchname))?;

    set_description(
        &mut config,
        &new_branchname,
        &format!("clone of {current_branchname}"),
    )?;

    let new_branch = repo.get_branch(Some(&new_branchname))?;
    stupid.checkout(new_branch.name().unwrap().unwrap())
}

fn rename(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let names: Vec<_> = matches
        .get_many::<String>("branch-any")
        .unwrap()
        .map(String::as_str)
        .collect();
    let current_branchname;
    let (old_branchname, new_branchname) = if names.len() == 2 {
        repo.get_branch(Some(names[0]))?;
        (names[0], names[1])
    } else {
        let current_branch = repo.get_branch(None)?;
        current_branchname = get_branch_name(&current_branch)?;
        (current_branchname.as_str(), names[0])
    };

    let stupid = repo.stupid();
    let mut config = repo.config()?;
    let parent_branchname = get_stgit_parent(&config, old_branchname);

    if let Ok(stack) = Stack::from_branch(repo, Some(old_branchname)) {
        let state_commit = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference")
            .peel_to_commit()?;
        repo.reference(
            &state_refname_from_branch_name(new_branchname),
            state_commit.id(),
            false,
            &format!("rename {old_branchname} to {new_branchname}"),
        )?;
        stupid
            .config_rename_section(
                &format!("branch.{old_branchname}.stgit"),
                &format!("branch.{new_branchname}.stgit"),
            )
            .ok();
        stupid.branch_move(Some(old_branchname), new_branchname)?;
        stack.deinitialize()?;
    } else {
        stupid.branch_move(Some(old_branchname), new_branchname)?;
    }
    set_stgit_parent(&mut config, new_branchname, parent_branchname.as_deref())?;
    Ok(())
}

fn protect(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(repo, get_one_str(matches, "branch"))?;
    let mut config = repo.config()?;
    stack.set_protected(&mut config, true)
}

fn unprotect(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(repo, get_one_str(matches, "branch"))?;
    let mut config = repo.config()?;
    stack.set_protected(&mut config, false)
}

fn delete(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let target_branchname = get_one_str(matches, "branch-any").expect("required argument");
    let mut target_branch = repo.get_branch(Some(target_branchname))?;
    let current_branch = repo.get_branch(None).ok();
    let current_branchname = current_branch.and_then(|branch| get_branch_name(&branch).ok());
    if Some(target_branchname) == current_branchname.as_deref() {
        return Err(anyhow!("Cannot delete the current branch"));
    }

    let config = repo.config()?;

    if let Ok(stack) = Stack::from_branch(repo, Some(target_branchname)) {
        if stack.is_protected(&config) {
            return Err(anyhow!("Delete not permitted: this branch is protected"));
        } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
            return Err(anyhow!(
                "Delete not permitted: the series still contains patches (override with --force)"
            ));
        }
        stack.deinitialize()?;
    }

    target_branch.delete()?;
    Ok(())
}

fn cleanup(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(repo, get_one_str(matches, "branch"))?;
    let config = repo.config()?;
    if stack.is_protected(&config) {
        return Err(anyhow!("Clean up not permitted: this branch is protected"));
    } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
        return Err(anyhow!(
            "Clean up not permitted: the series still contains patches (override with --force)"
        ));
    }
    stack.deinitialize()?;
    Ok(())
}

fn describe(repo: &git2::Repository, matches: &ArgMatches) -> Result<()> {
    let branch = repo.get_branch(get_one_str(matches, "branch-any"))?;
    let description = get_one_str(matches, "description").expect("required argument");
    let branchname = get_branch_name(&branch)?;
    let mut config = repo.config()?;
    set_description(&mut config, &branchname, description)
}
