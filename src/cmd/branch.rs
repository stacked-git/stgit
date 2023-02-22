// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch` implementation.

use std::{io::Write, rc::Rc};

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgMatches};
use termcolor::WriteColor;

use crate::{
    argset::{self, get_one_str},
    ext::RepositoryExtended,
    patch::SingleRevisionSpec,
    print_info_message,
    stack::{
        state_refname_from_branch_name, InitializationPolicy, Stack, StackAccess, StackStateAccess,
    },
    stupid::Stupid,
    wrap::Branch,
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
                        .value_parser(clap::value_parser!(SingleRevisionSpec)),
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
    let repo = gix::Repository::open()?;
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
        let current_branch = repo.get_branch(None).ok();
        let current_branchname = current_branch
            .as_ref()
            .and_then(|branch| branch.get_branch_name().ok());
        if let Some(target_branchname) = get_one_str(matches, "branch-any") {
            if Some(target_branchname) == current_branchname {
                Err(anyhow!("{target_branchname} is already the current branch"))
            } else {
                let stupid = repo.stupid();
                let statuses = stupid.statuses(None)?;
                if !matches.get_flag("merge") {
                    statuses.check_worktree_clean()?;
                }
                statuses.check_conflicts()?;
                let target_branch = repo.get_branch(Some(target_branchname))?;
                stupid.checkout(target_branch.get_branch_name().unwrap())
            }
        } else if let Some(name) = current_branchname {
            println!("{name}");
            Ok(())
        } else {
            // Print nothing if HEAD is detached; same as `git branch --show-current`.
            Ok(())
        }
    }
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

fn list(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let mut branchnames = Vec::new();
    for local_branch in repo.references()?.local_branches()?.filter_map(Result::ok) {
        let local_branch = Branch::wrap(local_branch);
        if let Ok(branchname) = local_branch.get_branch_name() {
            if !branchname.ends_with(".stgit") {
                branchnames.push(branchname.to_string());
            }
        }
    }

    branchnames.sort();
    let branchname_width = branchnames.iter().map(String::len).max();

    let current_branch = repo.get_branch(None).ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_name().ok());

    let config = repo.config_snapshot();

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    for branchname in &branchnames {
        let is_current = Some(branchname.as_str()) == current_branchname;

        if is_current {
            stdout.set_color(color_spec.set_intense(true))?;
            write!(stdout, "> ")?;
            color_spec.clear();
            stdout.set_color(&color_spec)?;
        } else {
            write!(stdout, "  ")?;
        };

        if let Ok(stack) = Stack::from_branch(
            repo,
            Some(branchname),
            InitializationPolicy::RequireInitialized,
        ) {
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

        let description = config
            .plumbing()
            .string("branch", Some(branchname.as_str().into()), "description")
            .unwrap_or_default();
        if description.is_empty() {
            writeln!(stdout)?;
        } else {
            write!(stdout, " ")?;
            stdout.write_all(description.as_bstr())?;
            writeln!(stdout)?;
        }
    }

    Ok(())
}

fn create(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let new_branchname = get_one_str(matches, "new-branch").expect("required argument");
    let new_fullname = gix::refs::FullName::try_from(format!("refs/heads/{new_branchname}"))?;
    if repo.try_find_reference(&new_fullname)?.is_some() {
        return Err(anyhow!("branch `{new_branchname}` already exists"));
    }

    repo.check_repository_state()?;
    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;

    let maybe_committish = matches.get_one::<SingleRevisionSpec>("committish");
    let maybe_committish_str = matches
        .get_raw("committish")
        .map(|raw_values| raw_values.into_iter().next().unwrap().to_str().unwrap());

    let parent_branch = if let Some(committish_str) = maybe_committish_str {
        statuses.check_worktree_clean()?;

        if let Ok(parent_reference) = repo.find_reference(committish_str) {
            let parent_branchname = parent_reference
                .name()
                .shorten()
                .to_str()
                .expect("reference name is valid UTF-8");
            print_info_message(
                matches,
                &format!("Recording `{parent_branchname}` as parent branch"),
            );
            Some(Branch::wrap(parent_reference))
        } else {
            print_info_message(
                matches,
                &format!("Do not know how to determine parent branch from `{committish_str}`"),
            );
            None
        }
    } else if let Ok(current_branch) = repo.get_branch(None) {
        Some(current_branch)
    } else {
        None
    };

    let (target_commit, target_name) = if let Some(parent_branch) = parent_branch.as_ref() {
        (
            Rc::new(parent_branch.get_commit()?),
            parent_branch.get_branch_name()?,
        )
    } else if let Some(committish) = maybe_committish {
        (
            committish.resolve(repo, None::<&Stack>)?.commit,
            maybe_committish_str.unwrap(),
        )
    } else {
        (Rc::new(repo.head_commit()?), "HEAD")
    };

    let parent_branchname = parent_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_name().ok());

    repo.edit_reference(gix::refs::transaction::RefEdit {
        change: gix::refs::transaction::Change::Update {
            log: gix::refs::transaction::LogChange {
                mode: gix::refs::transaction::RefLog::AndReference,
                force_create_reflog: false,
                message: format!("branch: Created from {target_name}").into(),
            },
            expected: gix::refs::transaction::PreviousValue::MustNotExist,
            new: gix::refs::Target::Peeled(target_commit.id),
        },
        name: gix::refs::FullName::try_from(format!("refs/heads/{new_branchname}"))?,
        deref: false,
    })?;

    let new_branch = Branch::wrap(repo.find_reference(new_branchname)?);

    let stack = match Stack::from_branch(
        repo,
        Some(new_branchname),
        InitializationPolicy::MustInitialize,
    ) {
        Ok(stack) => stack,
        Err(e) => {
            new_branch.delete()?;
            return Err(e);
        }
    };

    if let Some(parent_branch) = parent_branch.as_ref() {
        set_stgit_parent(repo, new_branchname, parent_branchname)?;
        if let Some(upstream_name) = copy_upstream(parent_branch, &new_branch, repo)? {
            print_info_message(
                matches,
                &format!("Using remote `{upstream_name}` to pull parent from"),
            );
        } else {
            print_info_message(matches, "Recording as a local branch");
        }
    }

    match stupid.checkout(new_branch.get_branch_name().unwrap()) {
        Ok(()) => Ok(()),
        Err(e) => {
            new_branch.delete()?;
            if let Ok(reference) = repo.find_reference(stack.get_stack_refname()) {
                reference.delete().ok();
            }
            Err(e)
        }
    }
}

fn copy_upstream(
    from_branch: &Branch,
    to_branch: &Branch,
    repo: &gix::Repository,
) -> Result<Option<String>> {
    let (category, from_short_name) = from_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("from reference is a local branch");
    assert!(matches!(category, gix::refs::Category::LocalBranch));
    let (category, to_short_name) = to_branch
        .get_reference_name()
        .category_and_short_name()
        .expect("to reference is a local branch");
    assert!(matches!(category, gix::refs::Category::LocalBranch));
    let config = repo.config_snapshot();
    let remote = config.string(format!("branch.{from_short_name}.remote").as_str());
    let merge = config.string(format!("branch.{from_short_name}.merge").as_str());
    if let (Some(remote), Some(merge)) = (remote, merge) {
        let merge_name = gix::refs::FullName::try_from(merge.as_bstr())?;
        let merge_short_name = merge_name.shorten().to_str_lossy();
        let mut local_config_file = repo.local_config_file()?;
        local_config_file.set_raw_value(
            "branch",
            Some(to_short_name),
            "remote",
            remote.as_bstr(),
        )?;
        local_config_file.set_raw_value("branch", Some(to_short_name), "merge", merge.as_bstr())?;
        repo.write_local_config(local_config_file)?;
        Ok(Some(format!("{remote}/{merge_short_name}")))
    } else {
        Ok(None)
    }
}

fn clone(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let current_branch = repo.get_branch(None)?;
    let current_branchname = current_branch.get_branch_name()?;

    let new_branchname = if let Some(new_branchname) = get_one_str(matches, "new-branch") {
        new_branchname.to_string()
    } else {
        let suffix = gix::actor::Time::now_local_or_utc().format(
            time::macros::format_description!("[year][month][day]-[hour][minute][second]"),
        );
        format!("{current_branchname}-{suffix}")
    };

    let stupid = repo.stupid();
    let statuses = stupid.statuses(None)?;
    statuses.check_worktree_clean()?;
    repo.check_repository_state()?;
    statuses.check_conflicts()?;

    if let Ok(stack) = Stack::from_branch(repo, None, InitializationPolicy::RequireInitialized) {
        stack.check_head_top_mismatch()?;
        let state_ref = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference");
        let state_commit = state_ref.id().object()?.try_into_commit()?;
        repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: format!("clone from {current_branchname}").into(),
                },
                expected: gix::refs::transaction::PreviousValue::MustNotExist,
                new: gix::refs::Target::Peeled(state_commit.id),
            },
            name: gix::refs::FullName::try_from(state_refname_from_branch_name(&new_branchname))?,
            deref: false,
        })?;
        stupid.branch_copy(None, &new_branchname)?;
    } else {
        stupid.branch_copy(None, &new_branchname)?;
        Stack::from_branch(
            repo,
            Some(&new_branchname),
            InitializationPolicy::MustInitialize,
        )?;
    };

    set_stgit_parent(repo, &new_branchname, Some(current_branchname))?;

    set_description(
        repo,
        &new_branchname,
        &format!("clone of {current_branchname}"),
    )?;

    let new_branch = repo.get_branch(Some(&new_branchname))?;
    stupid.checkout(new_branch.get_branch_name().unwrap())
}

fn rename(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let names: Vec<_> = matches
        .get_many::<String>("branch-any")
        .unwrap()
        .map(String::as_str)
        .collect();
    let current_branch;
    let (old_branchname, new_branchname) = if names.len() == 2 {
        repo.get_branch(Some(names[0]))?;
        (names[0], names[1])
    } else {
        current_branch = repo.get_branch(None)?;
        (current_branch.get_branch_name()?, names[0])
    };

    let stupid = repo.stupid();
    let parent_branchname = get_stgit_parent(&repo.config_snapshot(), old_branchname);

    if let Ok(stack) = Stack::from_branch(
        repo,
        Some(old_branchname),
        InitializationPolicy::RequireInitialized,
    ) {
        let state_commit = repo
            .find_reference(stack.get_stack_refname())
            .expect("just found this stack state reference")
            .into_fully_peeled_id()?
            .object()?
            .try_into_commit()?;
        repo.edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    mode: gix::refs::transaction::RefLog::AndReference,
                    force_create_reflog: false,
                    message: format!("rename {old_branchname} to {new_branchname}").into(),
                },
                expected: gix::refs::transaction::PreviousValue::MustNotExist,
                new: gix::refs::Target::Peeled(state_commit.id),
            },
            name: gix::refs::FullName::try_from(state_refname_from_branch_name(new_branchname))?,
            deref: false,
        })?;
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
    set_stgit_parent(repo, new_branchname, parent_branchname.as_deref())?;
    Ok(())
}

fn protect(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(
        repo,
        get_one_str(matches, "branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    stack.set_protected(true)
}

fn unprotect(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(
        repo,
        get_one_str(matches, "branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    stack.set_protected(false)
}

fn delete(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let target_branchname = get_one_str(matches, "branch-any").expect("required argument");
    let target_branch = repo.get_branch(Some(target_branchname))?;
    let current_branch = repo.get_branch(None).ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_name().ok());
    if Some(target_branchname) == current_branchname {
        return Err(anyhow!("cannot delete the current branch"));
    }

    if let Ok(stack) = Stack::from_branch(
        repo,
        Some(target_branchname),
        InitializationPolicy::RequireInitialized,
    ) {
        if stack.is_protected(&repo.config_snapshot()) {
            return Err(anyhow!("delete not permitted: this branch is protected"));
        } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
            return Err(anyhow!(
                "delete not permitted: the series still contains patches (override with --force)"
            ));
        }
        stack.deinitialize()?;
    }

    target_branch.delete()?;
    Ok(())
}

fn cleanup(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let stack = Stack::from_branch(
        repo,
        get_one_str(matches, "branch"),
        InitializationPolicy::RequireInitialized,
    )?;
    if stack.is_protected(&repo.config_snapshot()) {
        return Err(anyhow!("clean up not permitted: this branch is protected"));
    } else if !matches.get_flag("force") && stack.all_patches().count() > 0 {
        return Err(anyhow!(
            "clean up not permitted: the series still contains patches (override with --force)"
        ));
    }
    stack.deinitialize()?;
    Ok(())
}

fn describe(repo: &gix::Repository, matches: &ArgMatches) -> Result<()> {
    let branch = repo.get_branch(get_one_str(matches, "branch-any"))?;
    let description = get_one_str(matches, "description").expect("required argument");
    let branchname = branch.get_branch_name()?;
    set_description(repo, branchname, description)
}
