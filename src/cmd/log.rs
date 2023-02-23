// SPDX-License-Identifier: GPL-2.0-only

//! `stg log` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    branchloc::BranchLocator,
    ext::RepositoryExtended,
    patch::{patchrange, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "log",
    category: super::CommandCategory::PatchInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Display or optionally clear the stack changelog")
        .long_about(
            "Show the history of changes to the stack. If one or more patch names are \
             given, only the changes affecting those patches are shown.\n\
             \n\
             The 'stg undo' and 'stg redo' commands may be used to step back and forth \
             through historical stack states. The 'stg reset' command may be used to \
             reset the stack directly to a historic state.\n\
             \n\
             The '--clear' option may be used to delete the stack's change history. \
             Undo and redo are unavailable on a stack without change history. Clearing \
             the stack state history cannot be undone.",
        )
        .override_usage(
            "stg log [OPTIONS] [--] [patch]...\n       \
             stg log --clear",
        )
        .arg(
            Arg::new("patchranges-all")
                .help("Only show history for these patches")
                .value_name("patch")
                .num_args(1..)
                // .allow_hyphen_values() breaks parsing of options such as "-n1"
                .value_parser(clap::value_parser!(PatchRange)),
        )
        .arg(argset::branch_arg())
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show stack state diffs")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Limit output to <n> commits")
                .value_name("n")
                .value_parser(argset::parse_usize),
        )
        .arg(
            Arg::new("full")
                .long("full")
                .short('f')
                .help("Show using full commit log format")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("graphical")
                .long("graphical")
                .short('g')
                .help("Run gitk instead of printing to stdout")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["diff", "number", "full"]),
        )
        .arg(
            Arg::new("clear")
                .long("clear")
                .help("Clear the stack history")
                // .exclusive(true),
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["patchranges-all", "diff", "number", "full", "graphical"]),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let mut stack = Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::RequireInitialized,
    )?;

    if matches.get_flag("clear") {
        stack.clear_state_log("clear log")
    } else {
        let pathspecs: Option<Vec<String>> =
            if let Some(range_specs) = matches.get_many::<PatchRange>("patchranges-all") {
                Some(
                    patchrange::resolve_names(&stack, range_specs, RangeConstraint::All)?
                        .iter()
                        .map(|pn| format!("patches/{pn}"))
                        .collect(),
                )
            } else {
                None
            };

        let simplified_parent_id = stack
            .repo
            .find_reference(stack.get_stack_refname())?
            .into_fully_peeled_id()?
            .object()?
            .to_commit_ref()
            .parents()
            .next()
            .ok_or_else(|| anyhow!("`{}` does not have any parents", stack.get_stack_refname()))?;

        let stupid = repo.stupid();

        if matches.get_flag("graphical") {
            stupid.gitk(simplified_parent_id, pathspecs)
        } else {
            let num_commits = matches.get_one::<usize>("number").copied();
            stupid.log(
                simplified_parent_id,
                pathspecs,
                num_commits,
                crate::color::use_color(matches),
                matches.get_flag("full"),
                matches.get_flag("diff"),
            )
        }
    }
}
