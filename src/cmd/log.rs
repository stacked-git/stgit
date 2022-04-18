//! `stg log` implementation.

use std::str::FromStr;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    patchname::PatchName,
    stack::{Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("log", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("log")
        .about("Display or optionally clear the stack changelog")
        .long_about(
            "Show the history of changes to the stack. If one or more patch names are given, \
             only the changes affecting those patches are shown.\n\
             \n\
             The \"stg undo\" and \"stg redo\" commands may be used to step back and forth \
             through historical stack states. The \"stg reset\" command may be used to reset \
             the stack directly to a historic state.\n\
             \n\
             The --clear option may be used to delete the stack's change history. Undo and redo \
             are unavailable on a stack without change history. Clearing the stack state \
             history cannot be undone.",
        )
        .override_usage(
            "stg log [OPTIONS] [--] [<patchname>...]\n    \
             stg log --clear",
        )
        .arg(
            Arg::new("patchname")
                .help("Only show history for patchnames")
                .multiple_values(true)
                .validator(PatchName::from_str)
                .forbid_empty_values(true),
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show stack state diffs"),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Limit output to N commits")
                .value_name("N")
                .validator(|s| {
                    s.parse::<usize>()
                        .map_err(|_| format!("'{}' is not a positive integer", s))
                }),
        )
        .arg(
            Arg::new("full")
                .long("full")
                .short('f')
                .help("Show using full commit log format"),
        )
        .arg(
            Arg::new("graphical")
                .long("graphical")
                .short('g')
                .help("Run gitk instead of printing to stdout")
                .conflicts_with_all(&["diff", "number", "full"]),
        )
        .arg(
            Arg::new("clear")
                .long("clear")
                .help("Clear the stack history")
                // .exclusive(true),
                .conflicts_with_all(&["patchname", "diff", "number", "full", "graphical"]),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let mut stack = Stack::from_branch(&repo, opt_branch)?;

    if matches.is_present("clear") {
        stack.clear_state_log("clear log")
    } else {
        let pathspecs = if let Some(names) = matches.values_of("patchname") {
            let mut pathspecs = Vec::with_capacity(names.len());
            for patchname in names.map(|name| PatchName::from_str(name).expect("already validated"))
            {
                if stack.has_patch(&patchname) {
                    pathspecs.push(format!("patches/{patchname}"));
                } else {
                    let similar_names: Vec<&PatchName> = stack
                        .all_patches()
                        .filter(|pn| strsim::jaro_winkler(pn.as_ref(), patchname.as_ref()) > 0.75)
                        .collect();
                    if similar_names.is_empty() {
                        return Err(anyhow!("patch `{patchname}` does not exist"));
                    } else {
                        println!("Possible patches:");
                        for pn in similar_names {
                            println!("  {}", pn);
                        }
                        return Err(anyhow!("ambiguous patch name `{patchname}`"));
                    }
                }
            }
            pathspecs
        } else {
            Vec::new()
        };

        let simplified_parent_id = stack
            .repo
            .find_reference(&stack.refname)?
            .peel_to_commit()?
            .parent_id(0)?;

        let stupid = repo.stupid();

        if matches.is_present("graphical") {
            stupid.gitk(simplified_parent_id, Some(pathspecs))
        } else {
            let num_commits = matches
                .value_of("number")
                .map(|num_str| num_str.parse::<usize>().expect("already validated"));
            stupid.log(
                simplified_parent_id,
                Some(pathspecs),
                num_commits,
                matches.value_of("color"),
                matches.is_present("full"),
                matches.is_present("diff"),
            )
        }
    }
}
