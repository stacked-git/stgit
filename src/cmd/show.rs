// SPDX-License-Identifier: GPL-2.0-only

//! `stg show` implementation.

use std::path::PathBuf;

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    ext::RepositoryExtended,
    patch::{RangeConstraint, RangeRevisionSpec},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "show",
    category: super::CommandCategory::PatchInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Show patch commits")
        .long_about(
            "Show the commit log and diff corresponding to the given patches. \
             The topmost patch is shown by default, or HEAD if no patches are \
             applied.\n\
             The output is similar to 'git show'.",
        )
        .override_usage(
            "stg show [OPTIONS] [patch-or-rev]... [-- <path>...]\n       \
             stg show [OPTIONS] [--patch <patch-or-rev>]... [-- <path>...]\n       \
             stg show [OPTIONS] [-A] [-U] [-H] [-- <path>...]",
        )
        .arg(
            Arg::new("patchranges-all")
                .help("Patches or revisions to show")
                .long_help(
                    "Patches or revisions to show.\n\
                     \n\
                     A patch name, patch range of the form \
                     '[begin-patch]..[end-patch]', or any valid Git revision \
                     may be specified.",
                )
                .value_name("patch-or-rev")
                .num_args(1..)
                .value_parser(clap::value_parser!(RangeRevisionSpec))
                .conflicts_with_all(["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("pathspecs")
                .help("Limit diff to files matching path")
                .value_name("path")
                .last(true)
                .num_args(1..)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("patchranges")
                .long("patch")
                .short('p')
                .help("Patch or revision to show")
                .action(clap::ArgAction::Append)
                .value_name("patch-or-rev")
                .value_parser(clap::value_parser!(RangeRevisionSpec))
                .allow_hyphen_values(true)
                .conflicts_with("patchranges-all"),
        )
        .arg(argset::branch_arg())
        .arg(
            Arg::new("stat")
                .long("stat")
                .short('s')
                .help("Show a diffstat summary instead of the full diff")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::diff_opts_arg())
        .next_help_heading("Selection Options")
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Show the applied patches")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Show the unapplied patches")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Show the hidden patches")
                .action(clap::ArgAction::SetTrue),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch, InitializationPolicy::AllowUninitialized)?;

    let stat_flag = matches.get_flag("stat");
    let applied_flag = matches.get_flag("applied");
    let unapplied_flag = matches.get_flag("unapplied");
    let hidden_flag = matches.get_flag("hidden");

    let mut oids: Vec<gix::ObjectId> = Vec::new();

    if applied_flag {
        for patchname in stack.applied() {
            oids.push(stack.get_patch(patchname).commit.id);
        }
    }
    if unapplied_flag {
        for patchname in stack.unapplied() {
            oids.push(stack.get_patch(patchname).commit.id);
        }
    }
    if hidden_flag {
        for patchname in stack.hidden() {
            oids.push(stack.get_patch(patchname).commit.id);
        }
    }
    if let Some(range_specs) = matches
        .get_many::<RangeRevisionSpec>("patchranges-all")
        .or_else(|| matches.get_many::<RangeRevisionSpec>("patchranges"))
    {
        for spec in range_specs {
            crate::patch::revspec::resolve(
                &repo,
                Some(&stack),
                [spec],
                RangeConstraint::AllWithAppliedBoundary,
            )?
            .iter()
            .for_each(|rev| oids.push(rev.commit.id));
        }
    } else if !applied_flag && !unapplied_flag && !hidden_flag {
        oids.push(stack.get_branch_head().id);
    }

    repo.stupid().show(
        oids,
        matches.get_many::<PathBuf>("pathspecs"),
        stat_flag,
        crate::color::use_color(matches),
        argset::get_diff_opts(matches, &repo.config_snapshot(), false, false),
    )
}
