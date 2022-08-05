// SPDX-License-Identifier: GPL-2.0-only

//! `stg show` implementation.

use std::path::PathBuf;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset, patchrange,
    revspec::Error as RevError,
    stack::{Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "show",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::PatchInspection,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("show")
        .about("Show patch commits")
        .long_about(
            "Show the commit log and diff corresponding to the given patches. \
             The topmost patch is shown by default, or HEAD if no patches are \
             applied.\n\
             The output is similar to 'git show'.",
        )
        .override_usage(
            "stg show [OPTIONS] [patch-or-rev]... [-- <path>...]\n    \
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
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with_all(&["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("pathspecs")
                .help("Limit diff to files matching path")
                .value_name("path")
                .last(true)
                .multiple_values(true)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(&*argset::BRANCH_ARG)
        .arg(
            Arg::new("stat")
                .long("stat")
                .short('s')
                .help("Show a diffstat summary instead of the full diff"),
        )
        .arg(&*argset::DIFF_OPTS_ARG)
        .next_help_heading("SELECTION OPTIONS")
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Show the applied patches"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Show the unapplied patches"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Show the hidden patches"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;
    let config = repo.config()?;

    let opt_stat = matches.contains_id("stat");
    let opt_applied = matches.contains_id("applied");
    let opt_unapplied = matches.contains_id("unapplied");
    let opt_hidden = matches.contains_id("hidden");

    let mut oids: Vec<git2::Oid> = Vec::new();

    if opt_applied {
        for patchname in stack.applied() {
            oids.push(stack.get_patch(patchname).commit.id());
        }
    }
    if opt_unapplied {
        for patchname in stack.unapplied() {
            oids.push(stack.get_patch(patchname).commit.id());
        }
    }
    if opt_hidden {
        for patchname in stack.hidden() {
            oids.push(stack.get_patch(patchname).commit.id());
        }
    }
    if let Some(range_specs) = matches.get_many::<patchrange::Specification>("patchranges-all") {
        for spec in range_specs {
            match patchrange::patches_from_specs(
                [spec],
                &stack,
                patchrange::Allow::AllWithAppliedBoundary,
            ) {
                Ok(patchnames) => {
                    for patchname in &patchnames {
                        oids.push(stack.get_patch(patchname).commit.id())
                    }
                }
                Err(patchrange::Error::PatchNotKnown { patchname: _ }) => {
                    let spec_str = spec.to_string();
                    let oid =
                        crate::revspec::parse_stgit_revision(&repo, Some(&spec_str), opt_branch)
                            .map_err(|rev_err| match rev_err.downcast_ref::<RevError>() {
                                Some(RevError::InvalidRevision(spec)) => {
                                    anyhow!("Invalid revision spec `{spec}`")
                                }
                                Some(RevError::RevisionNotFound(spec)) => {
                                    anyhow!("Patch or revision `{spec}` not found")
                                }
                                _ => rev_err,
                            })?
                            .id();
                    oids.push(oid);
                }
                Err(patchrange::Error::PatchName(_)) => {
                    let spec_str = spec.to_string();
                    let oid =
                        crate::revspec::parse_stgit_revision(&repo, Some(&spec_str), opt_branch)
                            .map_err(|rev_err| match rev_err.downcast_ref::<RevError>() {
                                Some(RevError::InvalidRevision(spec)) => {
                                    anyhow!("Invalid patch or revision spec `{spec}`")
                                }
                                Some(RevError::RevisionNotFound(spec)) => {
                                    anyhow!("Invalid patch or revision `{spec}` not found",)
                                }
                                _ => rev_err,
                            })?
                            .id();
                    oids.push(oid);
                }
                Err(e) => {
                    return Err(e.into());
                }
            }
        }
    } else if !opt_applied && !opt_unapplied && !opt_hidden {
        oids.push(stack.branch_head.id());
    }

    repo.stupid().show(
        oids,
        matches.get_many::<PathBuf>("pathspecs"),
        opt_stat,
        crate::color::use_color(matches),
        &argset::get_diff_opts(matches, &config, false, false),
    )
}
