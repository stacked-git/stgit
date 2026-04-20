// SPDX-License-Identifier: GPL-2.0-only

//! `stg diff` implementation.

use std::path::PathBuf;

use anyhow::Result;
use clap::{Arg, ArgMatches, ValueHint};

use crate::{
    argset,
    ext::RepositoryExtended,
    patch::{RangeRevisionSpec, StGitBoundaryRevisions},
    stack::Stack,
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "diff",
    category: super::CommandCategory::PatchInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Show diff between revisions, a patch, or the working tree")
        .long_about(
            "Show diff between revisions, a patch, or the working tree.\n\
             \n\
             Display the diff (default) or diffstat comparing the current working \
             directory or a tree-ish object against another tree-ish object (defaulting \
             to HEAD). File paths can be specified to limit the diff output to the \
             specified files. Tree-ish objects use the format accepted by 'stg id'.",
        )
        .arg(
            Arg::new("pathspecs")
                .help("Limit diff to files matching path(s)")
                .value_name("path")
                .num_args(1..)
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(ValueHint::AnyPath),
        )
        .arg(
            Arg::new("range")
                .long("range")
                .short('r')
                .help("Show the diff between specified revisions")
                .long_help(
                    "Show diff between specified revisions. \
                     Revisions ranges are specified as 'rev1[..[rev2]]'. \
                     The revisions may be standard Git revision specifiers or \
                     patches.",
                )
                .value_name("revspec")
                .value_parser(clap::value_parser!(RangeRevisionSpec))
                .allow_hyphen_values(true),
        )
        .arg(
            Arg::new("stat")
                .long("stat")
                .short('s')
                .help("Show the stat instead of the diff")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::diff_opts_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;

    let revspec = if let Some(range_spec) = matches.get_one::<RangeRevisionSpec>("range") {
        match range_spec.resolve_revisions(&repo, None::<&Stack>, true)? {
            StGitBoundaryRevisions::Single(rev) => rev.commit.id.to_string(),
            StGitBoundaryRevisions::Bounds((rev0, rev1)) => {
                format!("{}..{}", rev0.commit.id, rev1.commit.id)
            }
        }
    } else {
        "HEAD".to_string()
    };

    repo.stupid().diff(
        &revspec,
        matches.get_many::<PathBuf>("pathspecs"),
        matches.get_flag("stat"),
        crate::color::use_color(matches),
        argset::get_diff_opts(matches, &repo.config_snapshot(), false, false),
    )
}
