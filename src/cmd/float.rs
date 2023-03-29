// SPDX-License-Identifier: GPL-2.0-only

//! `stg float` implementation.

use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchrange, PatchName, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "float",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Push patches to the top, even if applied")
        .long_about(
            "Push patches to the top, even if applied.\n\
             \n\
             Float one or more patches to be the topmost applied patches. The patches \
             to be floated may currently be either applied or unapplied. The necessary \
             pop and push operations will be performed to float the named patches. \
             Patches not specified will remain applied or unapplied as they were prior \
             to the float operation.",
        )
        .override_usage(
            "stg float [OPTIONS] <patch>...\n       \
             stg float [OPTIONS] <-S|--series> <file>",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to float")
                .value_name("patch")
                .num_args(1..)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchRange))
                .conflicts_with("series")
                .required_unless_present("series"),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Reorder patches without reapplying any patches")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("series")
                .long("series")
                .short('S')
                .help("Rearrange according to a series <file>")
                .value_name("file")
                .value_hint(clap::ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(argset::keep_arg())
        .arg(argset::committer_date_is_author_date_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    let noapply_flag = matches.get_flag("noapply");
    let keep_flag = matches.get_flag("keep");
    let opt_series = matches.get_one::<PathBuf>("series").map(PathBuf::as_path);

    repo.check_repository_state()?;
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;
    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = if let Some(series_path) = opt_series {
        parse_series(series_path, &stack)?
    } else {
        let range_specs = matches
            .get_many::<PatchRange>("patchranges")
            .expect("clap ensures either patches or series");
        patchrange::resolve_names(&stack, range_specs, RangeConstraint::Visible)?
    };

    if patches.is_empty() {
        return Err(anyhow!("no patches to float"));
    }

    if !keep_flag && (!noapply_flag || patches.iter().any(|pn| stack.is_applied(pn))) {
        statuses.check_index_and_worktree_clean()?;
    }

    let (applied, unapplied) = if noapply_flag {
        let applied: Vec<PatchName> = stack
            .applied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .cloned()
            .collect();
        let unapplied: Vec<PatchName> = patches
            .iter()
            .chain(stack.unapplied().iter().filter(|pn| !patches.contains(pn)))
            .cloned()
            .collect();
        (applied, unapplied)
    } else {
        let applied: Vec<PatchName> = stack
            .applied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .chain(patches.iter())
            .cloned()
            .collect();
        let unapplied: Vec<PatchName> = stack
            .unapplied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .cloned()
            .collect();
        (applied, unapplied)
    };

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .committer_date_is_author_date(matches.get_flag("committer-date-is-author-date"))
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.reorder_patches(Some(&applied), Some(&unapplied), None))
        .execute("float")?;

    Ok(())
}

fn parse_series(path: &Path, stack: &Stack) -> Result<Vec<PatchName>> {
    let use_stdin = path == Path::new("-");
    let contents: String = if use_stdin {
        use std::io::Read;
        let mut stdin = std::io::stdin();
        let mut contents = String::new();
        stdin.read_to_string(&mut contents)?;
        contents
    } else {
        std::fs::read_to_string(path)?
    };

    let mut series: Vec<PatchRange> = Vec::new();
    for s in contents
        .lines()
        .map(|line| {
            if let Some((content, _comment)) = line.split_once('#') {
                content
            } else {
                line
            }
            .trim()
        })
        .filter(|s| !s.is_empty())
    {
        series.push(PatchRange::from_str(s)?);
    }

    patchrange::resolve_names(stack, series.iter(), RangeConstraint::Visible).with_context(|| {
        if use_stdin {
            "<stdin>".to_string()
        } else {
            path.to_string_lossy().to_string()
        }
    })
}
