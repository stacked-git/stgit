// SPDX-License-Identifier: GPL-2.0-only

//! `stg goto` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{LocationConstraint, PatchLocator, PatchName},
    stack::{InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "goto",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Go to patch by pushing or popping as necessary")
        .arg(argset::keep_arg())
        .arg(argset::merged_arg())
        .arg(argset::committer_date_is_author_date_arg())
        .arg(argset::push_conflicts_arg())
        .arg(
            Arg::new("patch")
                .help("Patch to go to")
                .required(true)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchLocator)),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    let keep_flag = matches.get_flag("keep");
    let merged_flag = matches.get_flag("merged");
    let allow_push_conflicts =
        argset::resolve_allow_push_conflicts(&repo.config_snapshot(), matches);
    let committer_date_is_author_date = matches.get_flag("committer-date-is-author-date");

    repo.check_repository_state()?;
    let statuses = stupid.statuses(None)?;
    statuses.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !keep_flag {
        statuses.check_index_and_worktree_clean()?;
    }

    let patchname = matches
        .get_one::<PatchLocator>("patch")
        .expect("required argument")
        .resolve_name(&stack)?
        .constrain(&stack, LocationConstraint::Visible)?;

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .allow_push_conflicts(allow_push_conflicts)
        .committer_date_is_author_date(committer_date_is_author_date)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            if let Some(pos) = trans.applied().iter().position(|pn| pn == &patchname) {
                let applied = trans.applied()[0..=pos].to_vec();
                let mut unapplied = trans.applied()[pos + 1..].to_vec();
                unapplied.extend(trans.unapplied().iter().cloned());
                trans.reorder_patches(Some(&applied), Some(&unapplied), None)
            } else {
                let pos = trans
                    .unapplied()
                    .iter()
                    .position(|pn| pn == &patchname)
                    .expect("already determined patch exists and not hidden or applied");

                let to_apply: Vec<PatchName> = trans.unapplied()[0..=pos].to_vec();
                trans.push_patches(&to_apply, merged_flag)?;
                Ok(())
            }
        })
        .execute("goto")?;

    Ok(())
}
