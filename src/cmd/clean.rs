// SPDX-License-Identifier: GPL-2.0-only

//! `stg clean` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    ext::{CommitExtended, RepositoryExtended},
    patch::PatchName,
    stack::{InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "clean",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Delete empty patches from the series")
        .long_about(
            "Delete the empty patches from the entire series by default, \
             or only empty patches from the applied or unapplied patches. \
             A patch is considered empty if its tree is the same as its parent.",
        )
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Delete empty applied patches")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Delete empty unapplied patches")
                .action(clap::ArgAction::SetTrue),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::AllowUninitialized)?;
    stack.check_head_top_mismatch()?;
    repo.check_repository_state()?;

    let (clean_applied, clean_unapplied) =
        match (matches.get_flag("applied"), matches.get_flag("unapplied")) {
            (false, false) => (true, true),
            opts => opts,
        };

    let mut to_delete: Vec<PatchName> = Vec::new();

    if clean_applied {
        let applied = stack.applied();
        for (i, pn) in applied.iter().enumerate() {
            if stack.get_patch_commit(pn).is_no_change()? {
                if i + 1 == applied.len() {
                    // Do not clean the topmost patch if there are outstanding
                    // conflicts. The patch is only empty because the conflicts caused
                    // its contents to be dumped into the index and worktree.
                    if repo.stupid().statuses(None)?.check_conflicts().is_ok() {
                        to_delete.push(pn.clone());
                    }
                } else {
                    to_delete.push(pn.clone());
                }
            }
        }
    }

    if clean_unapplied {
        for pn in stack.unapplied() {
            if stack.get_patch_commit(pn).is_no_change()? {
                to_delete.push(pn.clone());
            }
        }
    }

    if !to_delete.is_empty() {
        stack
            .setup_transaction()
            .allow_conflicts(true)
            .use_index_and_worktree(false)
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| {
                let to_push = trans.delete_patches(|pn| to_delete.contains(pn))?;
                trans.push_patches(&to_push, false)?;
                Ok(())
            })
            .execute("delete")?;
    }

    Ok(())
}
