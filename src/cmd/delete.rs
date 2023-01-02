// SPDX-License-Identifier: GPL-2.0-only

//! `stg delete` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{patchrange, PatchName},
    stack::{Error, InitializationPolicy, Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "delete",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Delete patches")
        .override_usage(
            "stg delete [OPTIONS] <patch>...\n       \
             stg delete [OPTIONS] --top",
        )
        .arg(
            Arg::new("patchranges-all")
                .help("Patches to delete")
                .value_name("patch")
                .num_args(1..)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with("top")
                .required_unless_present("top"),
        )
        .arg(
            Arg::new("spill")
                .long("spill")
                .help("Spill patch contents to worktree and index")
                .long_help(
                    "Delete the patches, but without modifying the index and worktree. \
                     This only works when deleting applied patches at the top of the \
                     stack. The effect is to \"spill\" the patch contents into the \
                     index and worktree.\n\
                     \n\
                     This can be useful for splitting a patch into smaller pieces.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("top")
                .long("top")
                .short('t')
                .help("Delete topmost patch")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::branch_arg())
        .arg(argset::push_conflicts_arg())
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let stack = Stack::from_branch(&repo, opt_branch, InitializationPolicy::AllowUninitialized)?;
    let allow_push_conflicts =
        argset::resolve_allow_push_conflicts(&repo.config_snapshot(), matches);
    let spill_flag = matches.get_flag("spill");

    let patches: Vec<PatchName> = if matches.get_flag("top") {
        if let Some(patchname) = stack.applied().last() {
            vec![patchname.clone()]
        } else {
            return Err(Error::NoAppliedPatches.into());
        }
    } else {
        let patchranges = matches
            .get_many::<patchrange::Specification>("patchranges-all")
            .expect("clap will ensure either patches or --top");
        patchrange::patches_from_specs(
            patchranges,
            &stack,
            patchrange::Allow::AllWithAppliedBoundary,
        )?
    };

    if spill_flag
        && stack
            .applied()
            .iter()
            .rev()
            .take(patches.len())
            .any(|pn| !patches.contains(pn))
    {
        return Err(anyhow!("Can only spill topmost applied patches"));
    }

    repo.check_repository_state()?;
    let statuses = repo.stupid().statuses(None)?;
    statuses.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    // TODO: compat: these are not checked in Python version. How well is
    //       this handled here?
    // repo.check_index_clean()?;
    // repo.check_worktree_clean()?;

    if patches.is_empty() {
        return Ok(());
    }

    stack
        .setup_transaction()
        .use_index_and_worktree(opt_branch.is_none() && !spill_flag)
        .allow_push_conflicts(allow_push_conflicts)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            let to_push = trans.delete_patches(|pn| patches.contains(pn))?;
            trans.push_patches(&to_push, false)?;
            Ok(())
        })
        .execute("delete")?;

    Ok(())
}
