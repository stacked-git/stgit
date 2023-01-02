// SPDX-License-Identifier: GPL-2.0-only

//! `stg rename` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::PatchName,
    stack::{Error, InitializationPolicy, Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "rename",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Rename a patch")
        .long_about(
            "Rename [oldpatch] to <newpatch>. If [oldpatch] is not given, \
             the topmost patch will be renamed.",
        )
        .override_usage("stg rename [OPTIONS] [old-patch] <new-patch>")
        .arg(argset::branch_arg())
        .arg(
            Arg::new("patches")
                .help("Optional old patch and the new patch name")
                .required(true)
                .num_args(1..=2)
                .value_parser(clap::value_parser!(PatchName)),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let stack = Stack::from_branch(
        &repo,
        argset::get_one_str(matches, "branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    let mut patches: Vec<PatchName> = matches
        .get_many::<PatchName>("patches")
        .expect("clap ensures one or two names are provided")
        .cloned()
        .collect();

    let (old_patchname, new_patchname) = if patches.len() == 2 {
        let new_patchname = patches.remove(1);
        let old_patchname = patches.remove(0);
        (old_patchname, new_patchname)
    } else if let Some(top_patchname) = stack.applied().last() {
        assert_eq!(patches.len(), 1);
        (top_patchname.clone(), patches.remove(0))
    } else {
        return Err(Error::NoAppliedPatches.into());
    };

    if old_patchname.collides(&new_patchname) {
        return Err(anyhow!("Patch `{old_patchname}` already exists"));
    }

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.rename_patch(&old_patchname, &new_patchname))
        .execute(&format!("rename {old_patchname} {new_patchname}"))?;

    Ok(())
}
