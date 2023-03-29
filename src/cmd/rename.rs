// SPDX-License-Identifier: GPL-2.0-only

//! `stg rename` implementation.

use std::str::FromStr;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    branchloc::BranchLocator,
    color::get_color_stdout,
    ext::RepositoryExtended,
    patch::{PatchLocator, PatchName},
    stack::{InitializationPolicy, Stack, StackStateAccess},
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
            "Rename [old-patch] to <new-patch>. If [old-patch] is not given, the \
             topmost patch will be renamed.",
        )
        .override_usage(super::make_usage(
            "stg rename",
            &["[OPTIONS] [old-patch] <new-patch>"],
        ))
        .arg(argset::branch_arg())
        .arg(
            Arg::new("patches")
                .help("Optional old patch and the new patch name")
                .required(true)
                .allow_hyphen_values(true)
                .num_args(1..=2)
                .value_parser(clap::builder::NonEmptyStringValueParser::new()),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::from_branch_locator(
        &repo,
        matches.get_one::<BranchLocator>("branch"),
        InitializationPolicy::AllowUninitialized,
    )?;

    let patch_args: Vec<&String> = matches
        .get_many::<String>("patches")
        .expect("clap ensures one or two names are provided")
        .collect();

    let (old_patchname, new_patchname) = match patch_args.len() {
        1 => {
            let new_patch_str = patch_args[0];
            let new_patchname = PatchName::from_str(patch_args[0]).map_err(|e| {
                make().error(
                    clap::error::ErrorKind::InvalidValue,
                    format!(
                        "invalid value '{}{new_patch_str}{}' for '{}<new-patch>{}': {e}",
                        anstyle::AnsiColor::Yellow.on_default().render(),
                        anstyle::Reset.render(),
                        anstyle::AnsiColor::Yellow.on_default().render(),
                        anstyle::Reset.render(),
                    ),
                )
            })?;
            let old_patchname = if let Some(top_patchname) = stack.applied().last() {
                top_patchname.clone()
            } else {
                Err(super::Error::NoAppliedPatches)?
            };
            (old_patchname, new_patchname)
        }
        2 => {
            let (old_patch_str, new_patch_str) = (patch_args[0], patch_args[1]);
            let old_patchname = PatchLocator::from_str(old_patch_str)
                .map_err(|e| {
                    make().error(
                        clap::error::ErrorKind::InvalidValue,
                        format!(
                            "invalid value '{}{old_patch_str}{}' for '{}[<old-patch>]{}': {e}",
                            anstyle::AnsiColor::Yellow.on_default().render(),
                            anstyle::Reset.render(),
                            anstyle::AnsiColor::Yellow.on_default().render(),
                            anstyle::Reset.render(),
                        ),
                    )
                })?
                .resolve_name(&stack)?;
            let new_patchname = PatchName::from_str(new_patch_str).map_err(|e| {
                make().error(
                    clap::error::ErrorKind::InvalidValue,
                    format!(
                        "invalid value '{}{new_patch_str}{}' for '{}<new-patch>{}': {e}",
                        anstyle::AnsiColor::Yellow.on_default().render(),
                        anstyle::Reset.render(),
                        anstyle::AnsiColor::Yellow.on_default().render(),
                        anstyle::Reset.render(),
                    ),
                )
            })?;
            (old_patchname, new_patchname)
        }
        _ => unreachable!(),
    };

    if let Some(colliding_name) = stack.collides(&new_patchname) {
        if stack.has_patch(&new_patchname) {
            return Err(anyhow!("patch `{new_patchname}` already exists"));
        } else if colliding_name != &old_patchname {
            return Err(anyhow!(
                "new name `{new_patchname}` collides with `{colliding_name}`"
            ));
        }
    }

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.rename_patch(&old_patchname, &new_patchname))
        .execute(&format!("rename {old_patchname} {new_patchname}"))?;

    Ok(())
}
