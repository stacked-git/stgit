// SPDX-License-Identifier: GPL-2.0-only

//! `stg commit` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    stack::{Error, Stack, StackStateAccess},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "commit",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command<'static> {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Finalize patches to the stack base")
        .long_about(
            "Finalize one or more patches into the base of the current stack and \
             remove them from the series. This is the opposite of 'stg uncommit'. \
             Use this command when a patch is completed and no longer needs to be \
             managed with StGit.\n\
             \n\
             By default, the bottommost patch is committed. If patch names are \
             given, the stack is rearranged so that those patches are at the \
             bottom, and then they are committed.\n\
             \n\
             The -n/--number option specifies the number of applied patches to \
             commit (counting from the bottom of the stack). If -a/--all is given, \
             all applied patches are committed.",
        )
        .override_usage(
            "stg commit [OPTIONS] [patch]...\n    \
             stg commit [OPTIONS] -n <number>\n    \
             stg commit [OPTIONS] --all",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to commit")
                .value_name("patch")
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with_all(&["all", "number"]),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Commit the specified number of applied patches")
                .value_name("number")
                .value_parser(crate::argset::parse_usize)
                .conflicts_with("all"),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Commit all applied patches"),
        )
        .arg(
            Arg::new("allow-empty")
                .long("allow-empty")
                .help("Allow empty patches to be committed"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let range_specs = matches.get_many::<patchrange::Specification>("patchranges");
    let patches: Vec<PatchName> = if let Some(range_specs) = range_specs {
        let applied_and_unapplied = stack.applied_and_unapplied().collect::<Vec<&PatchName>>();
        let mut requested_patches = patchrange::patches_from_specs(
            range_specs,
            &stack,
            patchrange::Allow::VisibleWithAppliedBoundary,
        )?;
        requested_patches.sort_unstable_by_key(|pn0| {
            applied_and_unapplied
                .iter()
                .position(|pn1| &pn0 == pn1)
                .unwrap()
        });
        requested_patches
    } else if let Some(number) = matches.get_one::<usize>("number").copied() {
        if number == 0 {
            return Ok(());
        } else if number > stack.applied().len() {
            return Err(anyhow!(
                "There are not `{number}` applied patches to commit"
            ));
        } else {
            stack.applied()[0..number].to_vec()
        }
    } else if stack.applied().is_empty() {
        return Err(Error::NoAppliedPatches.into());
    } else if matches.contains_id("all") {
        stack.applied().to_vec()
    } else {
        vec![stack.applied()[0].clone()]
    };

    if !matches.contains_id("allow-empty") {
        let mut empty_patches: Vec<&PatchName> = Vec::new();
        for pn in &patches {
            let patch_commit = stack.get_patch_commit(pn);
            let parent = patch_commit.parent(0)?;
            if patch_commit.tree_id() == parent.tree_id() {
                empty_patches.push(pn);
            }
        }
        if empty_patches.len() == 1 {
            return Err(anyhow!(
                "Attempt to commit empty patch `{}`; use --allow-empty to override",
                empty_patches[0],
            ));
        } else if !empty_patches.is_empty() {
            return Err(anyhow!(
                "Attempt to commit {} empty patches; use `--allow-empty` to override",
                empty_patches.len(),
            ));
        }
    }

    stack.check_head_top_mismatch()?;

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .allow_conflicts_if_same_top(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.commit_patches(&patches))
        .execute("commit")?;

    Ok(())
}
