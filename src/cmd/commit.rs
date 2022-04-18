//! `stg commit` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{Error, Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("commit", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("commit")
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
        .arg(
            Arg::new("patches")
                .help("Patches to commit")
                .multiple_values(true)
                .conflicts_with_all(&["all", "number"]),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Commit the specified number of applied patches")
                .value_name("number")
                .validator(|s| {
                    s.parse::<usize>()
                        .map_err(|_| format!("'{}' is not an integer", s))
                })
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

    let patches: Vec<PatchName> = if let Some(patch_ranges) = matches.values_of("patches") {
        let applied_and_unapplied = stack.applied_and_unapplied().collect::<Vec<&PatchName>>();
        let mut requested_patches = parse_patch_ranges(
            patch_ranges,
            applied_and_unapplied.iter().copied(),
            stack.all_patches(),
        )?;
        requested_patches.sort_unstable_by_key(|pn0| {
            applied_and_unapplied
                .iter()
                .position(|pn1| &pn0 == pn1)
                .unwrap()
        });
        requested_patches
    } else if let Some(number) = matches.value_of("number").map(|num_str| {
        num_str
            .parse::<usize>()
            .expect("validator previously parsed this")
    }) {
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
    } else if matches.is_present("all") {
        stack.applied().to_vec()
    } else {
        vec![stack.applied()[0].clone()]
    };

    if !matches.is_present("allow-empty") {
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
