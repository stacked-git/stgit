// SPDX-License-Identifier: GPL-2.0-only

//! `stg sync` implementation.

use std::{ffi::OsStr, path::Path, str::FromStr};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgGroup};

use crate::{
    color::get_color_stdout,
    commit::{CommitExtended, RepositoryCommitExtended},
    patchname::PatchName,
    patchrange,
    repo::RepositoryExtended,
    stack::{Stack, StackStateAccess, StackTransaction},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "sync",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::PatchManipulation,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("sync")
        .about("Synchronize patches with a branch or a series")
        .long_about(
            "For each of the specified patches, perform a three-way merge with the \
             same patch in the specified branch or series. The command can be used for \
             keeping patches on several branches in sync. Note that the operation may \
             fail for some patches because of conflicts. The patches in the series \
             must apply cleanly.",
        )
        .override_usage("stg sync <--ref-branch=BRANCH|--series=SERIES> [<patch>...|--all]")
        .arg(
            Arg::new("patchranges")
                .help("Patches to synchronize")
                .value_name("patch")
                .multiple_values(true)
                .forbid_empty_values(true),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Synchronize all applied patches"),
        )
        .group(
            ArgGroup::new("which-patches")
                .args(&["patchranges", "all"])
                .required(false),
        )
        .arg(
            Arg::new("ref-branch")
                .long("ref-branch")
                .short('B')
                .help("Synchronize patches with <branch>")
                .value_name("branch"),
        )
        .arg(
            Arg::new("series")
                .long("series")
                .short('s')
                .help("Synchronize patches with <series>")
                .value_name("series")
                .value_hint(clap::ValueHint::FilePath),
        )
        .group(
            ArgGroup::new("target")
                .args(&["ref-branch", "series"])
                .required(true),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    repo.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = if matches.is_present("all") {
        stack.applied().to_vec()
    } else if let Some(patchranges) = matches.values_of("patchranges") {
        patchrange::parse_contiguous(
            patchranges,
            &stack,
            patchrange::Allow::VisibleWithAppliedBoundary,
        )?
    } else if let Some(patchname) = stack.applied().last() {
        vec![patchname.clone()]
    } else {
        return Err(crate::stack::Error::NoAppliedPatches.into());
    };

    let ref_stack = if let Some(ref_branchname) = matches.value_of("ref-branch") {
        Some(Stack::from_branch(&repo, Some(ref_branchname))?)
    } else {
        None
    };

    let series_dir = matches.value_of("series").map(|series_path| {
        Path::new(series_path)
            .parent()
            .unwrap_or_else(|| Path::new("."))
    });

    let ref_patches: Vec<PatchName> = if let Some(ref_stack) = ref_stack.as_ref() {
        if ref_stack.branch_name == stack.branch_name {
            return Err(anyhow!("Cannot synchronize with the current branch"));
        }
        ref_stack.applied().to_vec()
    } else if let Some(series_path) = matches.value_of("series") {
        let series = std::fs::read(series_path)
            .with_context(|| format!("opening series `{series_path}`"))?;
        let mut ref_patches = Vec::new();
        for line in series.lines() {
            let line = line
                .find_char('#')
                .map(|pos| &line[..pos])
                .unwrap_or(line)
                .trim();
            if line.is_empty() {
                continue;
            }
            let name = line
                .to_str()
                .map_err(|_| anyhow!("Series `{series_path}` contains non-UTF-8 patchname"))?;
            let patchname = PatchName::from_str(name)?;
            ref_patches.push(patchname);
        }
        ref_patches
    } else {
        panic!("either --ref-branch or --series is required")
    };

    let sync_patches: Vec<PatchName> = patches
        .iter()
        .filter_map(|pn| {
            if ref_patches.contains(pn) {
                Some(pn.clone())
            } else {
                None
            }
        })
        .collect();

    let first_patch = if let Some(patchname) = sync_patches.first() {
        patchname
    } else {
        return Err(anyhow!("No common patches to synchronize"));
    };

    let mut stack = stack;
    let mut pushed: Vec<PatchName> = Vec::new();
    let mut popped: Vec<PatchName> = Vec::new();
    let unapplied: Vec<PatchName> = stack.unapplied().to_vec();

    if let Some(pos) = stack.applied().iter().position(|pn| pn == first_patch) {
        let to_pop: Vec<_> = stack.applied()[pos + 1..].to_vec();
        if !to_pop.is_empty() {
            stack = stack
                .setup_transaction()
                .use_index_and_worktree(true)
                .with_output_stream(get_color_stdout(matches))
                .transact(|trans| {
                    let popped_extra = trans.pop_patches(|pn| to_pop.contains(pn))?;
                    assert!(popped_extra.is_empty());
                    Ok(())
                })
                .execute("sync (pop)")?;
        }
        let mut to_pop = to_pop;
        popped.append(&mut to_pop);
        pushed.push(first_patch.clone());
    }

    popped.extend(patches.iter().filter(|&pn| unapplied.contains(pn)).cloned());

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            for pn in pushed.iter().chain(popped.iter()) {
                let parent_id = trans.top().id();

                if popped.contains(pn) {
                    trans.push_patches(&[pn], false)?;
                }

                if !sync_patches.contains(pn) {
                    continue;
                }

                let commit = trans.get_patch_commit(pn);

                let maybe_tree_id = if let Some(ref_stack) = ref_stack.as_ref() {
                    branch_merge_patch(ref_stack, trans, pn, commit)?
                } else if let Some(series_dir) = series_dir {
                    series_merge_patch(series_dir, trans, pn, commit)?
                } else {
                    panic!("must have either ref_branch or series_dir");
                };

                if let Some(tree_id) = maybe_tree_id {
                    let commit_id = trans.repo().commit_ex(
                        &commit.author(),
                        &commit.committer(),
                        &commit.message_ex(),
                        tree_id,
                        [parent_id],
                    )?;
                    trans.update_patch(pn, commit_id)?;
                }
            }
            Ok(())
        })
        .execute("sync")?;

    Ok(())
}

fn branch_merge_patch(
    ref_stack: &Stack,
    trans: &StackTransaction,
    patchname: &PatchName,
    commit: &git2::Commit,
) -> Result<Option<git2::Oid>> {
    let ref_commit = ref_stack.get_patch_commit(patchname);
    let ref_parent = ref_commit.parent(0)?;
    let stupid = trans.repo().stupid();
    stupid.read_tree_checkout(trans.stack().branch_head.tree_id(), commit.tree_id())?;
    stupid.update_index_refresh()?;
    if !stupid.merge_recursive(ref_parent.tree_id(), commit.tree_id(), ref_commit.tree_id())? {
        return Err(crate::stack::Error::CausedConflicts(format!(
            "Merge conflicts syncing `{patchname}`"
        ))
        .into());
    }
    let mut diff_opts = git2::DiffOptions::new();
    diff_opts.force_binary(true);
    diff_opts.update_index(true);
    let commit_tree = commit.tree()?;
    let diff = trans
        .repo()
        .diff_tree_to_index(Some(&commit_tree), None, Some(&mut diff_opts))?;
    if !diff.deltas().any(|_| true) {
        Ok(None)
    } else {
        let tree_id = stupid.write_tree()?;
        Ok(Some(tree_id))
    }
}

fn series_merge_patch(
    series_dir: &Path,
    trans: &StackTransaction,
    patchname: &PatchName,
    commit: &git2::Commit,
) -> Result<Option<git2::Oid>> {
    let patch_filename: &str = patchname.as_ref();
    let patch_path = series_dir.join(patch_filename);
    let diff = std::fs::read(&patch_path)
        .with_context(|| format!("reading patch `{}`", patch_path.display()))?;

    let parent = commit.parent(0)?;

    let stupid = trans.repo().stupid();

    stupid.update_index_refresh()?;
    stupid.read_tree_checkout(trans.stack().branch_head.tree_id(), parent.tree_id())?;
    stupid
        .apply_to_worktree_and_index(&diff, false, None, None)
        .with_context(|| format!("applying {patchname} from series"))?;
    stupid.update_index_refresh()?;

    let pathsbuf = stupid
        .diff_index_names(parent.tree_id(), None)
        .context("finding modified files")?;
    let mut changed_paths: Vec<&OsStr> = Vec::new();
    for path_bytes in pathsbuf.split_str(b"\0") {
        if !path_bytes.is_empty() {
            let path = path_bytes
                .to_os_str()
                .context("getting modified file list")?;
            changed_paths.push(path);
        }
    }

    let mut index = trans.repo().index()?;
    index.read(true)?;
    index.update_all(changed_paths, None)?;
    index.write()?;
    let tree_id = index.write_tree()?;

    stupid.read_tree_checkout(tree_id, trans.stack().branch_head.tree_id())?;
    if !stupid.merge_recursive(
        parent.tree_id(),
        trans.stack().branch_head.tree_id(),
        tree_id,
    )? {
        return Err(crate::stack::Error::CausedConflicts(format!(
            "Merge conflicts syncing `{patchname}`"
        ))
        .into());
    }

    let mut diff_opts = git2::DiffOptions::new();
    diff_opts.force_binary(true);
    diff_opts.update_index(true);
    let commit_tree = commit.tree()?;
    let diff = trans
        .repo()
        .diff_tree_to_index(Some(&commit_tree), None, Some(&mut diff_opts))?;
    if !diff.deltas().any(|_| true) {
        Ok(None)
    } else {
        Ok(Some(tree_id))
    }
}
