// SPDX-License-Identifier: GPL-2.0-only

//! `stg sync` implementation.

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use bstr::{BString, ByteSlice};
use clap::{Arg, ArgGroup};

use crate::{
    argset,
    branchloc::BranchLocator,
    color::get_color_stdout,
    ext::{CommitExtended, RepositoryExtended},
    patch::{patchrange, PatchName, PatchRange, RangeConstraint},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess, StackTransaction},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "sync",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Synchronize patches with a branch or a series")
        .long_about(
            "For each of the specified patches, perform a three-way merge with the \
             same patch in the specified branch or series. The command can be used for \
             keeping patches on several branches in sync. Note that the operation may \
             fail for some patches because of conflicts. The patches in the series \
             must apply cleanly.",
        )
        .override_usage(super::make_usage(
            "stg sync",
            &["<--ref-branch=BRANCH|--series=SERIES> [<patch>...|--all]"],
        ))
        .arg(
            Arg::new("patchranges")
                .help("Patches to synchronize")
                .value_name("patch")
                .num_args(1..)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchRange)),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Synchronize all applied patches")
                .action(clap::ArgAction::SetTrue),
        )
        .group(
            ArgGroup::new("which-patches")
                .args(["patchranges", "all"])
                .required(false),
        )
        .arg(
            Arg::new("ref-branch")
                .long("ref-branch")
                .short('B')
                .help("Synchronize patches with <branch>")
                .value_name("branch")
                .value_parser(clap::value_parser!(BranchLocator)),
        )
        .arg(
            Arg::new("series")
                .long("series")
                .short('S')
                .help("Synchronize patches with <series>")
                .value_name("series")
                .value_parser(clap::value_parser!(PathBuf))
                .value_hint(clap::ValueHint::FilePath),
        )
        .group(
            ArgGroup::new("target")
                .args(["ref-branch", "series"])
                .required(true),
        )
        .arg(argset::committer_date_is_author_date_arg())
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AllowUninitialized)?;
    let stupid = repo.stupid();

    stupid.statuses(None)?.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = if matches.get_flag("all") {
        stack.applied().to_vec()
    } else if let Some(range_specs) = matches.get_many::<PatchRange>("patchranges") {
        patchrange::resolve_names_contiguous(
            &stack,
            range_specs,
            RangeConstraint::VisibleWithAppliedBoundary,
        )?
    } else if let Some(patchname) = stack.applied().last() {
        vec![patchname.clone()]
    } else {
        return Err(super::Error::NoAppliedPatches.into());
    };

    let ref_stack = matches
        .get_one::<BranchLocator>("ref-branch")
        .map(|loc| {
            Stack::from_branch_locator(&repo, Some(loc), InitializationPolicy::AllowUninitialized)
        })
        .transpose()?;

    let series_dir = matches
        .get_one::<PathBuf>("series")
        .map(|series_path| series_path.parent().unwrap_or_else(|| Path::new(".")));

    let ref_patches: Vec<PatchName> = if let Some(ref_stack) = ref_stack.as_ref() {
        if ref_stack.get_branch_name() == stack.get_branch_name() {
            return Err(anyhow!("cannot synchronize with the current branch"));
        }
        ref_stack.applied().to_vec()
    } else if let Some(series_path) = matches.get_one::<PathBuf>("series") {
        let series = std::fs::read(series_path)
            .with_context(|| format!("opening series `{}`", series_path.to_string_lossy()))?;
        let mut ref_patches = Vec::new();
        for line in series.lines() {
            let line = line
                .find_char('#')
                .map_or(line, |pos| &line[..pos])
                .trim_with(|c| c.is_ascii_whitespace());
            if line.is_empty() {
                continue;
            }
            let name = line.to_str().map_err(|_| {
                anyhow!(
                    "series `{}` contains non-UTF-8 patchname",
                    series_path.to_string_lossy()
                )
            })?;
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
        return Err(anyhow!("no common patches to synchronize"));
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
                let parent_id = trans.top().id;

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
                    let author = commit.author_strict()?;
                    let default_committer = trans.repo().get_committer()?;
                    let committer = if matches.get_flag("committer-date-is-author-date") {
                        let mut committer = default_committer.to_owned()?;
                        committer.time = author.time;
                        committer
                    } else {
                        default_committer.to_owned()?
                    };
                    let commit_id = trans.repo().commit_ex(
                        author.to_ref(&mut gix::date::parse::TimeBuf::default()),
                        committer.to_ref(&mut gix::date::parse::TimeBuf::default()),
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
    commit: &gix::Commit,
) -> Result<Option<gix::ObjectId>> {
    let commit_ref = commit.decode()?;
    let ref_commit = ref_stack.get_patch_commit(patchname);
    let ref_commit_ref = ref_commit.decode()?;
    let ref_parent = ref_commit.get_parent_commit()?;
    let ref_parent_ref = ref_parent.decode()?;
    let stupid = trans.repo().stupid();
    stupid.read_tree_checkout(
        trans.get_branch_head().tree_id()?.detach(),
        commit_ref.tree(),
    )?;
    stupid.update_index_refresh()?;
    if !stupid.merge_recursive(
        ref_parent_ref.tree(),
        commit_ref.tree(),
        ref_commit_ref.tree(),
    )? {
        return Err(super::Error::CausedConflicts(format!(
            "merge conflicts syncing `{patchname}`"
        ))
        .into());
    }

    if stupid.diff_index_quiet(commit_ref.tree())? {
        let tree_id = stupid.write_tree()?;
        Ok(Some(tree_id))
    } else {
        Ok(None)
    }
}

fn series_merge_patch(
    series_dir: &Path,
    trans: &StackTransaction,
    patchname: &PatchName,
    commit: &gix::Commit,
) -> Result<Option<gix::ObjectId>> {
    let patch_filename: &str = patchname.as_ref();
    let patch_path = series_dir.join(patch_filename);
    let diff: BString = std::fs::read(&patch_path)
        .with_context(|| format!("reading patch `{}`", patch_path.display()))?
        .into();

    let parent = commit.get_parent_commit()?;
    let parent_commit_ref = parent.decode()?;

    let stupid = trans.repo().stupid();

    let trans_head_tree_id = trans.get_branch_head().tree_id()?.detach();

    stupid.update_index_refresh()?;
    stupid.read_tree_checkout(trans_head_tree_id, parent_commit_ref.tree())?;
    stupid
        .apply_to_worktree_and_index(diff.as_ref(), false, false, None, None, None)
        .with_context(|| format!("applying {patchname} from series"))?;
    stupid.update_index_refresh()?;

    let pathsbuf = stupid
        .diff_index_names(parent_commit_ref.tree(), None)
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

    stupid.update_index(Some(changed_paths))?;
    let tree_id = stupid.write_tree()?;

    stupid.read_tree_checkout(tree_id, trans_head_tree_id)?;
    if !stupid.merge_recursive(parent_commit_ref.tree(), trans_head_tree_id, tree_id)? {
        return Err(super::Error::CausedConflicts(format!(
            "merge conflicts syncing `{patchname}`"
        ))
        .into());
    }

    if stupid.diff_index_quiet(commit.tree_id()?.detach())? {
        Ok(Some(tree_id))
    } else {
        Ok(None)
    }
}
