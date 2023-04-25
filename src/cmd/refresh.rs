// SPDX-License-Identifier: GPL-2.0-only

//! `stg refresh` implementation.

use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use anyhow::{anyhow, Result};
use clap::{Arg, ArgGroup, ArgMatches, ValueHint};
use indexmap::IndexSet;

use crate::{
    argset,
    color::get_color_stdout,
    ext::{CommitExtended, RepositoryExtended, SignatureExtended},
    hook::run_pre_commit_hook,
    patch::{patchedit, LocationConstraint, PatchLocator, PatchName},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::{Status, StatusOptions, Statuses, Stupid, StupidContext},
    wrap::Message,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "refresh",
    category: super::CommandCategory::PatchManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    let app = clap::Command::new("refresh")
        .about("Incorporate worktree changes into current patch")
        .long_about(
            "Include the latest work tree and index changes in the \
             current patch. This command generates a new git commit \
             object for the patch; the old commit is no longer visible.\n\
             \n\
             Refresh will warn if the index is dirty, and require use of \
             either the '--index' or '--force' options to override this \
             check. This is to prevent accidental full refresh when only \
             some changes were staged using git add interactive mode.\n\
             \n\
             You may optionally list one or more files or directories \
             relative to the current working directory; if you do, only \
             matching files will be updated.\n\
             \n\
             Behind the scenes, stg refresh first creates a new \
             temporary patch with your updates, and then merges that \
             patch into the patch you asked to have refreshed. If you \
             asked to refresh a patch other than the topmost patch, \
             there can be conflicts; in that case, the temporary patch \
             will be left for you to take care of, for example with stg \
             squash.\n\
             \n\
             The creation of the temporary patch is recorded in a \
             separate entry in the patch stack log; this means that one \
             undo step will undo the merge between the other patch and \
             the temp patch, and two undo steps will additionally get \
             rid of the temp patch.",
        )
        .arg(
            Arg::new("pathspecs")
                .help("Only refresh files matching path")
                .value_name("path")
                .num_args(1..)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .next_help_heading("Refresh Options")
        .arg(
            Arg::new("update")
                .long("update")
                .short('u')
                .help("Only update the current patch files")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("index")
                .long("index")
                .short('i')
                .help("Refresh from index instead of worktree")
                .long_help(
                    "Instead of setting the patch top to the current \
                     contents of the worktree, set it to the current \
                     contents of the index.",
                )
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["pathspecs", "update", "submodules", "force"]),
        )
        .arg(
            Arg::new("force")
                .long("force")
                .short('F')
                .help("Force refresh even if index is dirty")
                .long_help(
                    "Instead of warning the user when some work has \
                     already been staged (such as with git add \
                     interactive mode) force a full refresh.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(argset::push_conflicts_arg())
        .arg(
            Arg::new("patch")
                .long("patch")
                .short('p')
                .help("Refresh (applied) <patch> instead of the top patch")
                .num_args(1)
                .value_name("patch")
                .value_hint(ValueHint::Other)
                .allow_hyphen_values(true)
                .value_parser(clap::value_parser!(PatchLocator)),
        )
        .arg(
            Arg::new("annotate")
                .long("annotate")
                .short('a')
                .help("Annotate the patch log entry with <note>")
                .num_args(1)
                .value_name("note")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("submodules")
                .long("submodules")
                .help("Include submodules in patch content")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("update"),
        )
        .arg(
            Arg::new("no-submodules")
                .long("no-submodules")
                .help("Exclude submodules in patch content")
                .action(clap::ArgAction::SetTrue),
        )
        .group(ArgGroup::new("submodule-group").args(["submodules", "no-submodules"]))
        .arg(
            Arg::new("spill")
                .long("spill")
                .help("OBSOLETE: use 'stg spill'")
                .hide(true)
                .action(clap::ArgAction::SetTrue),
        );

    patchedit::add_args(app, true, false)
}

fn run(matches: &ArgMatches) -> Result<()> {
    if matches.get_flag("spill") {
        return Err(anyhow!(
            "`stg refresh --spill` is obsolete; use `stg spill` instead"
        ));
    }

    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::AllowUninitialized)?;
    let config = repo.config_snapshot();
    let allow_push_conflicts = argset::resolve_allow_push_conflicts(&config, matches);

    stack.check_head_top_mismatch()?;

    let patchname = if let Some(patch_loc) = matches.get_one::<PatchLocator>("patch") {
        patch_loc
            .resolve_name(&stack)?
            .constrain(&stack, LocationConstraint::Visible)?
    } else if let Some(top_patchname) = stack.applied().last() {
        top_patchname.clone()
    } else {
        return Err(super::Error::NoAppliedPatches.into());
    };

    let tree_id = assemble_refresh_tree(
        &stack,
        matches,
        matches.get_flag("update").then_some(&patchname),
    )?;

    let mut log_msg = "refresh ".to_string();
    let opt_annotate = matches.get_one::<String>("annotate");

    // Make temp patch
    let temp_commit_id = stack.repo.commit_ex(
        &repo.get_author()?.override_author(matches),
        repo.get_committer()?,
        &Message::from(format!("Refresh of {patchname}")),
        tree_id,
        [stack.get_branch_head().id],
    )?;

    let temp_patchname = {
        let len_limit = None;
        let allow = vec![];
        let disallow: Vec<&PatchName> = stack.all_patches().collect();
        PatchName::make("refresh-temp", true, len_limit).uniquify(&allow, &disallow)
    };

    let stack = stack
        .setup_transaction()
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.new_applied(&temp_patchname, temp_commit_id))
        .execute(&format!(
            "refresh {temp_patchname} (create temporary patch)"
        ))?;

    let mut absorb_success = false;
    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .allow_push_conflicts(allow_push_conflicts)
        .transact(|trans| {
            if let Some(pos) = trans.applied().iter().position(|pn| pn == &patchname) {
                // Absorb temp patch into already applied patch
                let to_pop = trans.applied()[pos + 1..].to_vec();
                if to_pop.len() > 1 {
                    let popped_extra = trans.pop_patches(|pn| to_pop.contains(pn))?;
                    assert!(
                        popped_extra.is_empty(),
                        "only requested patches should be popped"
                    );
                    trans.push_patches(&[&temp_patchname], false)?;
                }

                let temp_commit = trans.get_patch_commit(&temp_patchname);

                let mut to_pop = to_pop;
                let top_name = to_pop.pop();
                assert_eq!(top_name.as_ref(), Some(&temp_patchname));

                let (new_patchname, new_commit_id) = match patchedit::EditBuilder::default()
                    .original_patchname(Some(&patchname))
                    .existing_patch_commit(trans.get_patch_commit(&patchname))
                    .override_tree_id(temp_commit.tree_id()?.detach())
                    .allow_diff_edit(false)
                    .allow_implicit_edit(false)
                    .allow_template_save(false)
                    .edit(trans, &repo, matches)?
                {
                    patchedit::EditOutcome::Edited {
                        new_patchname,
                        new_commit_id,
                    } => (new_patchname, new_commit_id),
                    patchedit::EditOutcome::TemplateSaved(_) => {
                        panic!("not allowed for refresh")
                    }
                };

                trans.delete_patches(|pn| pn == &temp_patchname)?;
                assert_eq!(Some(&patchname), trans.applied().last());
                if let Some(commit_id) = new_commit_id {
                    trans.update_patch(&patchname, commit_id)?;
                }
                if let Some(new_patchname) = new_patchname {
                    trans.rename_patch(&patchname, &new_patchname)?;
                } else {
                    log_msg.push_str(patchname.as_ref());
                }
                if let Some(annotation) = opt_annotate {
                    log_msg.push_str("\n\n");
                    log_msg.push_str(annotation);
                }

                trans.push_patches(&to_pop, false)?;
                absorb_success = true;
            } else {
                // Absorb temp patch into unapplied patch
                let popped_extra = trans.pop_patches(|pn| pn == &temp_patchname)?;
                assert!(popped_extra.is_empty());

                // Try to create the new tree of the refreshed patch.
                // This is the same as pushing the temp patch onto the target patch,
                // but without a worktree to spill conflicts to; so if the simple
                // merge fails, the refresh must be aborted.

                let patch_commit = trans.get_patch_commit(&patchname);
                let temp_commit = trans.get_patch_commit(&temp_patchname);
                let base = temp_commit.get_parent_commit()?.tree_id()?.detach();
                let ours = patch_commit.tree_id()?.detach();
                let theirs = temp_commit.tree_id()?.detach();

                if let Some(tree_id) = repo.stupid().with_temp_index(|stupid_temp| {
                    stupid_temp.read_tree(ours)?;
                    if stupid_temp.apply_treediff_to_index(base, theirs, true)? {
                        let tree_id = stupid_temp.write_tree()?;
                        Ok(Some(tree_id))
                    } else {
                        Ok(None)
                    }
                })? {
                    let (new_patchname, new_commit_id) = match patchedit::EditBuilder::default()
                        .original_patchname(Some(&patchname))
                        .existing_patch_commit(trans.get_patch_commit(&patchname))
                        .override_tree_id(tree_id)
                        .allow_diff_edit(false)
                        .allow_template_save(false)
                        .edit(trans, &repo, matches)?
                    {
                        patchedit::EditOutcome::Edited {
                            new_patchname,
                            new_commit_id,
                        } => (new_patchname, new_commit_id),
                        patchedit::EditOutcome::TemplateSaved(_) => {
                            panic!("not allowed for refresh")
                        }
                    };

                    if let Some(commit_id) = new_commit_id {
                        trans.update_patch(&patchname, commit_id)?;
                    }
                    if let Some(new_patchname) = new_patchname {
                        trans.rename_patch(&patchname, &new_patchname)?;
                        log_msg.push_str(new_patchname.as_ref());
                    } else {
                        log_msg.push_str(patchname.as_ref());
                    }
                    if let Some(annotation) = opt_annotate {
                        log_msg.push_str("\n\n");
                        log_msg.push_str(annotation);
                    }
                    trans.delete_patches(|pn| pn == &temp_patchname)?;
                    absorb_success = true;
                }
            }
            Ok(())
        })
        .execute(&log_msg)?;

    if !absorb_success {
        println!(
            "The new changes did not apply cleanly to {}. \
             They were saved in {}.",
            &patchname, &temp_patchname,
        );
    }

    Ok(())
}

fn determine_refresh_paths(
    stupid: &StupidContext,
    statuses: &Statuses,
    patch_commit: Option<&Rc<gix::Commit>>,
    force: bool,
) -> Result<IndexSet<PathBuf>> {
    let refresh_paths: IndexSet<&Path> = if let Some(patch_commit) = patch_commit {
        // Restrict update to the paths that were already part of the patch.
        let parent_tree_id = patch_commit.get_parent_commit()?.tree_id()?.detach();
        let diff_files =
            stupid.diff_tree_files(parent_tree_id, patch_commit.tree_id()?.detach())?;
        let patch_paths = diff_files.iter().collect::<IndexSet<_>>();

        statuses
            .iter()
            .filter_map(|entry| {
                let path = entry.path();
                if patch_paths.contains(path) {
                    Some(path)
                } else {
                    None
                }
            })
            .collect()
    } else {
        statuses.iter().map(|entry| entry.path()).collect()
    };

    // Ensure no conflicts in the files to be refreshed.
    statuses.check_conflicts_filter(|entry| refresh_paths.contains(entry.path()))?;

    // Ensure worktree and index states are valid for the given options.
    // Forcing means changes will be taken from both the index and worktree.
    // If not forcing, all changes must be either in the index or worktree,
    // but not both.
    if !force {
        let mut is_index_clean = true;
        let mut is_worktree_clean = true;
        for entry in statuses.iter() {
            if !matches!(entry.index_status(), Status::Unmodified) {
                is_index_clean = false;
            }
            if !matches!(entry.worktree_status(), Status::Unmodified) {
                is_worktree_clean = false;
            }
            if !is_index_clean && !is_worktree_clean {
                return Err(anyhow!(
                    "the index is dirty; consider using `--index` or `--force`",
                ));
            }
        }
    }

    // TODO: interrogate status once and avoid allocating PathBufs
    let refresh_paths = refresh_paths
        .iter()
        .map(|path| path.to_path_buf())
        .collect();
    Ok(refresh_paths)
}

fn write_tree(
    stack: &Stack,
    refresh_paths: &IndexSet<PathBuf>,
    is_path_limiting: bool,
) -> Result<gix::ObjectId> {
    // N.B. using temp index is necessary for the cases where there are conflicts in the
    // default index. I.e. by using a temp index, a subset of paths without conflicts
    // may be formed into a coherent tree while leaving the default index as-is.
    let stupid = stack.repo.stupid();
    if is_path_limiting {
        let tree_id_result = stupid.with_temp_index(|stupid_temp| {
            stupid_temp.read_tree(stack.get_branch_head().tree_id()?.detach())?;
            stupid_temp.update_index(Some(refresh_paths))?;
            stupid_temp.write_tree()
        });
        stupid.update_index(Some(refresh_paths))?;
        tree_id_result
    } else {
        if !refresh_paths.is_empty() {
            stupid.update_index(Some(refresh_paths))?;
        }
        stupid.write_tree()
    }
}

pub(crate) fn assemble_refresh_tree(
    stack: &Stack,
    matches: &ArgMatches,
    limit_to_patchname: Option<&PatchName>,
) -> Result<gix::ObjectId> {
    let stupid = stack.repo.stupid();
    let opt_pathspecs = matches.get_many::<PathBuf>("pathspecs");
    let is_path_limiting = limit_to_patchname.is_some() || opt_pathspecs.is_some();
    let statuses;

    let refresh_paths = if matches.get_flag("index") {
        // When refreshing from the index, no path limiting may be used.
        assert!(!is_path_limiting);
        IndexSet::new()
    } else {
        let maybe_patch_commit = limit_to_patchname.map(|pn| stack.get_patch_commit(pn));
        let submodules_flag = matches.get_flag("submodules");
        let nosubmodules_flag = matches.get_flag("no-submodules");
        let use_submodules = if !submodules_flag && !nosubmodules_flag {
            stack
                .repo
                .config_snapshot()
                .boolean("stgit.refreshsubmodules")
                .unwrap_or(false)
        } else {
            submodules_flag
        };
        let mut status_opts = StatusOptions::default();
        status_opts.include_submodules(use_submodules);
        if let Some(pathspecs) = opt_pathspecs {
            status_opts.pathspecs(pathspecs);
        }
        statuses = stupid.statuses(Some(&status_opts))?;

        determine_refresh_paths(
            &stupid,
            &statuses,
            maybe_patch_commit,
            matches.get_flag("force"),
        )?
    };

    let tree_id = write_tree(stack, &refresh_paths, is_path_limiting)?;

    let tree_id = if matches.get_flag("no-verify")
        || !run_pre_commit_hook(stack.repo, matches.get_flag("edit"))?
        || !stupid.diff_index_quiet(tree_id)?
    {
        tree_id
    } else {
        // Update index and rewrite tree if hook updated files in index
        write_tree(stack, &refresh_paths, is_path_limiting)?
    };

    Ok(tree_id)
}
