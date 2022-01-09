use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
    str::FromStr,
};

use clap::{App, Arg, ArgGroup, ArgMatches, ArgSettings, ValueHint};
use indexmap::IndexSet;

use crate::{
    commit::CommitExtended,
    error::Error,
    hook::run_pre_commit_hook,
    index::TemporaryIndex,
    patchedit::PatchEditOverlay,
    patchname::PatchName,
    pathspec,
    stack::{ConflictMode, Stack, StackStateAccess},
    stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("refresh", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    let app = App::new("refresh")
        .about("Incorporate worktree changes into current patch")
        .long_about(
            "Include the latest work tree and index changes in the \
             current patch. This command generates a new git commit \
             object for the patch; the old commit is no longer visible.\n\
             \n\
             Refresh will warn if the index is dirty, and require use of \
             either the '--index' or '--force' options to override this \
             check. This is to prevent accidental full refresh when only \
             some changes were staged using git add interative mode.\n\
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
            Arg::new("update")
                .long("update")
                .short('u')
                .help("Only update the current patch files"),
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
                .conflicts_with_all(&["pathspecs", "update", "submodules", "force"]),
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
                ),
        )
        .arg(
            Arg::new("patch")
                .long("patch")
                .short('p')
                .help("Refresh (applied) PATCH instead of the top patch")
                .setting(ArgSettings::TakesValue)
                .value_name("PATCH")
                .value_hint(ValueHint::Other)
                .validator(PatchName::from_str),
        )
        .arg(
            Arg::new("annotate")
                .long("annotate")
                .short('a')
                .help("Annotate the patch log entry with NOTE")
                .setting(ArgSettings::TakesValue)
                .value_name("NOTE")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("submodules")
                .long("submodules")
                .short('s')
                .help("Include submodules in patch content")
                .conflicts_with_all(&["update"]),
        )
        .arg(
            Arg::new("no-submodules")
                .long("no-submodules")
                .help("Exclude submodules in patch content"),
        )
        .group(ArgGroup::new("submodule-group").args(&["submodules", "no-submodules"]))
        .arg(
            Arg::new("spill")
                .long("spill")
                .help("OBSOLETE: use 'stg spill'")
                .hide(true),
        )
        .arg(
            Arg::new("pathspecs")
                .help("Only refresh files matching path")
                .value_name("path")
                .multiple_values(true)
                .allow_invalid_utf8(true),
        );

    crate::patchedit::add_args(app)
}

fn run(matches: &ArgMatches) -> super::Result {
    if matches.is_present("spill") {
        return Err(Error::Generic(
            "`stg refresh --spill` is obsolete; use `stg spill` instead".to_string(),
        ));
    }

    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    stack.check_head_top_mismatch()?;

    let patchname = if let Some(patchname) = matches
        .value_of("patch")
        .map(|s| PatchName::from_str(s).expect("clap already validated"))
    {
        if stack.has_patch(&patchname) {
            patchname
        } else {
            return Err(Error::PatchDoesNotExist(patchname));
        }
    } else if let Some(top_patchname) = stack.applied().last() {
        top_patchname.clone()
    } else {
        return Err(Error::NoAppliedPatches);
    };

    let tree_id = assemble_refresh_tree(
        &stack,
        matches,
        matches.is_present("update").then(|| &patchname),
    )?;

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut log_msg = "refresh ".to_string();
    let opt_annotate = matches.value_of("annotate");

    // Make temp patch
    let patch_commit = stack.get_patch_commit(&patchname);
    let temp_commit_id = stack.repo.commit_ex(
        &patch_commit.author(),
        &patch_commit.committer(),
        &format!("Refresh of {}", &patchname),
        tree_id,
        [stack.head().id()],
    )?;

    let temp_patchname = {
        let len_limit = None;
        let lower = true;
        let allow = vec![];
        let disallow: Vec<&PatchName> = stack.all_patches().collect();
        PatchName::make_unique("refresh-temp", len_limit, lower, &allow, &disallow)
    };

    let discard_changes = false;
    let use_index_and_worktree = false;
    let stack = stack
        .transaction(
            ConflictMode::Disallow,
            discard_changes,
            use_index_and_worktree,
            |trans| trans.push_applied(&temp_patchname, temp_commit_id, &mut stdout),
        )
        .execute(&format!(
            "refresh {} (create temporary patch)",
            &temp_patchname
        ))?;

    let mut absorb_success = false;
    let use_index_and_worktree = true;
    let exec_context = stack.transaction(
        ConflictMode::Disallow,
        discard_changes,
        use_index_and_worktree,
        |trans| {
            if let Some(pos) = trans.applied().iter().position(|pn| pn == &patchname) {
                // Absorb temp patch into already applied patch
                let to_pop = trans.applied()[pos + 1..].to_vec();
                if to_pop.len() > 1 {
                    let popped_extra = trans.pop_patches(|pn| to_pop.contains(pn), &mut stdout)?;
                    assert!(
                        popped_extra.is_empty(),
                        "only requested patches should be popped"
                    );
                    let is_last = true;
                    let already_merged = false;
                    trans.push_patch(&temp_patchname, already_merged, is_last, &mut stdout)?;
                }

                let temp_commit = trans.get_patch_commit(&temp_patchname);

                let mut to_pop = to_pop;
                let top_name = to_pop.pop();
                assert_eq!(top_name.as_ref(), Some(&temp_patchname));

                let (new_patchname, commit_id) = crate::patchedit::edit(
                    trans,
                    &repo,
                    Some(&patchname),
                    Some(trans.get_patch_commit(&patchname)),
                    matches,
                    PatchEditOverlay {
                        tree_id: Some(temp_commit.tree_id()),
                        ..Default::default()
                    },
                )?;

                trans.delete_patches(|pn| pn == &temp_patchname, &mut stdout)?;
                assert_eq!(Some(&patchname), trans.applied().last());
                trans.update_top(commit_id)?;
                if let Some(new_patchname) = new_patchname {
                    trans.rename_patch(&patchname, &new_patchname, &mut stdout)?;
                    log_msg.push_str(new_patchname.as_ref());
                } else {
                    log_msg.push_str(patchname.as_ref());
                }
                if let Some(annotation) = opt_annotate {
                    log_msg.push_str("\n\n");
                    log_msg.push_str(annotation);
                }

                trans.push_patches(&to_pop, &mut stdout)?;
                absorb_success = true;
            } else {
                // Absorb temp patch into unapplied patch
                let popped_extra = trans.pop_patches(|pn| pn == &temp_patchname, &mut stdout)?;
                assert!(popped_extra.is_empty());

                // Try to create the new tree of the refreshed patch.
                // This is the same as pushing the temp patch onto the target patch,
                // but without a worktree to spill conflicts to; so if the simple
                // merge fails, the refresh must be aborted.

                let patch_commit = trans.get_patch_commit(&patchname);
                let temp_commit = trans.get_patch_commit(&temp_patchname);
                let base = temp_commit.parent(0)?.tree_id();
                let ours = patch_commit.tree_id();
                let theirs = temp_commit.tree_id();

                if let Some(tree_id) = repo.with_temp_index_file(|temp_index| {
                    let temp_index_path = temp_index.path().unwrap();
                    let workdir = repo.workdir().unwrap();
                    stupid::read_tree(ours, temp_index_path)?;
                    if stupid::apply_treediff_to_index(base, theirs, workdir, temp_index_path)? {
                        let tree_id = stupid::write_tree(temp_index_path)?;
                        Ok(Some(tree_id))
                    } else {
                        Ok(None)
                    }
                })? {
                    let (new_patchname, commit_id) = crate::patchedit::edit(
                        trans,
                        &repo,
                        Some(&patchname),
                        Some(trans.get_patch_commit(&patchname)),
                        matches,
                        PatchEditOverlay {
                            tree_id: Some(tree_id),
                            ..Default::default()
                        },
                    )?;
                    trans.update_patch(&patchname, commit_id)?;
                    if let Some(new_patchname) = new_patchname {
                        trans.rename_patch(&patchname, &new_patchname, &mut stdout)?;
                        log_msg.push_str(new_patchname.as_ref());
                    } else {
                        log_msg.push_str(patchname.as_ref());
                    }
                    if let Some(annotation) = opt_annotate {
                        log_msg.push_str("\n\n");
                        log_msg.push_str(annotation);
                    }
                    trans.delete_patches(|pn| pn == &temp_patchname, &mut stdout)?;
                    absorb_success = true;
                }
            }
            Ok(())
        },
    );
    exec_context.execute(&log_msg)?;

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
    repo: &git2::Repository,
    pathspecs: Option<clap::OsValues>,
    patch_commit: Option<&git2::Commit>,
    use_submodules: bool,
    force: bool,
) -> Result<IndexSet<PathBuf>, Error> {
    let mut status_opts = git2::StatusOptions::new();
    status_opts.show(git2::StatusShow::IndexAndWorkdir);
    status_opts.exclude_submodules(!use_submodules);

    if let Some(pathspecs) = pathspecs {
        let workdir = repo.workdir().expect("not a bare repository");
        let curdir = std::env::current_dir()?;

        for pathspec in pathspecs {
            let norm_pathspec =
                pathspec::normalize_pathspec(workdir, &curdir, Path::new(pathspec))?;
            status_opts.pathspec(norm_pathspec);
        }
    }

    let mut refresh_paths: IndexSet<PathBuf> = repo
        .statuses(Some(&mut status_opts))?
        .iter()
        .map(|entry| PathBuf::from(path_from_bytes(entry.path_bytes())))
        .collect();

    if let Some(patch_commit) = patch_commit {
        // Restrict update to the paths that were already part of the patch.
        let patch_tree = patch_commit.tree()?;
        let parent_tree = patch_commit.parent(0)?.tree()?;
        let mut diff_opts = git2::DiffOptions::new();
        diff_opts.ignore_submodules(!use_submodules);
        diff_opts.force_binary(true); // Less expensive(?)

        let mut patch_paths: IndexSet<PathBuf> = IndexSet::new();

        repo.diff_tree_to_tree(Some(&parent_tree), Some(&patch_tree), Some(&mut diff_opts))?
            .foreach(
                &mut |delta, _| {
                    if let Some(old_path) = delta.old_file().path() {
                        patch_paths.insert(old_path.to_owned());
                    }
                    if let Some(new_path) = delta.new_file().path() {
                        patch_paths.insert(new_path.to_owned());
                    }
                    true
                },
                None,
                None,
                None,
            )?;

        // Set intersection to determine final subset of paths.
        refresh_paths.retain(|path| patch_paths.contains(path));
    }

    // Ensure no conflicts in the files to be refreshed.
    if repo
        .index()?
        .conflicts()?
        .filter_map(|maybe_entry| maybe_entry.ok())
        .any(|conflict| {
            if let (Some(our), Some(their)) = (&conflict.our, &conflict.their) {
                refresh_paths.contains(path_from_bytes(&our.path))
                    || (their.path != our.path
                        && refresh_paths.contains(path_from_bytes(&their.path)))
            } else if let Some(our) = conflict.our {
                refresh_paths.contains(path_from_bytes(&our.path))
            } else if let Some(their) = conflict.their {
                refresh_paths.contains(path_from_bytes(&their.path))
            } else {
                false
            }
        })
    {
        return Err(Error::OutstandingConflicts);
    }

    // Ensure worktree and index states are valid for the given options.
    // Forcing means changes will be taken from both the index and worktree.
    // If not forcing, all changes must be either in the index or worktree,
    // but not both.
    if !force {
        let mut status_opts = git2::StatusOptions::new();
        status_opts.show(git2::StatusShow::Index);
        status_opts.exclude_submodules(!use_submodules);
        let is_index_clean = repo.statuses(Some(&mut status_opts))?.is_empty();

        if !is_index_clean {
            let mut status_opts = git2::StatusOptions::new();
            status_opts.show(git2::StatusShow::Workdir);
            status_opts.exclude_submodules(!use_submodules);
            let is_worktree_clean = repo.statuses(Some(&mut status_opts))?.is_empty();

            if !is_worktree_clean {
                return Err(Error::Generic(
                    "the index is dirty; consider using `--index` or `--force`".to_string(),
                ));
            }
        }
    }

    Ok(refresh_paths)
}

pub(crate) fn assemble_refresh_tree(
    stack: &Stack,
    matches: &ArgMatches,
    limit_to_patchname: Option<&PatchName>,
) -> Result<git2::Oid, Error> {
    let repo = stack.repo;
    let opt_submodules = matches.is_present("submodules");
    let opt_nosubmodules = matches.is_present("no-submodules");
    let use_submodules = if !opt_submodules && !opt_nosubmodules {
        let config = repo.config()?;
        config.get_bool("stgit.refreshsubmodules").unwrap_or(false)
    } else {
        opt_submodules
    };
    let opt_pathspecs = matches.values_of_os("pathspecs");
    let is_path_limiting = limit_to_patchname.is_some() || opt_pathspecs.is_some();

    let refresh_paths = if matches.is_present("index") {
        // When refreshing from the index, no path limiting may be used.
        assert!(!is_path_limiting);
        IndexSet::new()
    } else {
        let maybe_patch_commit = limit_to_patchname.map(|pn| stack.get_patch_commit(pn));
        determine_refresh_paths(
            repo,
            opt_pathspecs,
            maybe_patch_commit,
            use_submodules,
            matches.is_present("force"),
        )?
    };

    let tree_id = {
        let paths: &IndexSet<PathBuf> = &refresh_paths;
        let mut default_index = stack.repo.index()?;

        // N.B. using temp index is necessary for the cases where there are conflicts in the
        // default index. I.e. by using a temp index, a subset of paths without conflicts
        // may be formed into a coherent tree while leaving the default index as-is.
        let tree_id_result = if is_path_limiting {
            let head_tree = stack.head.tree()?;
            let tree_id_result = stack.repo.with_temp_index(|temp_index| {
                temp_index.read_tree(&head_tree)?;
                temp_index.add_all(paths, git2::IndexAddOption::DEFAULT, None)?;
                Ok(temp_index.write_tree()?)
            });

            default_index.update_all(paths, None)?;
            tree_id_result
        } else {
            if !paths.is_empty() {
                default_index.update_all(paths, None)?;
            }
            Ok(default_index.write_tree()?)
        };
        default_index.write()?;
        tree_id_result
    }?;

    let tree_id = if matches.is_present("no-verify") {
        tree_id
    } else {
        run_pre_commit_hook(repo, matches.is_present("edit"))?;
        // Re-read index from filesystem because pre-commit hook may have modified it
        let mut index = repo.index()?;
        index.read(false)?;
        index.write_tree()?
    };

    Ok(tree_id)
}

#[cfg(unix)]
pub fn path_from_bytes(b: &[u8]) -> &Path {
    use std::os::unix::ffi::OsStrExt;
    Path::new(OsStr::from_bytes(b))
}

#[cfg(windows)]
pub fn path_from_bytes(b: &[u8]) -> &Path {
    Path::new(std::str::from_utf8(b).expect("paths on Windows must be utf8"))
}
