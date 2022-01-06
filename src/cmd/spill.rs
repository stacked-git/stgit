use std::path::{Path, PathBuf};

use clap::{App, Arg, ArgMatches};

use crate::{
    commit::CommitExtended,
    error::Error,
    index::TemporaryIndex,
    stack::{ConflictMode, Stack, StackStateAccess},
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("spill", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("spill")
        .about("Spill topmost patch content, emptying patch")
        .long_about(
            "Spill topmost patch content. The patch becomes empty, but the patch's \
             changes remain in the index and worktree.\n\
             \n\
             Spilling a patch may be useful for reselecting the files/hunks to be \
             included in the patch.",
        )
        .arg(
            Arg::new("annotate")
                .long("annotate")
                .short('a')
                .help("Annotate the patch log entry with note")
                .takes_value(true)
                .value_name("note"),
        )
        .arg(
            Arg::new("reset")
                .long("reset")
                .short('r')
                .help("Also reset the index")
                .long_help(
                    "Also reset the index such that the patch's changes only remain \
                     in the worktree. Without this option, the patch's changes will \
                     be in both the index and worktree.",
                ),
        )
        .arg(
            Arg::new("pathspecs")
                .help("Only spill files matching path")
                .value_name("path")
                .multiple_values(true)
                .allow_invalid_utf8(true),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    stack.check_head_top_mismatch()?;

    let patchname = stack.applied().last().ok_or(Error::NoAppliedPatches)?;
    let patch_commit = stack.get_patch_commit(patchname);
    let parent = patch_commit.parent(0)?;

    let tree_id = if let Some(pathspecs) = matches.values_of_os("pathspecs") {
        let workdir = repo.workdir().expect("not a bare repository");
        let curdir = std::env::current_dir()?;
        let mut norm_pathspec: Vec<PathBuf> = Vec::with_capacity(pathspecs.len());
        for spec in pathspecs {
            norm_pathspec.push(crate::pathspec::normalize_pathspec(
                workdir,
                &curdir,
                Path::new(spec),
            )?);
        }

        let pathspec = git2::Pathspec::new(&norm_pathspec)?;
        let parent_tree = parent.tree()?;

        repo.with_temp_index(|temp_index| {
            temp_index.read_tree(&parent_tree)?; // Could copy default index
            let all_pathspecs: &[&str] = &["*"];
            temp_index.add_all(
                all_pathspecs,
                git2::IndexAddOption::DEFAULT,
                Some(&mut |path, _matching_pathspec| {
                    if pathspec.matches_path(path, git2::PathspecFlags::DEFAULT) {
                        1 // Skip add; keeps parent's version of file in index
                    } else {
                        0 // Add to index; i.e. keep patch's version of file
                    }
                }),
            )?;
            Ok(temp_index.write_tree()?)
        })?
    } else {
        parent.tree_id()
    };

    let new_commit_id = repo.commit_ex(
        &patch_commit.author(),
        &patch_commit.committer(),
        patch_commit.message_raw().unwrap(),
        tree_id,
        patch_commit.parent_ids(),
    )?;

    let reflog_msg = if let Some(annotation) = matches.value_of("annotate") {
        format!("spill {}\n\n{}", patchname, annotation)
    } else {
        format!("spill {}", patchname)
    };
    let conflict_mode = ConflictMode::Allow;
    let discard_changes = false;
    let use_index_and_worktree = false;
    let patchname = patchname.clone();
    stack
        .transaction(
            conflict_mode,
            discard_changes,
            use_index_and_worktree,
            |trans| trans.update_patch(&patchname, new_commit_id),
        )
        .execute(&reflog_msg)?;

    if matches.is_present("reset") {
        let mut index = repo.index()?;
        let tree = repo.find_tree(tree_id)?;
        index.read_tree(&tree)?;
        index.write()?;
    }

    Ok(())
}
