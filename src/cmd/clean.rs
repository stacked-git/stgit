//! `stg clean` implementation.

use anyhow::Result;
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    repo::RepositoryExtended,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("clean", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("clean")
        .about("Delete empty patches from the series")
        .long_about(
            "Delete the empty patches from the entire series by default, \
             or only empty patches from the applied or unapplied patches. \
             A patch is considered empty if its tree is the same as its parent.",
        )
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Delete empty applied patches"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Delete empty unapplied patches"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    stack.check_head_top_mismatch()?;
    repo.check_repository_state()?;

    let (clean_applied, clean_unapplied) = match (
        matches.is_present("applied"),
        matches.is_present("unapplied"),
    ) {
        (false, false) => (true, true),
        opts => opts,
    };

    let mut to_delete: Vec<PatchName> = Vec::new();

    if clean_applied {
        let applied = stack.applied();
        for (i, pn) in applied.iter().enumerate() {
            if is_empty(&stack, pn)? {
                if i + 1 == applied.len() {
                    // Do not clean the topmost patch if there are outstanding
                    // conflicts. The patch is only empty because the conflicts caused
                    // its contents to be dumped into the index and worktree.
                    if !repo.index()?.has_conflicts() {
                        to_delete.push(pn.clone());
                    }
                } else {
                    to_delete.push(pn.clone());
                }
            }
        }
    }

    if clean_unapplied {
        for pn in stack.unapplied() {
            if is_empty(&stack, pn)? {
                to_delete.push(pn.clone());
            }
        }
    }

    stack
        .setup_transaction()
        .allow_conflicts(true)
        .use_index_and_worktree(false)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            let to_push = trans.delete_patches(|pn| to_delete.contains(pn))?;
            trans.push_patches(&to_push, false)?;
            Ok(())
        })
        .execute("delete")?;

    Ok(())
}

fn is_empty(stack: &Stack, patchname: &PatchName) -> Result<bool> {
    let patch_commit = stack.get_patch_commit(patchname);
    Ok(patch_commit.parent_count() == 1
        && patch_commit.tree_id()
            == stack
                .repo
                .find_commit(patch_commit.parent_id(0).unwrap())?
                .tree_id())
}
