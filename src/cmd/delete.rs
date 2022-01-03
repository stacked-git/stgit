use clap::{App, Arg, ArgMatches};

use crate::{
    error::Error,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{ConflictMode, Stack, StackStateAccess, StackTransaction},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("delete", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("delete")
        .about("Delete patches")
        .arg(
            Arg::new("spill")
                .long("spill")
                .help("Spill patch contents to worktree and index")
                .long_help(
                    "Delete the patches, but without modifying the index and worktree. \
                     This only works when deleting applied patches at the top of the \
                     stack. The effect is to \"spill\" the patch contents into the \
                     index and worktree.\n\
                     \n\
                     This can be useful for splitting a patch into smaller pieces.",
                ),
        )
        .arg(
            Arg::new("top")
                .long("top")
                .short('t')
                .help("Delete topmost patch"),
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to delete")
                .multiple_values(true)
                .conflicts_with("top")
                .required_unless_present("top"),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;
    let opt_spill = matches.is_present("spill");

    let patches: Vec<PatchName> = if matches.is_present("top") {
        if let Some(patchname) = stack.applied().last() {
            vec![patchname.clone()]
        } else {
            return Err(Error::NoAppliedPatches);
        }
    } else {
        let patch_ranges = matches
            .values_of("patches")
            .expect("clap will ensure either patches or --top");
        parse_patch_ranges(patch_ranges, stack.all_patches(), stack.all_patches())?
    };

    if opt_spill
        && stack
            .applied()
            .iter()
            .rev()
            .take(patches.len())
            .any(|pn| !patches.contains(pn))
    {
        return Err(Error::Generic(
            "can only spill topmost applied patches".to_string(),
        ));
    }

    let conflicts_okay = false;
    stack.check_repository_state(conflicts_okay)?;
    stack.check_head_top_mismatch()?;
    // TODO: compat: these are not checked in Python version. How well is
    //       this handled here?
    // stack.check_index_clean()?;
    // stack.check_worktree_clean()?;

    let mut stdout = crate::color::get_color_stdout(matches);

    let discard_changes = false;
    let use_index_and_worktree = opt_branch.is_none() && !opt_spill;
    let trans_context = StackTransaction::make_context(
        stack,
        ConflictMode::Disallow,
        discard_changes,
        use_index_and_worktree,
    );

    let exec_context = trans_context.transact(|trans| {
        let to_push = trans.delete_patches(|pn| patches.contains(pn), &mut stdout)?;
        trans.push_patches(&to_push, &mut stdout)?;
        Ok(())
    });
    exec_context.execute("delete")?;

    Ok(())
}
