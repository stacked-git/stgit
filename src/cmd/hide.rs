use clap::{App, Arg, ArgMatches};

use crate::{
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{ConflictMode, Stack, StackTransaction},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("hide", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("hide")
        .about("Hide patches in the series")
        .long_about(
            "Hide patches in the series.\n\
             \n\
             Hidden patches are no longer shown in the plain 'series' output.",
        )
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to hide")
                .required(true)
                .multiple_values(true)
                .forbid_empty_values(true),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    stack.check_head_top_mismatch()?;

    let patch_ranges = matches
        .values_of("patches")
        .expect("clap ensures at least one range is provided");

    let patches: Vec<PatchName> = parse_patch_ranges(
        patch_ranges,
        stack.state.all_patches(),
        stack.state.all_patches(),
    )?;

    // Already hidden patches are silent no-ops.
    let to_hide: Vec<PatchName> = patches
        .iter()
        .filter(|pn| !stack.state.hidden.contains(pn))
        .cloned()
        .collect();

    let mut stdout = crate::color::get_color_stdout(matches);

    let discard_changes = false;
    let use_index_and_worktree = false;
    let trans_context = StackTransaction::make_context(
        stack,
        ConflictMode::Allow,
        discard_changes,
        use_index_and_worktree,
    );

    let exec_context = trans_context.transact(|trans| trans.hide_patches(&to_hide, &mut stdout));
    exec_context.execute("hide")?;

    Ok(())
}
