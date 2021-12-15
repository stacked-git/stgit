use std::str::FromStr;

use clap::{App, Arg, ArgMatches};

use crate::{
    error::Error,
    patchname::PatchName,
    stack::{ConflictMode, Stack, StackTransaction},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("rename", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("rename")
        .about("Rename a patch")
        .long_about(
            "Rename [oldpatch] to <newpatch>. If [oldpatch] is not given, \
             the topmost patch will be renamed.",
        )
        .override_usage("stg rename [OPTIONS] [old-patch] <new-patch>")
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("patches")
                .help("Optional old patch and the new patch name")
                .required(true)
                .multiple_values(true)
                .min_values(1)
                .max_values(2)
                .validator(PatchName::from_str)
                .forbid_empty_values(true),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    let mut patches: Vec<PatchName> = matches
        .values_of("patches")
        .expect("clap ensures one or two names are provided")
        .map(|s| PatchName::from_str(s).expect("clap already validated"))
        .collect();

    let (old_patchname, new_patchname) = if patches.len() == 2 {
        let new_patchname = patches.remove(1);
        let old_patchname = patches.remove(0);
        (old_patchname, new_patchname)
    } else if let Some(top_patchname) = stack.state.applied.last() {
        assert_eq!(patches.len(), 1);
        (top_patchname.clone(), patches.remove(0))
    } else {
        return Err(Error::NoAppliedPatches);
    };

    let mut stdout = crate::color::get_color_stdout(matches);

    let discard_changes = false;
    let use_index_and_worktree = false;
    let trans_context = StackTransaction::make_context(
        stack,
        ConflictMode::Allow,
        discard_changes,
        use_index_and_worktree,
    );

    let exec_context = trans_context
        .transact(|trans| trans.rename_patch(&old_patchname, &new_patchname, &mut stdout));
    exec_context.execute(&format!("rename {} to {}", &old_patchname, &new_patchname))?;

    Ok(())
}
