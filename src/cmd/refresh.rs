use clap::{App, Arg, ArgGroup, ArgMatches, ArgSettings, ValueHint};

use crate::argset;

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("refresh", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("refresh")
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
          rid of the temp patch.\n\
          \n\
          Additionally, the '--spill' option resets the topmost \
          patch, emptying the patch while leaving the patch's \
          changes intact in the worktree.",
        )
        .arg(
            Arg::new("update")
                .long("update")
                .short('u')
                .about("Only update the current patch files"),
        )
        .arg(
            Arg::new("index")
                .long("index")
                .short('i')
                .about("Refresh from index instead of worktree")
                .long_about(
                    "Instead of setting the patch top to the current \
                contents of the worktree, set it to the current \
                contents of the index.",
                ),
        )
        .arg(
            Arg::new("force")
                .long("force")
                .short('F')
                .about("Force refresh even if index is dirty")
                .long_about(
                    "Instead of warning the user when some work has \
                already been staged (such as with git add \
                interactive mode) force a full refresh.",
                ),
        )
        .arg(
            Arg::new("patch")
                .long("patch")
                .short('p')
                .about("Refresh (applied) PATCH instead of the top patch")
                .setting(ArgSettings::TakesValue)
                .value_name("PATCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("annotate")
                .long("annotate")
                .short('a')
                .about("Annotate the patch log entry with NOTE")
                .setting(ArgSettings::TakesValue)
                .value_name("NOTE")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("submodules")
                .long("submodules")
                .short('s')
                .about("Include submodules in patch content"),
        )
        .arg(
            Arg::new("no-submodules")
                .long("no-submodules")
                .about("Exclude submodules in patch content"),
        )
        .group(ArgGroup::new("submodule-group").args(&["submodules", "no-submodules"]))
        .arg(
            Arg::new("spill")
                .long("spill")
                .about("Spill patch content")
                .long_about(
                    "Spill patch content to worktree and index, \
                emptying patch content",
                ),
        )
        .args(&*crate::message::MESSAGE_ARGS)
        .arg(&*argset::HOOK_ARG)
        .args(&*crate::trailers::TRAILER_ARGS)
        .args(&*crate::signature::AUTHOR_SIGNATURE_ARGS)
        .arg(&*argset::DIFF_OPTS_ARG)
}

fn run(_matches: &ArgMatches) -> super::Result {
    println!("refresh!");
    Ok(())
}
