use anyhow::{anyhow, Result};
use clap::Arg;

use crate::color::get_color_stdout;
use crate::patchrange::parse_patch_ranges;
use crate::stack::{Stack, StackState};
use crate::stupid::Stupid;

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("reset", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("reset")
        .about("Reset the patch stack to an earlier state")
        .long_about(
            "Reset the patch stack to an earlier state. If no state is specified, reset only the \
             changes in the worktree.\n\
             \n\
             The state is specified with a commit id from the stack log, which may be viewed with \
             \"stg log\". Patch name arguments may optionally be provided to limit which patches \
             are reset.",
        )
        .override_usage(
            "stg reset [--hard] [<committish> [<patchname>...]]\n    \
             stg reset --hard",
        )
        .trailing_var_arg(true)
        .arg(
            Arg::new("committish")
                .help("Stack state committish")
                .required_unless_present("hard"),
        )
        .arg(
            Arg::new("patchname")
                .help("Only reset patchname(s)")
                .multiple_values(true),
        )
        .arg(
            Arg::new("hard")
                .long("hard")
                .help("Discard changes in the index and worktree"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    if let Some(committish) = matches.value_of("committish") {
        let stack = Stack::from_branch(&repo, None)?;
        let commit = repo
            .revparse_single(committish)
            .map_err(|_| anyhow!("invalid committish `{committish}`"))?
            .into_commit()
            .map_err(|_| anyhow!("target `{committish}` is not a commit"))?;
        stack
            .setup_transaction()
            .use_index_and_worktree(true)
            .discard_changes(matches.is_present("hard"))
            .allow_bad_head(matches.values_of("patchname").is_none())
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| {
                let reset_state = StackState::from_commit(trans.repo(), &commit)?;
                if let Some(names) = matches.values_of("patchname") {
                    let patchnames = parse_patch_ranges(
                        names,
                        reset_state.all_patches(),
                        reset_state.all_patches(),
                    )?;
                    trans.reset_to_state_partially(reset_state, &patchnames)
                } else {
                    trans.reset_to_state(reset_state)
                }
            })
            .execute("reset")?;
        Ok(())
    } else if matches.is_present("hard") {
        let head_tree = repo.head()?.peel_to_tree()?;
        repo.stupid().read_tree_checkout_hard(head_tree.id())
    } else {
        unreachable!();
    }
}
