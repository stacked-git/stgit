// SPDX-License-Identifier: GPL-2.0-only

//! `stg reset` implementation.

use anyhow::{anyhow, Result};
use clap::Arg;

use crate::{
    color::get_color_stdout,
    patchrange,
    stack::{Stack, StackState},
    stupid::Stupid,
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    (
        "reset",
        StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackManipulation,
        },
    )
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
            Arg::new("patchranges-all")
                .help("Only reset these patches")
                .value_name("patch")
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification)),
        )
        .arg(
            Arg::new("hard")
                .long("hard")
                .help("Discard changes in the index and worktree"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    if let Some(committish) = crate::argset::get_one_str(matches, "committish") {
        let stack = Stack::from_branch(&repo, None)?;
        let commit = repo
            .revparse_single(committish)
            .map_err(|_| anyhow!("Invalid committish `{committish}`"))?
            .into_commit()
            .map_err(|_| anyhow!("Target `{committish}` is not a commit"))?;
        stack
            .setup_transaction()
            .use_index_and_worktree(true)
            .discard_changes(matches.contains_id("hard"))
            .allow_bad_head(
                matches
                    .get_many::<patchrange::Specification>("patchranges-all")
                    .is_none(),
            )
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| {
                let reset_state = StackState::from_commit(trans.repo(), &commit)?;
                if let Some(range_specs) =
                    matches.get_many::<patchrange::Specification>("patchranges-all")
                {
                    let patchnames = patchrange::patches_from_specs(
                        range_specs,
                        &reset_state,
                        patchrange::Allow::All,
                    )?;
                    trans.reset_to_state_partially(reset_state, &patchnames)
                } else {
                    trans.reset_to_state(reset_state)
                }
            })
            .execute("reset")?;
        Ok(())
    } else if matches.contains_id("hard") {
        let head_tree = repo.head()?.peel_to_tree()?;
        repo.stupid().read_tree_checkout_hard(head_tree.id())
    } else {
        unreachable!();
    }
}
