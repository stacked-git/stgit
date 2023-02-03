// SPDX-License-Identifier: GPL-2.0-only

//! `stg redo` implementation.

use anyhow::Result;
use clap::Arg;

use super::undo::find_undo_state;
use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack},
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "redo",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Undo the last undo operation")
        .long_about(
            "If the last command was an undo, the patch stack state will be reset to its state \
             before the undo. Consecutive redos will undo the effects of consecutive invocations \
             of 'stg undo'.\n\
             \n\
             It is an error to redo if the last stack-modifying command was not an undo.",
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Undo the last <n> undos")
                .value_name("n")
                .allow_negative_numbers(true)
                .value_parser(|s: &str| {
                    argset::parse_usize(s).and_then(|n| {
                        if n >= 1 {
                            Ok(n)
                        } else {
                            Err(anyhow::anyhow!("Bad number of commands to redo"))
                        }
                    })
                }),
        )
        .arg(
            Arg::new("hard")
                .long("hard")
                .help("Discard changes in the index and worktree")
                .action(clap::ArgAction::SetTrue),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git_repository::Repository::open()?;
    let stack = Stack::from_branch(&repo, None, InitializationPolicy::RequireInitialized)?;
    let redo_steps = matches.get_one::<usize>("number").copied().unwrap_or(1);

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .allow_bad_head(true)
        .discard_changes(matches.get_flag("hard"))
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            let undo_steps = -(redo_steps as isize);
            let redo_state = find_undo_state(trans.stack(), undo_steps)?;
            trans.reset_to_state(redo_state)
        })
        .execute(&format!("redo {redo_steps}"))?;

    Ok(())
}
