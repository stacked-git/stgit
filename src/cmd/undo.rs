// SPDX-License-Identifier: GPL-2.0-only

//! `stg undo` implementation.

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use clap::Arg;

use crate::{
    color::get_color_stdout,
    stack::{Stack, StackState},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("undo", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("undo")
        .about("Undo the last command")
        .long_about(
            "Reset the patch stack to the state before the last operation. \
             Consecutive undos will go back to yet older stack states.",
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Undo the last N commands")
                .value_name("N")
                .validator(|s| {
                    s.parse::<isize>()
                        .map_err(|_| format!("'{s}' is not an integer"))
                        .and_then(|n| {
                            if n >= 1 {
                                Ok(())
                            } else {
                                Err("Bad number of commands to undo".to_string())
                            }
                        })
                }),
        )
        .arg(
            Arg::new("hard")
                .long("hard")
                .help("Discard changes in the index and worktree"),
        )
}

fn run(matches: &clap::ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let undo_steps = matches
        .value_of("number")
        .map(|num_str| num_str.parse::<isize>().expect("already validated"))
        .unwrap_or(1);

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .allow_bad_head(true)
        .discard_changes(matches.is_present("hard"))
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            let undo_state = find_undo_state(trans.stack(), undo_steps)?;
            trans.reset_to_state(undo_state)
        })
        .execute(&format!("undo {undo_steps}"))?;

    Ok(())
}

pub(super) fn find_undo_state<'repo>(
    stack: &Stack<'repo>,
    undo_steps: isize,
) -> Result<StackState<'repo>> {
    let mut undo_steps = undo_steps;
    let mut state_commit = stack
        .repo
        .find_reference(&stack.refname)?
        .peel_to_commit()?;
    loop {
        let state = StackState::from_commit(stack.repo, &state_commit)?;
        if undo_steps == 0 {
            break Ok(state);
        }
        let msg = state_commit.message_raw_bytes();
        let urstate = parse_undo_redo_message(msg);
        if undo_steps > 0 {
            if let Some(URState::Undo(n)) = urstate {
                undo_steps += n;
            } else {
                undo_steps -= 1;
            }
        } else if let Some(URState::Undo(_)) = urstate {
            undo_steps += 1;
        } else if let Some(URState::Redo(n)) = urstate {
            undo_steps -= n;
        } else {
            break Err(anyhow!("No more redo information available"));
        }

        state_commit = state
            .prev
            .ok_or_else(|| anyhow!("Not enough undo information available"))?;
    }
}

#[derive(Debug)]
enum URState {
    Undo(isize),
    Redo(isize),
}

fn parse_undo_redo_message(msg: &[u8]) -> Option<URState> {
    let fields: Vec<_> = msg.fields().collect();
    if fields.len() == 2 {
        if let Some(n) = fields[1]
            .to_str()
            .ok()
            .and_then(|s| s.parse::<isize>().ok())
        {
            if fields[0] == b"undo" {
                return Some(URState::Undo(n));
            } else if fields[0] == b"redo" {
                return Some(URState::Redo(n));
            }
        }
    }
    None
}
