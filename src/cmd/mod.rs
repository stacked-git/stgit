// SPDX-License-Identifier: GPL-2.0-only

//! StGit subcommand implementations.
//!
//! Each subcommand is in its own module. The [`STGIT_COMMANDS`] slice constant contains
//! a [`StGitCommand`] instance for each subcommand.

pub(crate) mod branch;
pub(crate) mod clean;
pub(crate) mod commit;
pub(crate) mod completion;
pub(crate) mod delete;
pub(crate) mod diff;
pub(crate) mod edit;
pub(crate) mod email;
pub(crate) mod export;
pub(crate) mod files;
pub(crate) mod float;
pub(crate) mod fold;
pub(crate) mod goto;
pub(crate) mod hide;
pub(crate) mod id;
pub(crate) mod import;
pub(crate) mod init;
pub(crate) mod log;
pub(crate) mod new;
pub(crate) mod next;
pub(crate) mod patches;
pub(crate) mod pick;
pub(crate) mod pop;
pub(crate) mod prev;
pub(crate) mod pull;
pub(crate) mod push;
pub(crate) mod rebase;
pub(crate) mod redo;
pub(crate) mod refresh;
pub(crate) mod rename;
pub(crate) mod repair;
pub(crate) mod reset;
pub(crate) mod series;
pub(crate) mod show;
pub(crate) mod sink;
pub(crate) mod spill;
pub(crate) mod squash;
pub(crate) mod sync;
pub(crate) mod top;
pub(crate) mod uncommit;
pub(crate) mod undo;
pub(crate) mod unhide;
pub(crate) mod version;

/// Command categories for use in, e.g. man pages.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum CommandCategory {
    PatchInspection,
    PatchManipulation,
    StackInspection,
    StackManipulation,
    Administration,
}

/// Entry point for a StGit subcommand.
pub(crate) struct StGitCommand {
    /// Name of command.
    pub name: &'static str,

    /// Category the command belongs in.
    pub category: CommandCategory,

    /// Function pointer for making the [`clap::Command`] for the StGit subcommand.
    pub make: fn() -> clap::Command<'static>,

    /// Function pointer for running the StGit subcommand.
    pub run: fn(&clap::ArgMatches) -> anyhow::Result<()>,
}

/// Builtin [`StGitCommand`]'s.
///
/// This is used in [`crate::main`] for command line argument parsing and
/// eventual dispatch of a subcommand.
pub(crate) const STGIT_COMMANDS: &[StGitCommand] = &[
    branch::STGIT_COMMAND,
    clean::STGIT_COMMAND,
    commit::STGIT_COMMAND,
    completion::STGIT_COMMAND,
    delete::STGIT_COMMAND,
    diff::STGIT_COMMAND,
    edit::STGIT_COMMAND,
    email::STGIT_COMMAND,
    export::STGIT_COMMAND,
    files::STGIT_COMMAND,
    float::STGIT_COMMAND,
    fold::STGIT_COMMAND,
    goto::STGIT_COMMAND,
    hide::STGIT_COMMAND,
    id::STGIT_COMMAND,
    import::STGIT_COMMAND,
    init::STGIT_COMMAND,
    log::STGIT_COMMAND,
    new::STGIT_COMMAND,
    next::STGIT_COMMAND,
    patches::STGIT_COMMAND,
    pick::STGIT_COMMAND,
    pop::STGIT_COMMAND,
    prev::STGIT_COMMAND,
    pull::STGIT_COMMAND,
    push::STGIT_COMMAND,
    rebase::STGIT_COMMAND,
    redo::STGIT_COMMAND,
    refresh::STGIT_COMMAND,
    rename::STGIT_COMMAND,
    repair::STGIT_COMMAND,
    reset::STGIT_COMMAND,
    series::STGIT_COMMAND,
    show::STGIT_COMMAND,
    sink::STGIT_COMMAND,
    spill::STGIT_COMMAND,
    squash::STGIT_COMMAND,
    sync::STGIT_COMMAND,
    top::STGIT_COMMAND,
    uncommit::STGIT_COMMAND,
    undo::STGIT_COMMAND,
    unhide::STGIT_COMMAND,
    version::STGIT_COMMAND,
];
