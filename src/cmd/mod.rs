//! StGit subcommand implementations.
//!
//! Each subcommand is in its own module. The [`get_commands()`] function generates a
//! mapping of the subcommand names to their [`StGitCommand`] struct.

use std::collections::BTreeMap;

pub(crate) mod branch;
pub(crate) mod clean;
pub(crate) mod commit;
pub(crate) mod delete;
pub(crate) mod diff;
pub(crate) mod edit;
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

/// Mapping of StGit subcommand name to [`StGitCommand`] struct.
pub(crate) type Commands = BTreeMap<&'static str, StGitCommand>;

/// Entry point for a StGit subcommand.
pub(crate) struct StGitCommand {
    /// Function pointer for making the [`clap::Command`] for the StGit subcommand.
    pub make: fn() -> clap::Command<'static>,

    /// Function pointer for running the StGit subcommand.
    pub run: fn(&clap::ArgMatches) -> anyhow::Result<()>,
}

/// Generate mapping of subcommand name to [`StGitCommand`].
///
/// This is used in [`crate::main`] for command line argument parsing and
/// eventual dispatch of a subcommand.
pub(crate) fn get_commands() -> Commands {
    BTreeMap::from([
        branch::get_command(),
        clean::get_command(),
        commit::get_command(),
        delete::get_command(),
        diff::get_command(),
        edit::get_command(),
        export::get_command(),
        files::get_command(),
        float::get_command(),
        fold::get_command(),
        goto::get_command(),
        hide::get_command(),
        id::get_command(),
        import::get_command(),
        init::get_command(),
        log::get_command(),
        new::get_command(),
        next::get_command(),
        patches::get_command(),
        pick::get_command(),
        pop::get_command(),
        prev::get_command(),
        pull::get_command(),
        push::get_command(),
        rebase::get_command(),
        redo::get_command(),
        refresh::get_command(),
        rename::get_command(),
        repair::get_command(),
        reset::get_command(),
        series::get_command(),
        show::get_command(),
        sink::get_command(),
        spill::get_command(),
        squash::get_command(),
        sync::get_command(),
        top::get_command(),
        uncommit::get_command(),
        undo::get_command(),
        unhide::get_command(),
        version::get_command(),
    ])
}
