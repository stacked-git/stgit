use std::collections::BTreeMap;

pub(crate) mod branch;
pub(crate) mod clean;
pub(crate) mod clone;
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
pub(crate) mod mail;
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

pub(crate) type Commands = BTreeMap<&'static str, StGitCommand>;

pub(crate) struct StGitCommand {
    pub get_app: fn() -> clap::App<'static>,
    pub run: fn(&clap::ArgMatches) -> anyhow::Result<()>,
}

pub(crate) fn get_commands() -> Commands {
    BTreeMap::from([
        clean::get_command(),
        commit::get_command(),
        delete::get_command(),
        diff::get_command(),
        edit::get_command(),
        files::get_command(),
        float::get_command(),
        goto::get_command(),
        hide::get_command(),
        id::get_command(),
        init::get_command(),
        new::get_command(),
        next::get_command(),
        patches::get_command(),
        pop::get_command(),
        prev::get_command(),
        push::get_command(),
        refresh::get_command(),
        rename::get_command(),
        series::get_command(),
        show::get_command(),
        sink::get_command(),
        spill::get_command(),
        top::get_command(),
        uncommit::get_command(),
        unhide::get_command(),
        version::get_command(),
    ])
}

pub(crate) const PYTHON_COMMANDS: &[&str] = &[
    "branch", "clone", "export", "fold", "import", "log", "mail", "pick", "pull", "rebase", "redo",
    "repair", "reset", "squash", "sync", "undo",
];
