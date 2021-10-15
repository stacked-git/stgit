use clap::App;

pub(crate) fn get_subcommand() -> App<'static> {
    App::new("init").about("Initialize StGit stack on current branch")
}

pub(crate) fn run() -> super::Result {
    Ok(())
}
