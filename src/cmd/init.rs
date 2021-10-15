use clap::App;
use git2::Repository;

pub(crate) fn get_subcommand() -> App<'static> {
    App::new("init").about("Initialize StGit stack on current branch")
}

pub(crate) fn run() -> super::Result {
    let repo = Repository::open_from_env()?;
    let branch_name = None;
    crate::stack::initialize(&repo, branch_name)?;
    Ok(())
}
