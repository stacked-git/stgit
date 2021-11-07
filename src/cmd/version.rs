use clap::{App, ArgMatches};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("version", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("version").about("Print version information and exit")
}

fn run(_: &ArgMatches) -> super::Result {
    println!("Stacked Git {}", env!("CARGO_PKG_VERSION"));
    std::process::Command::new("git")
        .arg("--version")
        .status()
        .expect("Failed to run git process");
    Ok(())
}
