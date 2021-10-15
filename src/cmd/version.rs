use clap::App;

pub(crate) fn get_subcommand() -> App<'static> {
    App::new("version").about("Print version information and exit")
}

pub(crate) fn run() -> super::Result {
    println!("Stacked Git {}", env!("CARGO_PKG_VERSION"));
    std::process::Command::new("git")
        .arg("--version")
        .status()
        .expect("Failed to run git process");
    Ok(())
}
