#[macro_use]
extern crate lazy_static;

mod argset;
mod cmd;
mod error;
mod stack;

use clap::{crate_license, crate_version, App, AppSettings};
use pyo3::prelude::*;

fn main() {
    let app = App::new("stg")
        .about("Maintain a stack of patches on top of a Git branch.")
        .global_setting(AppSettings::ColoredHelp)
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::HelpRequired)
        .global_setting(AppSettings::DisableVersionForSubcommands)
        .setting(AppSettings::AllowExternalSubcommands) // TODO: remove
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version(crate_version!())
        .license(crate_license!())
        .subcommand_placeholder("command", "COMMANDS")
        .subcommand(cmd::init::get_subcommand())
        .subcommand(cmd::new::get_subcommand())
        .subcommand(cmd::series::get_subcommand())
        .subcommand(cmd::refresh::get_subcommand())
        .subcommand(cmd::id::get_subcommand())
        .subcommand(cmd::version::get_subcommand());

    // TODO: alias subcommands from config (stgit.alias.)
    // TODO: add top-level -C option
    // TODO: get repository and/or stack handle for subcommand

    let matches = app.get_matches();

    let result: cmd::Result = match matches.subcommand() {
        Some(("id", cmd_matches)) => cmd::id::run(cmd_matches),
        Some(("init", _)) => cmd::init::run(),
        Some(("new", cmd_matches)) => cmd::new::run(cmd_matches),
        Some(("refresh", cmd_matches)) => cmd::refresh::run(cmd_matches),
        Some(("series", cmd_matches)) => cmd::series::run(cmd_matches),
        Some(("version", _)) => cmd::version::run(),
        _ => punt_to_python(),
    };

    match result {
        Ok(()) => std::process::exit(0),
        _ => std::process::exit(1),
    }
}

fn punt_to_python() -> cmd::Result {
    let result: Result<(), pyo3::PyErr> = Python::with_gil(|py| {
        let stgit_main = py.import("stgit.main")?;
        let main = stgit_main.getattr("main")?;
        let argv: Vec<String> = std::env::args().collect();
        let ret_val = main.call1((argv,))?.extract()?;
        unsafe { pyo3::ffi::Py_Finalize() }
        std::process::exit(ret_val);
    });

    if let Err(e) = result {
        Python::with_gil(|py| e.print(py));
        Err(crate::error::Error::PythonError { source: e })
    } else {
        Ok(())
    }
}
