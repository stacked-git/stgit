#[macro_use]
extern crate lazy_static;

mod argset;
mod cmd;
mod error;
mod patchname;
mod stack;

use std::io::Write;

use clap::{crate_license, crate_version, App, AppSettings};
use pyo3::prelude::*;
use termcolor::WriteColor;

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

    if let Err(e) = result {
        print_error_message(e);
        std::process::exit(1)
    } else {
        std::process::exit(0)
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
        Err(crate::error::Error::PythonError(e))
    } else {
        Ok(())
    }
}

fn print_error_message(err: crate::error::Error) {
    let color_choice = if atty::is(atty::Stream::Stderr) {
        termcolor::ColorChoice::Auto
    } else {
        termcolor::ColorChoice::Never
    };
    let mut stderr = termcolor::StandardStream::stderr(color_choice);
    let mut color = termcolor::ColorSpec::new();
    stderr.set_color(color.set_fg(Some(termcolor::Color::Red)).set_bold(true)).unwrap();
    write!(stderr, "error: ").unwrap();
    stderr.set_color(color.set_fg(None).set_bold(false)).unwrap();
    let err_string: String = err.to_string();
    let mut remainder: &str = &err_string;
    loop {
        let parts: Vec<&str> = remainder.splitn(3, '`').collect();
        match parts.len() {
            0 => {
                write!(stderr, "\n").unwrap();
                break;
            }
            1 => {
                write!(stderr, "{}\n", parts[0]).unwrap();
                break;
            }
            2 => {
                write!(stderr, "{}`{}\n", parts[0], parts[1]).unwrap();
                break;
            }
            3 => {
                write!(stderr, "{}`", parts[0]).unwrap();
                stderr.set_color(color.set_fg(Some(termcolor::Color::Yellow))).unwrap();
                write!(stderr, "{}", parts[1]).unwrap();
                stderr.set_color(color.set_fg(None)).unwrap();
                write!(stderr, "`").unwrap();
                remainder = &parts[2];
            }
            _ => panic!("unhandled split len"),
        }
    }
}
