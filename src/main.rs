#[macro_use]
extern crate lazy_static;

mod alias;
mod argset;
mod cmd;
mod commitdata;
mod edit;
mod error;
mod hook;
mod message;
mod patchdescription;
mod patchname;
mod revspec;
mod signature;
mod stack;
mod templates;
mod trailers;

use std::{ffi::OsString, io::Write};

use clap::{crate_license, crate_version, App, AppSettings};
use pyo3::prelude::*;
use termcolor::WriteColor;

fn main() {
    let maybe_repo = git2::Repository::open_from_env().ok();
    let maybe_config = if let Some(ref repo) = maybe_repo {
        repo.config()
    } else {
        git2::Config::open_default()
    }
    .ok();

    let commands = cmd::get_commands();
    let excluded = commands.keys().map(|k| *k).collect();
    let aliases = match alias::get_aliases(maybe_config.as_ref(), excluded) {
        Ok(aliases) => aliases,
        Err(e) => {
            print_error_message(e);
            std::process::exit(2);
        }
    };

    let mut app = App::new("stg")
        .about("Maintain a stack of patches on top of a Git branch.")
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::HelpRequired)
        .global_setting(AppSettings::UseLongFormatForHelpSubcommand)
        .setting(AppSettings::AllowExternalSubcommands) // TODO: remove
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version(crate_version!())
        .license(crate_license!())
        .max_term_width(88)
        .subcommand_placeholder("command", "COMMANDS")
        .subcommands(commands.values().map(|command| (command.get_app)()))
        .subcommands(
            aliases
                .iter()
                .map(|(name, alias)| alias::get_app(name, alias)),
        );

    // TODO: add top-level -C option
    // TODO: get repository and/or stack handle for subcommand

    let matches = app.get_matches_mut();
    let result: cmd::Result = if let Some((command_name, cmd_matches)) = matches.subcommand() {
        if let Some(command) = commands.get(command_name) {
            (command.run)(cmd_matches)
        } else if let Some(alias) = aliases.get(command_name) {
            match alias {
                alias::Alias::StGit(alias) => {
                    match alias.split() {
                        Ok(str_args) => {
                            let mut argv: Vec<OsString> = vec!["stg".into()];
                            argv.extend(str_args.iter().map(|s| s.into()));
                            if let Some(cmd_args) = cmd_matches.values_of_os("args") {
                                argv.extend(cmd_args.map(|v| v.into()));
                            }

                            let matches = app.get_matches_from(argv);
                            if let Some((command_name2, cmd_matches)) = matches.subcommand() {
                                if let Some(command) = commands.get(command_name2) {
                                    (command.run)(cmd_matches)
                                } else if aliases.get(command_name2).is_some() {
                                    Err(crate::error::Error::RecursiveAlias(
                                        command_name.to_string(),
                                    ))
                                } else {
                                    punt_to_python()
                                }
                            } else {
                                panic!("no subcommand for alias?")
                            }
                        }
                        Err(reason) => Err(crate::error::Error::BadAlias(
                            command_name.to_string(),
                            reason,
                        )),
                    }
                }
                alias::Alias::Shell(alias) => {
                    let args = cmd_matches.values_of_os("args").unwrap_or_default();
                    alias.run(args, maybe_repo.as_ref())
                }
            }
        } else {
            punt_to_python()
        }
    } else {
        panic!("no subcommand?")
    };

    if let Err(e) = result {
        print_error_message(e);
        std::process::exit(2)
    } else {
        std::process::exit(0)
    }
}

fn punt_to_python() -> cmd::Result {
    let result: Result<(), pyo3::PyErr> = Python::with_gil(|py| {
        let stgit_main = py.import("stgit.main")?;
        let main = stgit_main.getattr("main")?;
        let argv: Vec<String> = std::env::args().collect();
        let ret_val = main.call1((argv,))?;
        let rc = if ret_val.is_none() {
            0
        } else {
            ret_val.extract()?
        };
        unsafe { pyo3::ffi::Py_Finalize() }
        std::process::exit(rc);
    });

    if let Err(e) = result {
        Python::with_gil(|py| e.print(py));
        Err(crate::error::Error::Python(e))
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
    stderr
        .set_color(color.set_fg(Some(termcolor::Color::Red)).set_bold(true))
        .unwrap();
    write!(stderr, "error: ").unwrap();
    stderr
        .set_color(color.set_fg(None).set_bold(false))
        .unwrap();
    let err_string: String = err.to_string();
    let mut remainder: &str = &err_string;
    loop {
        let parts: Vec<&str> = remainder.splitn(3, '`').collect();
        match parts.len() {
            0 => {
                writeln!(stderr).unwrap();
                break;
            }
            1 => {
                writeln!(stderr, "{}", parts[0]).unwrap();
                break;
            }
            2 => {
                writeln!(stderr, "{}`{}", parts[0], parts[1]).unwrap();
                break;
            }
            3 => {
                write!(stderr, "{}`", parts[0]).unwrap();
                stderr
                    .set_color(color.set_fg(Some(termcolor::Color::Yellow)))
                    .unwrap();
                write!(stderr, "{}", parts[1]).unwrap();
                stderr.set_color(color.set_fg(None)).unwrap();
                write!(stderr, "`").unwrap();
                remainder = parts[2];
            }
            _ => panic!("unhandled split len"),
        }
    }
}
