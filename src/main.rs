#[macro_use]
extern crate lazy_static;

mod alias;
mod argset;
mod cmd;
mod color;
mod commit;
mod hook;
mod index;
mod patchedit;
mod patchname;
mod patchrange;
mod pathspec;
mod revspec;
mod signature;
mod stack;
mod stupid;
mod templates;

use std::{
    ffi::{OsStr, OsString},
    io::Write,
};

use anyhow::{anyhow, Context, Result};
use clap::{crate_version, App, AppSettings, ArgMatches, ValueHint};
use termcolor::WriteColor;

const GENERAL_ERROR: i32 = 1;
const COMMAND_ERROR: i32 = 2;
const CONFLICT_ERROR: i32 = 3;

fn get_base_app() -> App<'static> {
    App::new("stg")
        .about("Maintain a stack of patches on top of a Git branch.")
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::HelpExpected)
        .max_term_width(88)
        .arg(
            clap::Arg::new("change_dir")
                .short('C')
                .help("Run as if started in PATH")
                .setting(clap::ArgSettings::AllowInvalidUtf8)
                .setting(clap::ArgSettings::MultipleOccurrences)
                .setting(clap::ArgSettings::AllowHyphenValues)
                .value_name("PATH")
                .value_hint(ValueHint::AnyPath),
        )
        .arg(color::get_color_arg().global(true))
}

/// Just enough of an App instance to find candidate subcommands
fn get_bootstrap_app() -> App<'static> {
    get_base_app()
        .setting(AppSettings::AllowExternalSubcommands)
        .setting(AppSettings::DisableHelpFlag)
        .setting(AppSettings::DisableHelpSubcommand)
        .setting(AppSettings::AllowInvalidUtf8ForExternalSubcommands)
        .arg(
            clap::Arg::new("help-option")
                .short('h')
                .long("help")
                .help("Print help information"),
        )
}

/// Builds on the minimal App to compose a complete top-level App instance.
fn get_full_app(commands: cmd::Commands, aliases: alias::Aliases) -> App<'static> {
    get_base_app()
        .version(crate_version!())
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::UseLongFormatForHelpSubcommand)
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand_help_heading("COMMANDS")
        .subcommand_value_name("command")
        .subcommands(commands.values().map(|command| (command.get_app)()))
        .subcommands(aliases.values().map(|alias| alias.get_app()))
        .subcommands(
            cmd::PYTHON_COMMANDS
                .iter()
                .map(|name| App::new(*name).about("Implemented in Python")),
        )
}

fn main() {
    let argv: Vec<OsString> = std::env::args_os().collect();
    let commands = cmd::get_commands();

    // Avoid the expense of constructing a full-blown clap::App with all the dozens of
    // subcommands except in the few cases where that is warranted. In most cases, only
    // the App instance of a single StGit subcommand is required.
    let app = get_bootstrap_app();

    // First, using a minimal top-level App instance, let clap find anything that looks
    // like a subcommand name (i.e. by using AppSettings::AllowExternalSubcommands).
    let result = if let Ok(matches) = app.try_get_matches_from(&argv) {
        // N.B. changing directories here, early, affects which aliases will
        // ultimately be found.
        if let Err(e) = change_directories(&matches) {
            Err(e)
        } else if matches.is_present("help-option") {
            full_app_help(argv, commands, None)
        } else if let Some((sub_name, sub_matches)) = matches.subcommand() {
            // If the name matches any known commands, then only the App for that
            // particular command is constructed and the costs of searching for aliases
            // and constructing all commands' App instances are avoided.
            if let Some(command) = commands.get(sub_name) {
                execute_command(command, argv)
            } else if cmd::PYTHON_COMMANDS.contains(&sub_name) {
                punt_to_python()
            } else {
                // If the subcommand name does not match a known command, the aliases
                // are located, which involves finding the Git repo and parsing the
                // various levels of config files. If the subcommand name matches an
                // alias, it is executed and the cost of constructing all commands' App
                // instances is still avoided.
                match get_aliases(&commands) {
                    Ok((aliases, maybe_repo)) => {
                        if let Some(alias) = aliases.get(sub_name) {
                            let user_args: Vec<&OsStr> =
                                if let Some(args) = sub_matches.values_of_os("") {
                                    args.collect()
                                } else {
                                    Vec::new()
                                };

                            match alias.kind {
                                alias::AliasKind::Shell => {
                                    execute_shell_alias(alias, &user_args, maybe_repo.as_ref())
                                }
                                alias::AliasKind::StGit => execute_stgit_alias(
                                    alias, &argv[0], &user_args, &commands, &aliases,
                                ),
                            }
                        } else {
                            // If no command or alias matches can be determined from the
                            // above process, then a complete clap::App instance is
                            // constructed with all subcommand App instances for each
                            // command and alias. The command line is then re-processed
                            // by this full-blown App instance which is expected to
                            // terminate with an appropriate help message.
                            full_app_help(argv, commands, Some(aliases))
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        } else {
            full_app_help(argv, commands, None)
        }
    } else {
        // -C options not processed in this branch. This is okay because clap's error
        // message will not include aliases (which depend on -C) anyway.
        full_app_help(argv, commands, None)
    };

    let code = match result {
        Ok(()) => 0,
        Err(e) => {
            let code = match e.downcast_ref::<stack::Error>() {
                Some(stack::Error::TransactionHalt(_) | stack::Error::CheckoutConflicts(_)) => {
                    CONFLICT_ERROR
                }
                _ => COMMAND_ERROR,
            };
            print_error_message(e);
            code
        }
    };
    std::process::exit(code);
}

/// Change the current directory based on any -C options from the top-level App matches.
/// Each -C path is relative to the prior. Empty paths are allowed, but ignored.
fn change_directories(matches: &ArgMatches) -> Result<()> {
    if let Some(paths) = matches.values_of_os("change_dir") {
        for path in paths.filter(|p| !p.is_empty()) {
            std::env::set_current_dir(path)
                .with_context(|| format!("cannot change to `{}`", path.to_string_lossy()))?;
        }
    }
    Ok(())
}

/// Process argv using full top-level App instance with the expectation that argv is
/// somehow invalid. The full App can then output a help message with a complete view of
/// all commands and aliases, and then terminate to process.
fn full_app_help(
    argv: Vec<OsString>,
    commands: cmd::Commands,
    aliases: Option<alias::Aliases>,
) -> Result<()> {
    let aliases = if let Some(aliases) = aliases {
        aliases
    } else {
        match get_aliases(&commands) {
            Ok((aliases, _)) => aliases,
            Err(e) => return Err(e),
        }
    };

    // full_app_help should only be called once it has been determined that the command
    // line does not have a viable subcommand or alias. Thus this get_matches_from()
    // call should print an appropriate help message and terminate the process.
    let err = get_full_app(commands, aliases)
        .try_get_matches_from(&argv)
        .expect_err("command line should not have viable matches");
    err.print().expect("failed to print clap error");
    std::process::exit(if err.use_stderr() { GENERAL_ERROR } else { 0 })
}

/// Execute regular StGit subcommand. The particular command must have previously been
/// matched in argv such that it is guaranteed to be matched again here.
/// N.B. a new top-level app instance is created to ensure that help messages are
/// formatted using the correct executable path (`argv[0]`).
fn execute_command(command: &cmd::StGitCommand, argv: Vec<OsString>) -> Result<()> {
    let top_app = get_base_app().subcommand((command.get_app)());
    match top_app.try_get_matches_from(argv) {
        Ok(top_matches) => {
            let (_, cmd_matches) = top_matches
                .subcommand()
                .expect("this command is ensured to be the only subcommand");
            (command.run)(cmd_matches)
        }

        Err(err) => {
            err.print().expect("failed to print clap error");
            std::process::exit(if err.use_stderr() { GENERAL_ERROR } else { 0 })
        }
    }
}

/// Execute shell alias subprocess. If the child process fails, the parent process will
/// be terminated, returning the child's return code.
fn execute_shell_alias(
    alias: &alias::Alias,
    user_args: &[&OsStr],
    repo: Option<&git2::Repository>,
) -> Result<()> {
    if let Some(first_arg) = user_args.get(0) {
        if ["-h", "--help"].contains(&first_arg.to_str().unwrap_or("")) {
            eprintln!("'{}' is aliased to '!{}'", &alias.name, &alias.command);
        }
    }

    // TODO: Git chooses its shell path at compile time based on OS or user override.
    let shell_path = "sh";
    let shell_chars = &[
        '|', '&', ';', '<', '>', '(', ')', '$', '`', ' ', '*', '?', '[', '#', '~', '=', '%', '\\',
        '"', '\'', '\t', '\n',
    ];

    let mut command = if alias.command.find(shell_chars).is_some() {
        // Need to wrap in shell command
        let mut command = std::process::Command::new(shell_path);
        command.arg("-c");
        if user_args.is_empty() {
            command.arg(&alias.command);
        } else {
            command.arg(&format!("{} \"$@\"", &alias.command));
            command.arg(&alias.command);
        }
        command
    } else {
        std::process::Command::new(&alias.command)
    };
    command.args(user_args);

    if let Some(repo) = repo {
        if let Some(workdir) = repo.workdir() {
            if let Ok(cur_dir) = std::env::current_dir() {
                if let Ok(prefix) = cur_dir.strip_prefix(workdir) {
                    if !cur_dir.starts_with(repo.path()) {
                        let mut prefix = prefix.as_os_str().to_os_string();
                        if !prefix.is_empty() {
                            prefix.push("/");
                        }
                        command.env("GIT_PREFIX", &prefix);
                    }
                }
            }
            if let Ok(rel_dir) = repo.path().strip_prefix(workdir) {
                if rel_dir == std::path::PathBuf::from(".git") {
                    command.env_remove("GIT_DIR");
                } else {
                    command.env("GIT_DIR", rel_dir);
                }
            } else {
                command.env("GIT_DIR", repo.path());
            }
        } else {
            command.env("GIT_DIR", repo.path());
        }
    }

    let status = command.status().with_context(|| {
        format!(
            "while expanding shell alias `{}`: `{}`",
            alias.name, alias.command
        )
    })?;
    if status.success() {
        Ok(())
    } else {
        std::process::exit(status.code().unwrap_or(-1));
    }
}

/// Execute alias to StGit command. Recursive aliases are detected.
fn execute_stgit_alias(
    alias: &alias::Alias,
    exec_path: &OsString,
    user_args: &[&OsStr],
    commands: &cmd::Commands,
    aliases: &alias::Aliases,
) -> Result<()> {
    match alias.split() {
        Ok(alias_args) => {
            let mut new_argv: Vec<OsString> =
                Vec::with_capacity(1 + alias_args.len() + user_args.len());
            new_argv.push(exec_path.clone());
            new_argv.extend(alias_args.iter().map(OsString::from));
            new_argv.extend(user_args.iter().map(OsString::from));

            let resolved_cmd_name = alias_args
                .first()
                .expect("empty aliases are filtered in get_aliases()")
                .as_str();

            if let Some(first_user_arg) = user_args.get(0) {
                if ["-h", "--help"].contains(&first_user_arg.to_str().unwrap_or("")) {
                    eprintln!("'{}' is aliased to '{}'", &alias.name, &alias.command);
                }
            }

            if let Some(command) = commands.get(resolved_cmd_name) {
                execute_command(command, new_argv)
            } else if aliases.contains_key(resolved_cmd_name) {
                Err(anyhow!("recursive alias `{}`", alias.name))
            } else {
                Err(anyhow!(
                    "bad alias for `{}`: `{}` is not a stg command",
                    alias.name,
                    resolved_cmd_name
                ))
            }
        }
        Err(reason) => Err(anyhow!("bad alias for `{}`: {}", alias.name, reason)),
    }
}

/// Get aliases mapping. Since aliases are defined in git config files, an attempt is
/// made to open a repo so that its local config can be inspected along with the user
/// global and system configs.
/// N.B. the outcome of this alias search depends on the current directory and thus
/// depends on -C options being processed.
fn get_aliases(commands: &cmd::Commands) -> Result<(alias::Aliases, Option<git2::Repository>)> {
    let maybe_repo = git2::Repository::open_from_env().ok();
    let maybe_config = if let Some(ref repo) = maybe_repo {
        repo.config()
    } else {
        git2::Config::open_default()
    }
    .ok();
    let excluded = commands.keys().chain(&["help"]).copied().collect();
    alias::get_aliases(maybe_config.as_ref(), excluded).map(|aliases| (aliases, maybe_repo))
}

fn punt_to_python() -> Result<()> {
    let status = std::process::Command::new("python")
        .args(["-m", "stgit"])
        .args(std::env::args_os().skip(1))
        .status()
        .context("failed to run python")?;
    std::process::exit(status.code().unwrap_or(-1));
}

fn print_error_message(err: anyhow::Error) {
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
    let err_string = format!("{:#}", err);
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
