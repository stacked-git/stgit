// SPDX-License-Identifier: GPL-2.0-only

//! The Stacked Git (StGit) executable, `stg`.
//!
//! StGit is a tool for maintaining a stack of patches on top of a regular Git branch.
//! Patches may be created (`stg new`), pushed (`stg push`), popped (`stg pop`), updated
//! (`stg refresh`, `stg edit`), etc. before transforming them into regular git commits
//! (using `stg commit`).

mod alias;
mod argset;
mod cmd;
mod color;
mod commit;
mod hook;
mod patchedit;
mod patchname;
mod patchrange;
mod repo;
mod revspec;
mod signal;
mod signature;
mod stack;
mod stupid;
mod templates;

use std::{ffi::OsString, io::Write, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use clap::ArgMatches;
use stupid::StupidContext;
use termcolor::WriteColor;

use self::cmd::STGIT_COMMANDS;

/// Process exit code for command line parsing errors.
const GENERAL_ERROR: i32 = 1;

/// Process exit code for errors occurring when (sub)command is executing.
const COMMAND_ERROR: i32 = 2;

/// Process exit code for when a command halts due to merge conflicts.
const CONFLICT_ERROR: i32 = 3;

/// Create base [`clap::Command`] instance.
///
/// The base [`clap::Command`] returned by this function is intended to be supplemented
/// with additional setup.
///
/// The general strategy employed here is to only compose as much of the
/// [`clap::Command`] graph as needed to execute the target subcommand; avoiding the
/// cost of instantiating [`clap::Command`] instances for every StGit subcommand.
fn get_base_command(color_choice: Option<termcolor::ColorChoice>) -> clap::Command {
    let command = clap::Command::new("stg")
        .about("Maintain a stack of patches on top of a Git branch.")
        .override_usage(
            "stg [OPTIONS] <command> [...]\n       \
             stg [OPTIONS] <-h|--help>\n       \
             stg --version",
        )
        .help_expected(true)
        .max_term_width(88)
        .disable_version_flag(true)
        .arg(
            clap::Arg::new("version")
                .long("version")
                .help("Print version information")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::Arg::new("change-dir")
                .short('C')
                .help("Run as if started in <path>")
                .long_help(
                    "Run as if stg was started in '<path>' instead of the current \
                     working directory. When multiple `-C` options are given, each \
                     subsequent non-absolute `-C <path>` is interpreted relative to \
                     the preceding `-C <path>`.\n\
                     \n\
                     This option affects arguments that expect path names or path \
                     specs in that their interpretations of the path names would be \
                     made relative to the working directory caused by the `-C` option.",
                )
                .value_parser(clap::value_parser!(PathBuf))
                .action(clap::ArgAction::Append)
                .allow_hyphen_values(true)
                .value_name("path")
                .value_hint(clap::ValueHint::AnyPath),
        )
        .arg(color::get_color_arg().global(true).display_order(998));
    if let Some(color_choice) = color_choice {
        command.color(color::termcolor_choice_to_clap(color_choice))
    } else {
        command
    }
}

/// Create [`clap::Command`] instance sufficient for finding subcommand candidates.
///
/// By using [`clap::Command::allow_external_subcommands()`], the command line arguments
/// can be quickly parsed just enough to determine whether the user has providied a valid
/// subcommand or alias, but without the cost of instantiating [`clap::Command`]
/// instances for any of subcommands or aliases.
fn get_bootstrap_command(color_choice: Option<termcolor::ColorChoice>) -> clap::Command {
    get_base_command(color_choice)
        .allow_external_subcommands(true)
        .disable_help_flag(true)
        .disable_help_subcommand(true)
        .arg(
            clap::Arg::new("help-option")
                .short('h')
                .long("help")
                .help("Print help information")
                .action(clap::ArgAction::SetTrue),
        )
}

/// Builds on the minimal Command to compose a complete top-level Command instance.
///
/// This fully formed [`clap::Command`] contains sub-[`clap::Command`] instances for
/// every StGit subcommand and alias. This flavor of [`clap::Command`] instance is
/// useful in contexts where the global help needs to be presented to the user; i.e.
/// when `--help` is provided or when the user specifies an invalid subcommand or alias.
pub(crate) fn get_full_command(
    aliases: &alias::Aliases,
    color_choice: Option<termcolor::ColorChoice>,
) -> clap::Command {
    get_base_command(color_choice)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand_value_name("command")
        .subcommands(STGIT_COMMANDS.iter().map(|command| (command.make)()))
        .subcommands(aliases.values().map(alias::Alias::make))
}

/// Main entry point for `stg` executable.
///
/// The name of the game is to dispatch to the appropriate subcommand or alias as
/// quickly as possible.
fn main() -> ! {
    let argv: Vec<OsString> = std::env::args_os().collect();

    // Chicken and egg: the --color option must be parsed from argv in order to setup
    // clap with the desired color choice. So a simple pre-parse is performed just to
    // get the color choice.
    let color_choice = color::parse_color_choice(&argv);

    if let Err(e) = self::signal::setup() {
        exit_with_result(Err(e), color_choice)
    }

    // Avoid the expense of constructing a full-blown clap::Command with all the dozens of
    // subcommands except in the few cases where that is warranted. In most cases, only
    // the Command instance of a single StGit subcommand is required.
    // First, using a minimal top-level Command instance, let clap find anything that looks
    // like a subcommand name (i.e. by using AppSettings::AllowExternalSubcommands).
    if let Ok(matches) = get_bootstrap_command(color_choice).try_get_matches_from(&argv) {
        // N.B. changing directories here, early, affects which aliases will ultimately
        // be found.
        if matches.get_flag("version") {
            execute_command(
                &self::cmd::version::STGIT_COMMAND,
                vec![argv[0].clone(), OsString::from("version")],
                color_choice,
            )
        } else if let Err(e) = change_directories(&matches) {
            exit_with_result(Err(e), color_choice)
        } else if matches.get_flag("help-option") {
            full_app_help(argv, None, color_choice)
        } else if let Some((sub_name, sub_matches)) = matches.subcommand() {
            // Initialize git2 library only in case a subcommand is matched. This limits
            // the cost of libgit2 initialization to code paths that will actually use
            // libgit2.
            unsafe { init_libgit2_extensions() }.expect("can set libgit2 extensions");

            // If the name matches any known subcommands, then only the Command for that
            // particular command is constructed and the costs of searching for aliases
            // and constructing all subcommands' Command instances are avoided.
            if let Some(command) = STGIT_COMMANDS
                .iter()
                .find(|command| command.name == sub_name)
            {
                execute_command(command, argv, color_choice)
            } else {
                // If the subcommand name does not match a builtin subcommand, the
                // aliases are located, which involves finding the Git repo and parsing
                // the various levels of config files. If the subcommand name matches an
                // alias, it is executed and the cost of constructing all subcommands'
                // Command instances is still avoided.
                match get_aliases() {
                    Err(e) => exit_with_result(Err(e), color_choice),
                    Ok((aliases, maybe_repo)) => {
                        if let Some(alias) = aliases.get(sub_name) {
                            let user_args: Vec<OsString> = sub_matches
                                .get_many::<OsString>("")
                                .map_or_else(Vec::new, |vals| vals.cloned().collect());

                            match alias.kind {
                                alias::AliasKind::Shell => execute_shell_alias(
                                    alias,
                                    user_args,
                                    color_choice,
                                    maybe_repo.as_ref(),
                                ),
                                alias::AliasKind::StGit => execute_stgit_alias(
                                    alias,
                                    &argv[0],
                                    user_args,
                                    color_choice,
                                    &aliases,
                                ),
                            }
                        } else {
                            // If no command or alias matches can be determined from the
                            // above process, then a complete clap::Command instance is
                            // constructed with all subcommand Command instances for
                            // each subcommand and alias. The command line is then
                            // re-processed by this full-blown Command instance which is
                            // expected to terminate with an appropriate help message.
                            full_app_help(argv, Some(aliases), color_choice)
                        }
                    }
                }
            }
        } else {
            full_app_help(argv, None, color_choice)
        }
    } else {
        // -C options are not processed in this branch. This is okay because clap's
        // error message will not include aliases (which depend on -C).
        full_app_help(argv, None, color_choice)
    }
}

/// Initialize [`git2`] library extensions.
///
/// By default, [`git2`] fails any operation if it encounters any unknown extensions
/// configured in a repo. StGit is uses [`git2`] library as an interface to the git
/// object database and for interrogating the git configuration.
///
/// As a workaround until `worktreeconfig` extension is supported upstream, add it to
/// whitelisted extensions.
///
/// # Safety
/// * Modifying whitelisted extensions in [`git2`] is like modifying a static mut
///   variable (unsafe).
/// * This function must be called at initialization time (before git2 library usage)
///   and should be called only once.
unsafe fn init_libgit2_extensions() -> Result<()> {
    git2::opts::set_extensions(&["worktreeconfig"]).context("set `worktreeconfig` extension")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_libgit2_extensions() {
        unsafe { init_libgit2_extensions() }.unwrap();
    }
}

/// Exit the program based on the provided [`Result`].
///
/// Error results from conflicts trigger merge conflicts to be printed and an exit code
/// of [`CONFLICT_ERROR`].
fn exit_with_result(result: Result<()>, color_choice: Option<termcolor::ColorChoice>) -> ! {
    let code = match result {
        Ok(()) => 0,
        Err(e) => {
            print_error_message(color_choice, &e);
            match e.downcast_ref::<stack::Error>() {
                Some(stack::Error::TransactionHalt { conflicts, .. }) => {
                    if *conflicts {
                        print_merge_conflicts();
                    }
                    CONFLICT_ERROR
                }
                Some(stack::Error::CheckoutConflicts(_))
                | Some(stack::Error::CausedConflicts(_)) => CONFLICT_ERROR,
                _ => COMMAND_ERROR,
            }
        }
    };
    std::process::exit(code)
}

/// Change the current directory based on any -C options from the top-level Command matches.
///
/// Each -C path is relative to the prior. Empty paths are allowed, but ignored.
fn change_directories(matches: &ArgMatches) -> Result<()> {
    if let Some(paths) = matches.get_many::<PathBuf>("change-dir") {
        for path in paths.filter(|&p| !p.as_os_str().is_empty()) {
            std::env::set_current_dir(path)
                .with_context(|| format!("cannot change to `{}`", path.to_string_lossy()))?;
        }
    }
    Ok(())
}

/// Display the help for the fully-instantiated top-level [`clap::Command`].
///
/// Process argv using full top-level [`clap::Command`] instance with the expectation
/// that argv was previously determined to be invalid. The full command can then output
/// a help message with a complete view of all subcommands and aliases, and then
/// terminate to process.
fn full_app_help(
    argv: Vec<OsString>,
    aliases: Option<alias::Aliases>,
    color_choice: Option<termcolor::ColorChoice>,
) -> ! {
    let aliases = if let Some(aliases) = aliases {
        aliases
    } else {
        match get_aliases() {
            Ok((aliases, _)) => aliases,
            Err(e) => exit_with_result(Err(e), color_choice),
        }
    };

    // full_app_help should only be called once it has been determined that the command
    // line does not have a viable subcommand or alias. Thus this get_matches_from()
    // call should print an appropriate help message and terminate the process.
    let err = get_full_command(&aliases, color_choice)
        .try_get_matches_from(argv)
        .expect_err("command line should not have viable matches");
    err.print().expect("failed to print clap error");
    std::process::exit(if err.use_stderr() { GENERAL_ERROR } else { 0 })
}

/// Execute regular StGit subcommand.
///
/// The particular subcommand name must have previously been matched in argv such that
/// it is guaranteed to be matched again here.
///
/// N.B. a new top-level app instance is created to ensure that help messages are
/// formatted using the correct executable path (from `argv[0]`).
fn execute_command(
    command: &cmd::StGitCommand,
    argv: Vec<OsString>,
    color_choice: Option<termcolor::ColorChoice>,
) -> ! {
    match get_base_command(color_choice)
        .subcommand((command.make)())
        .try_get_matches_from(argv)
    {
        Ok(top_matches) => {
            let (_sub_name, sub_matches) = top_matches
                .subcommand()
                .expect("this subcommand is already known to be in argv");
            exit_with_result((command.run)(sub_matches), color_choice)
        }

        Err(err) => {
            err.print().expect("clap can print its error message");
            std::process::exit(if err.use_stderr() { GENERAL_ERROR } else { 0 })
        }
    }
}

/// Execute shell alias subprocess.
///
/// If the child process fails, the parent process will be terminated, returning the
/// child's return code.
fn execute_shell_alias(
    alias: &alias::Alias,
    user_args: Vec<OsString>,
    color_choice: Option<termcolor::ColorChoice>,
    repo: Option<&git2::Repository>,
) -> ! {
    if let Some(first_arg) = user_args.first() {
        if [OsString::from("-h"), OsString::from("--help")].contains(first_arg) {
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
            command.current_dir(workdir);
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
                if rel_dir == PathBuf::from(".git") {
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

    match command.status().with_context(|| {
        format!(
            "while expanding shell alias `{}`: `{}`",
            alias.name, alias.command
        )
    }) {
        Ok(status) => std::process::exit(status.code().unwrap_or(-1)),
        Err(e) => exit_with_result(Err(e), color_choice),
    }
}

/// Execute alias to StGit command.
///
/// Recursive aliases are detected.
fn execute_stgit_alias(
    alias: &alias::Alias,
    exec_path: &OsString,
    user_args: Vec<OsString>,
    color_choice: Option<termcolor::ColorChoice>,
    aliases: &alias::Aliases,
) -> ! {
    let result = match alias.split() {
        Ok(alias_args) => {
            if let Some(first_user_arg) = user_args.first() {
                if [OsString::from("-h"), OsString::from("--help")].contains(first_user_arg) {
                    eprintln!("'{}' is aliased to '{}'", &alias.name, &alias.command);
                }
            }

            let mut user_args = user_args;
            let mut argv: Vec<OsString> =
                Vec::with_capacity(1 + alias_args.len() + user_args.len());
            argv.push(exec_path.clone());
            argv.extend(alias_args.iter().map(OsString::from));
            argv.append(&mut user_args);

            let resolved_cmd_name = alias_args
                .first()
                .expect("empty aliases are filtered in get_aliases()")
                .as_str();

            if let Some(command) = STGIT_COMMANDS
                .iter()
                .find(|command| command.name == resolved_cmd_name)
            {
                execute_command(command, argv, color_choice)
            } else if aliases.contains_key(resolved_cmd_name) {
                Err(anyhow!("Recursive alias `{}`", alias.name))
            } else {
                Err(anyhow!(
                    "Bad alias for `{}`: `{resolved_cmd_name}` is not a stg command",
                    alias.name,
                ))
            }
        }
        Err(reason) => Err(anyhow!("Bad alias for `{}`: {reason}", alias.name)),
    };

    exit_with_result(result, color_choice)
}

/// Get aliases mapping.
///
/// Since aliases are defined in git config files, an attempt is made to open a repo so
/// that its local config can be inspected along with the user global and system
/// configs.
///
/// N.B. the outcome of this alias search depends on the current directory and thus
/// depends on -C options having been previously processed.
pub(crate) fn get_aliases() -> Result<(alias::Aliases, Option<git2::Repository>)> {
    let maybe_repo = git2::Repository::open_from_env().ok();
    maybe_repo
        .as_ref()
        .map_or_else(git2::Config::open_default, git2::Repository::config)
        .map_or_else(
            |_| Ok(alias::get_default_aliases()),
            |config| {
                alias::get_aliases(&config, |name| {
                    STGIT_COMMANDS.iter().any(|command| command.name == name) || name == "help"
                })
            },
        )
        .map(|aliases| (aliases, maybe_repo))
}

/// Print user-facing message to stderr.
///
/// Any parts of `msg` enclosed in backticks (``) are highlighted in yellow.
fn print_message(
    label: &str,
    label_color: termcolor::Color,
    stderr: &mut termcolor::StandardStream,
    msg: &str,
) {
    let mut color = termcolor::ColorSpec::new();
    stderr
        .set_color(color.set_fg(Some(label_color)).set_bold(true))
        .unwrap();
    write!(stderr, "{label}: ").unwrap();
    stderr
        .set_color(color.set_fg(None).set_bold(false))
        .unwrap();
    let mut remainder: &str = msg;
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

/// Print user-facing informational message to stderr.
pub(crate) fn print_info_message(matches: &ArgMatches, msg: &str) {
    let mut stderr = color::get_color_stderr(matches);
    print_message("info", termcolor::Color::Blue, &mut stderr, msg);
}

/// Print user-facing warning message to stderr.
pub(crate) fn print_warning_message(matches: &ArgMatches, msg: &str) {
    let mut stderr = color::get_color_stderr(matches);
    print_message("warning", termcolor::Color::Yellow, &mut stderr, msg);
}

/// Print user-facing error message to stderr.
fn print_error_message(color_choice: Option<termcolor::ColorChoice>, err: &anyhow::Error) {
    use is_terminal::IsTerminal;
    let color_choice = color_choice.unwrap_or_else(|| {
        if std::io::stderr().is_terminal() {
            termcolor::ColorChoice::Auto
        } else {
            termcolor::ColorChoice::Never
        }
    });
    let mut stderr = termcolor::StandardStream::stderr(color_choice);
    let err_string = format!("{err:#}");
    print_message("error", termcolor::Color::Red, &mut stderr, &err_string);
}

/// Print file names with merge conflicts to stdout.
// TODO: this should print to stderr instead.
fn print_merge_conflicts() {
    let stupid = StupidContext::default();
    let cdup: OsString = stupid
        .rev_parse_cdup()
        .unwrap_or_else(|_| OsString::from(""));
    let conflicts: Vec<OsString> = stupid.diff_unmerged_names().unwrap_or_else(|_| Vec::new());
    let pathspecs = if cdup.is_empty() {
        conflicts
    } else {
        let mut pathspecs: Vec<OsString> = Vec::new();
        let cdup = std::path::Path::new(&cdup);
        for conflict in conflicts {
            let path: OsString = cdup.join(conflict).into();
            pathspecs.push(path);
        }
        pathspecs
    };
    stupid.status_short(Some(pathspecs)).unwrap_or_default();
}
