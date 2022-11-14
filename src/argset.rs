// SPDX-License-Identifier: GPL-2.0-only

//! [`clap::Arg`] definitions common to several StGit commands.

use clap::Arg;

/// The `--branch`/`-b` option for selecting an alternative branch.
pub(crate) fn branch_arg() -> Arg {
    Arg::new("branch")
        .long("branch")
        .short('b')
        .help("Use BRANCH instead of current branch")
        .num_args(1)
        .value_name("branch")
        .value_hint(clap::ValueHint::Other)
        .value_parser(parse_branch_name)
}

/// The `--keep/-k` option.
pub(crate) fn keep_arg() -> Arg {
    Arg::new("keep")
        .long("keep")
        .short('k')
        .help("Keep the local changes")
        .action(clap::ArgAction::SetTrue)
}

/// The `--merged` option checking for already-merged patches before pushes.
pub(crate) fn merged_arg() -> Arg {
    Arg::new("merged")
        .long("merged")
        .short('m')
        .help("Check for patches merged upstream")
        .action(clap::ArgAction::SetTrue)
}

/// The `--diff-opt`/`-O` option for pass-through to subordinate `git` processes.
pub(crate) fn diff_opts_arg() -> Arg {
    Arg::new("git-diff-opt")
        .long("diff-opt")
        .alias("diff-opts")
        .short('O')
        .help("Pass additional <option> to `git diff`")
        .long_help(
            "Pass additional <option> to `git diff`.\n\
             \n\
             See the git-diff(1) man page. This option may be specified multiple \
             times.",
        )
        .num_args(1)
        .allow_hyphen_values(true)
        .action(clap::ArgAction::Append)
        .value_name("option")
        .value_hint(clap::ValueHint::Other)
}

/// For use with `clap::Arg::value_parser()` to ensure a branch name is valid.
pub(crate) fn parse_branch_name(name: &str) -> anyhow::Result<String> {
    if git2::Branch::name_is_valid(name)? {
        Ok(name.to_string())
    } else {
        Err(anyhow::anyhow!("invalid branch name `{name}`"))
    }
}

/// Get a `&str` from a `clap::ArgMatches` instance for the given `id`.
///
/// This function may be cleaner than calling `ArgMatches::get_one::<String>()` directly
/// since that function returns `Option<&String>` which often needs to be mapped to
/// `Option<&str>`.
pub(crate) fn get_one_str<'a>(matches: &'a clap::ArgMatches, id: &str) -> Option<&'a str> {
    matches.get_one::<String>(id).map(String::as_str)
}

/// For use with `clap::Arg::value_parser()` to parse a usize argument.
///
/// This function has a custom error message that is preferable to the messages reported
/// by `str::parse::<usize>()`.
pub(crate) fn parse_usize(s: &str) -> anyhow::Result<usize> {
    s.parse::<usize>()
        .map_err(|_| anyhow::anyhow!("'{s}' is not a positive integer"))
}

/// Compose aggregate set of git diff options from various sources.
///
/// These options are meant to be passed to various subordinate `git` commands that take
/// diff options.
///
/// The base set of options come from `stgit.diff-opts` in the config. Additional
/// options from `--diff-opt`/`-O` command line options are appended. And StGit
/// command-specific policies for displaying the full object id (`--full-index`) and
/// including binary diffs (`--binary`) are tacked on at the end.
///
/// The returned Vec<String> is appropriate for inserting directly into the command line
/// of subordinate `git` commands.
pub(crate) fn get_diff_opts(
    matches: &clap::ArgMatches,
    config: &git2::Config,
    force_full_index: bool,
    force_binary: bool,
) -> Vec<String> {
    let mut opts = Vec::new();

    if let Ok(value) = config.get_string("stgit.diff-opts") {
        for arg in value.split_ascii_whitespace() {
            opts.push(String::from(arg));
        }
    }

    if let Some(values) = matches.get_many::<String>("git-diff-opt") {
        opts.extend(values.cloned());
    }

    if force_full_index {
        opts.push(String::from("--full-index"));
    }

    if force_binary {
        opts.push(String::from("--binary"));
    }

    opts
}
