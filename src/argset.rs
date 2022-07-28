// SPDX-License-Identifier: GPL-2.0-only

//! [`clap::Arg`] definitions common to several StGit commands.

use clap::{Arg, ValueHint};

lazy_static! {
    /// The `--branch`/`-b` option for selecting an alternative branch.
    pub(crate) static ref BRANCH_ARG: Arg<'static> = Arg::new("branch")
        .long("branch")
        .short('b')
        .help("Use BRANCH instead of current branch")
        .takes_value(true)
        .value_name("branch")
        .value_hint(ValueHint::Other);

    /// The `--keep/-k` option.
    pub(crate) static ref KEEP_ARG: Arg<'static> =
        Arg::new("keep").long("keep").short('k').help("Keep the local changes");

    /// The `--merged` option checking for already-merged patches before pushes.
    pub(crate) static ref MERGED_ARG: Arg<'static> = Arg::new("merged")
        .long("merged")
        .short('m')
        .help("Check for patches merged upstream");

    /// The `--diff-opts`/`-O` option for pass-through to subordinate `git` processes.
    pub(crate) static ref DIFF_OPTS_ARG: Arg<'static> = Arg::new("diff-opts")
        .long("diff-opts")
        .short('O')
        .help("Extra options to pass to \"git diff\"")
        .takes_value(true)
        .allow_hyphen_values(true)
        .multiple_occurrences(true)
        .value_name("options")
        .value_hint(ValueHint::Other);
}

/// Compose aggregate set of git diff options from various sources.
///
/// These options are meant to be passed to various subordinate `git` commands that take
/// diff options.
///
/// The base set of options come from `stgit.diff-opts` in the config. Additional
/// options from `--diff-opts`/`-O` command line options are appended. And StGit
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
            opts.push(String::from(arg))
        }
    }

    if let Some(values) = matches.values_of("diff-opts") {
        for value in values {
            for arg in value.split_ascii_whitespace() {
                opts.push(String::from(arg))
            }
        }
    }

    if force_full_index {
        opts.push(String::from("--full-index"))
    }

    if force_binary {
        opts.push(String::from("--binary"));
    }

    opts
}
