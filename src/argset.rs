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
        .value_name("BRANCH")
        .value_hint(ValueHint::Other);

    /// The `--keep` option.
    pub(crate) static ref KEEP_ARG: Arg<'static> =
        Arg::new("keep").long("keep").help("Keep the local changes");

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
        .value_name("OPTIONS")
        .value_hint(ValueHint::Other);
}

pub(crate) fn get_diff_opts(
    matches: &clap::ArgMatches,
    force_full_index: bool,
    force_binary: bool,
) -> Vec<&str> {
    let mut opts = Vec::new();
    if let Some(values) = matches.values_of("diff-opts") {
        for value in values {
            for arg in value.split_ascii_whitespace() {
                opts.push(arg)
            }
        }
    }
    if force_full_index {
        opts.push("--full-index")
    }
    if force_binary {
        opts.push("--binary");
    }
    opts
}
