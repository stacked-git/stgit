use clap::{App, Arg, ArgMatches, ValueHint};

use crate::{error::Error, revspec::parse_stgit_revision};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("diff", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("diff")
        .about("Show a diff")
        .long_about(
            "Show the diff (default) or diffstat between the current working copy \
             or a tree-ish object and another tree-ish object (defaulting to HEAD). \
             File names can also be given to restrict the diff output. The \
             tree-ish object has the format accepted by the 'stg id' command.",
        )
        .arg(
            Arg::new("pathspecs")
                .help("Limit diff to files matching path(s)")
                .value_name("path")
                .multiple_values(true)
                .allow_invalid_utf8(true)
                .forbid_empty_values(true)
                .value_hint(ValueHint::AnyPath),
        )
        .arg(
            Arg::new("range")
                .long("range")
                .short('r')
                .help("Show the diff between specified revisions")
                .long_help(
                    "Show diff between specified revisions. \
                     Revisions ranges are specified as 'rev1[..[rev2]]'. \
                     The revisions may be standard Git revision specifiers or \
                     patches.",
                )
                .value_name("revspec"),
        )
        .arg(
            Arg::new("stat")
                .long("stat")
                .short('s')
                .help("Show the stat instead of the diff"),
        )
        .arg(&*crate::argset::DIFF_OPTS_ARG)
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;

    let mut diff_cmd = std::process::Command::new("git");
    diff_cmd.arg("diff");

    if matches.is_present("stat") {
        diff_cmd.args(["--stat", "--summary"]);
    }

    if let Some(diff_opts) = matches.value_of("diff-opts") {
        for opt in diff_opts.split_ascii_whitespace() {
            diff_cmd.arg(opt);
        }
    }

    if let Some(range_str) = matches.value_of("range") {
        if let Some((rev1, rev2)) = range_str.split_once("..") {
            if rev1.is_empty() {
                return Err(Error::InvalidRevision(range_str.to_string()));
            }
            let rev1 = parse_stgit_revision(&repo, Some(rev1), None)?;
            if rev2.is_empty() {
                diff_cmd.arg(format!("{}..", rev1));
            } else {
                let rev2 = parse_stgit_revision(&repo, Some(rev2), None)?;
                diff_cmd.arg(format!("{}..{}", rev1, rev2));
            }
        } else {
            let rev1 = parse_stgit_revision(&repo, Some(range_str), None)?;
            diff_cmd.arg(rev1.to_string());
        }
    } else {
        diff_cmd.arg("HEAD");
    };

    if let Some(pathspecs) = matches.values_of_os("pathspecs") {
        diff_cmd.arg("--");
        diff_cmd.args(pathspecs);
    }

    diff_cmd.stdin(std::process::Stdio::null());
    let status = diff_cmd.status().map_err(Error::GitExecute)?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::GitCommand(
            "git diff".to_string(),
            format!("failed with code {}", status.code().unwrap_or(-1)),
        ))
    }
}
