// SPDX-License-Identifier: GPL-2.0-only

//! `stg branch --list` implementation.

use std::io::Write;

use anyhow::Result;
use bstr::ByteSlice;
use termcolor::WriteColor;

use crate::{
    ext::RepositoryExtended,
    stack::{InitializationPolicy, Stack},
    wrap::Branch,
};

pub(super) fn command() -> clap::Command {
    clap::Command::new("--list")
        .short_flag('l')
        .override_usage("stg branch {--list,-l}")
        .about("List branches in this repository")
        .long_about(
            "List each branch in the current repository along with its description, if \
             any. The current branch is prefixed with '>'. Branches initialized with \
             StGit stacks are prefixed with 's'. Protected branches are prefixed with \
             'p'.",
        )
}

pub(super) fn dispatch(repo: &gix::Repository, matches: &clap::ArgMatches) -> Result<()> {
    let mut branchnames = Vec::new();
    for local_branch in repo.references()?.local_branches()?.filter_map(Result::ok) {
        let local_branch = Branch::wrap(local_branch);
        if let Ok(branchname) = local_branch.get_branch_partial_name() {
            if !branchname.as_ref().ends_with(".stgit") {
                branchnames.push(branchname);
            }
        }
    }

    branchnames.sort();
    let branchname_width = branchnames.iter().map(|name| name.as_ref().len()).max();

    let current_branch = repo.get_current_branch().ok();
    let current_branchname = current_branch
        .as_ref()
        .and_then(|branch| branch.get_branch_partial_name().ok());

    let config = repo.config_snapshot();

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    for branchname in &branchnames {
        let is_current = Some(branchname) == current_branchname.as_ref();

        if is_current {
            stdout.set_color(color_spec.set_intense(true))?;
            write!(stdout, "> ")?;
            color_spec.clear();
            stdout.set_color(&color_spec)?;
        } else {
            write!(stdout, "  ")?;
        };

        if let Ok(stack) =
            Stack::from_branch_name(repo, branchname, InitializationPolicy::RequireInitialized)
        {
            color_spec.set_fg(Some(termcolor::Color::Cyan));
            stdout.set_color(&color_spec)?;
            write!(stdout, "s")?;
            color_spec.clear();
            stdout.set_color(&color_spec)?;
            if stack.is_protected(&config) {
                color_spec.set_fg(Some(termcolor::Color::Yellow));
                stdout.set_color(&color_spec)?;
                write!(stdout, "p\t")?;
                color_spec.clear();
                stdout.set_color(&color_spec)?;
            } else {
                write!(stdout, " \t")?;
            }
        } else {
            write!(stdout, "  \t")?;
        }

        if is_current {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Green)))?;
        }
        let branchname_width = branchname_width.expect("max is Some when !branchnames.is_empty()");
        write!(stdout, "{branchname:branchname_width$}")?;
        if is_current {
            color_spec.clear();
            stdout.set_color(&color_spec)?;
        }

        color_spec.set_dimmed(true);
        stdout.set_color(&color_spec)?;
        write!(stdout, "  |")?;
        color_spec.clear();
        stdout.set_color(&color_spec)?;

        let description = config
            .plumbing()
            .string("branch", Some(branchname.into()), "description")
            .unwrap_or_default();
        if description.is_empty() {
            writeln!(stdout)?;
        } else {
            write!(stdout, " ")?;
            stdout.write_all(description.as_bstr())?;
            writeln!(stdout)?;
        }
    }

    Ok(())
}
