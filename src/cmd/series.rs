// SPDX-License-Identifier: GPL-2.0-only

//! `stg series` implementation.

use std::{io::Write, str::FromStr};

use anyhow::{anyhow, Result};
use clap::{Arg, ArgGroup, ArgMatches, ValueHint};
use termcolor::WriteColor;

use crate::{
    argset,
    commit::CommitExtended,
    patchname::PatchName,
    patchrange,
    stack::{Stack, StackStateAccess},
};

const UNPRINTABLE: &str = "???";

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "series",
    category: super::CommandCategory::StackInspection,
    make,
    run,
};

fn make() -> clap::Command<'static> {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Display the patch series")
        .long_about(
            "Show all the patches in the series, or just those in the \
             given range, ordered from top to bottom.\n\
             \n\
             The applied patches are prefixed with a '+' (except the \
             current patch, which is prefixed with a '>'), the \
             unapplied patches with a '-', and the hidden patches with \
             a '!'.\n\
             \n\
             Empty patches are prefixed with a '0'.",
        )
        .override_usage(
            "stg series [OPTIONS] [-A] [-U] [-H]\n    \
             stg series [OPTIONS] --all\n    \
             stg series [OPTIONS] --short\n    \
             stg series [OPTIONS] <patch>...",
        )
        .arg(
            Arg::new("patchranges-all")
                .help("Patches to display")
                .value_name("patch")
                .multiple_values(true)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with_all(&["all", "applied", "unapplied", "hidden", "short"]),
        )
        .arg(argset::branch_arg())
        .next_help_heading("SELECT OPTIONS")
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Select all patches, including hidden patches")
                .conflicts_with_all(&["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("short")
                .long("short")
                .short('s')
                .help("Select patches around the topmost patch only"),
        )
        .group(ArgGroup::new("all-short-group").args(&["all", "short"]))
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Select the applied patches only"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Select the unapplied patches only"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Select the hidden patches only"),
        )
        .arg(
            Arg::new("missing")
                .long("missing")
                .short('m')
                .help("Select patches in <branch> not present in current branch")
                .takes_value(true)
                .value_name("branch")
                .value_hint(ValueHint::Other),
        )
        .next_help_heading("DISPLAY OPTIONS")
        .arg(
            Arg::new("author")
                .long("author")
                .help("Display author name for each patch"),
        )
        .arg(
            Arg::new("count")
                .long("count")
                .short('c')
                .help("Display the number of selected patches and exit")
                .conflicts_with_all(&[
                    "description",
                    "author",
                    "empty",
                    "show-branch",
                    "no-prefix",
                ]),
        )
        .arg(
            Arg::new("commit-id")
                .long("commit-id")
                .short('i')
                .help("Display the commit id for each patch")
                .long_help(
                    "Display the commit id for each patch.\n\
                     \n\
                     The optional length indicates how many prefix characters of the \
                     commit id to display. The default is \"full\", which displays the \
                     full commit id, but may alternatively be specified as a positive \
                     integer greater than or equal to 4.",
                )
                .value_name("length")
                .takes_value(true)
                .min_values(0)
                .number_of_values(1)
                .default_missing_value("full")
                .require_equals(true)
                .value_parser(clap::value_parser!(CommitIdLength)),
        )
        .arg(
            Arg::new("description")
                .long("description")
                .short('d')
                .help("Display short description for each patch")
                .overrides_with("no-description"),
        )
        .arg(
            Arg::new("no-description")
                .long("no-description")
                .help("Do not display the patch description")
                .overrides_with("description"),
        )
        .group(ArgGroup::new("description-group").args(&["description", "no-description"]))
        .arg(
            Arg::new("empty")
                .long("empty")
                .short('e')
                .help("Display whether patches are empty")
                .long_help(
                    "Before the '+', '>', '-', and '!' prefixes, print \
                     a column that contains either '0' (for empty \
                     patches) or a space (for non-empty patches).",
                ),
        )
        .arg(
            Arg::new("no-prefix")
                .long("no-prefix")
                .alias("noprefix")
                .short('P')
                .help("Do not display the patch status prefix"),
        )
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .help("Display the branch name with the listed patches"),
        )
}

#[derive(Clone)]
enum CommitIdLength {
    Full,
    Length(usize),
}

impl FromStr for CommitIdLength {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "full" => Ok(CommitIdLength::Full),
            _ => usize::from_str(s)
                .ok()
                .and_then(|n| {
                    if n >= 4 {
                        Some(CommitIdLength::Length(n))
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    anyhow!("length must be \"full\" or an integer greater than or equal to 4")
                }),
        }
    }
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let opt_missing = argset::get_one_str(matches, "missing");
    let (stack, cmp_stack) = if let Some(ref_branch) = opt_missing {
        (
            Stack::from_branch(&repo, Some(ref_branch))?,
            Some(Stack::from_branch(&repo, opt_branch)?),
        )
    } else {
        (Stack::from_branch(&repo, opt_branch)?, None)
    };

    let opt_all = matches.contains_id("all");
    let opt_applied = matches.contains_id("applied");
    let opt_unapplied = matches.contains_id("unapplied");
    let opt_hidden = matches.contains_id("hidden");

    let mut patches: Vec<(PatchName, git2::Oid, char)> = vec![];

    if let Some(range_specs) = matches.get_many::<patchrange::Specification>("patchranges-all") {
        let top_patchname = stack.applied().last();
        for patchname in patchrange::contiguous_patches_from_specs(
            range_specs,
            &stack,
            patchrange::Allow::AllWithAppliedBoundary,
        )? {
            let commit_id = stack.get_patch_commit(&patchname).id();
            let sigil = if Some(&patchname) == top_patchname {
                '>'
            } else if stack.is_applied(&patchname) {
                '+'
            } else if stack.is_unapplied(&patchname) {
                '-'
            } else {
                '!'
            };
            patches.push((patchname, commit_id, sigil));
        }
    } else {
        let show_applied = opt_applied || opt_all || !(opt_unapplied || opt_hidden);
        let show_unapplied = opt_unapplied || opt_all || !(opt_applied || opt_hidden);
        let show_hidden = opt_hidden || opt_all;

        if show_applied {
            if let Some((last_patchname, rest)) = stack.applied().split_last() {
                for patchname in rest {
                    let commit_id = stack.get_patch_commit(patchname).id();
                    patches.push((patchname.clone(), commit_id, '+'));
                }
                let last_oid = stack.get_patch_commit(last_patchname).id();
                patches.push((last_patchname.clone(), last_oid, '>'));
            }
        }

        if show_unapplied {
            for patchname in stack.unapplied() {
                let commit_id = stack.get_patch_commit(patchname).id();
                patches.push((patchname.clone(), commit_id, '-'));
            }
        }

        if show_hidden {
            for patchname in stack.hidden() {
                let commit_id = stack.get_patch_commit(patchname).id();
                patches.push((patchname.clone(), commit_id, '!'));
            }
        }
    }

    if let Some(cmp_stack) = cmp_stack {
        patches.retain(|(patchname, _, _)| {
            cmp_stack
                .all_patches()
                .all(|cmp_patchname| patchname != cmp_patchname)
        });
    }

    if matches.contains_id("short") {
        let shortnr = repo
            .config()
            .and_then(|config| config.get_i32("stgit.shortnr"))
            .unwrap_or(5);
        let shortnr: usize = if shortnr < 0 { 0 } else { shortnr as usize };

        if let Some(top_pos) = patches.iter().position(|(_, _, sigil)| *sigil == '>') {
            if patches.len() - top_pos > shortnr {
                patches.drain(top_pos + 1 + shortnr..);
            }
            if top_pos > shortnr {
                patches.drain(0..top_pos - shortnr);
            }
        } else {
            patches.drain(shortnr.min(patches.len())..);
        }
    }

    if matches.contains_id("count") {
        println!("{}", patches.len());
        return Ok(());
    }

    let opt_commit_id = matches.get_one::<CommitIdLength>("commit-id");
    let opt_description = matches.contains_id("description");
    let opt_author = matches.contains_id("author");

    let branch_prefix = format!("{}:", &stack.branch_name);
    let branch_prefix = if matches.contains_id("show-branch") {
        branch_prefix.as_str()
    } else {
        ""
    };

    let patchname_width = if opt_commit_id.is_some() || opt_description || opt_author {
        patches.iter().map(|(pn, _, _)| pn.len()).max().unwrap_or(0)
    } else {
        0
    };

    let author_width: usize = if opt_author && opt_description {
        patches
            .iter()
            .map(|(_, commit_id, _)| -> usize {
                if let Ok(commit) = repo.find_commit(*commit_id) {
                    let author = commit.author();
                    author.name().unwrap_or(UNPRINTABLE).len()
                } else {
                    0
                }
            })
            .max()
            .unwrap_or(0)
    } else {
        0
    };

    let opt_no_prefix = matches.contains_id("no-prefix");
    let opt_empty = matches.contains_id("empty");

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    for (patchname, commit_id, sigil) in patches {
        let commit = repo.find_commit(commit_id)?;

        if opt_empty {
            if commit.is_no_change()? {
                stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Cyan)))?;
                write!(stdout, "0")?;
                stdout.set_color(color_spec.set_fg(None))?;
            } else {
                write!(stdout, " ")?;
            }
        }

        if !opt_no_prefix {
            let sigil_color = match sigil {
                '+' => Some(termcolor::Color::Green),
                '>' => Some(termcolor::Color::Blue),
                '-' => Some(termcolor::Color::Magenta),
                '!' => Some(termcolor::Color::Red),
                _ => None,
            };
            stdout.set_color(color_spec.set_fg(sigil_color))?;
            write!(stdout, "{sigil} ")?;
            stdout.set_color(color_spec.set_fg(None))?;
        }

        match sigil {
            '+' => color_spec.set_intense(true),
            '>' => color_spec.set_bold(true),
            '-' => color_spec.set_dimmed(true),
            '!' => color_spec.set_dimmed(true).set_italic(true),
            _ => panic!("unhandled sigil {sigil:?}"),
        };
        stdout.set_color(&color_spec)?;

        if let Some(commit_length) = opt_commit_id.as_ref() {
            let id_str = commit_id.to_string();
            let id_prefix = match commit_length {
                CommitIdLength::Full => id_str.as_str(),
                CommitIdLength::Length(n) => {
                    let n = (*n).min(id_str.len());
                    &id_str[..n]
                }
            };
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
            write!(stdout, "{id_prefix} ")?;
            stdout.set_color(color_spec.set_fg(None))?;
        }

        write!(stdout, "{branch_prefix}{patchname:patchname_width$}")?;

        if opt_author {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Black)))?;
            write!(stdout, " # ")?;
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
            if let Ok(author) = commit.author_strict() {
                write!(stdout, "{:author_width$}", &author.name().unwrap(),)?;
            } else {
                let author = commit.author();
                let name = String::from_utf8_lossy(author.name_bytes());
                write!(stdout, "{name:author_width$}")?;
            }
        }
        if opt_description {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Black)))?;
            write!(stdout, " #")?;
            if let Some(summary) = commit.summary() {
                stdout.set_color(color_spec.set_fg(None))?;
                write!(stdout, " {summary}")?;
            }
        }
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
    }

    Ok(())
}
