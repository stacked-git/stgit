//! `stg series` implementation.

use std::io::Write;

use anyhow::Result;
use clap::{Arg, ArgGroup, ArgMatches, ValueHint};
use git2::Oid;
use termcolor::WriteColor;

use crate::{
    commit::CommitExtended,
    patchname::PatchName,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

const UNPRINTABLE: &str = "???";

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("series", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("series")
        .about("Print the patch series")
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
        .arg(&*crate::argset::BRANCH_ARG)
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Show all patches, including hidden patches")
                .conflicts_with_all(&["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Show the applied patches only"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Show the unapplied patches only"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Show the hidden patches only"),
        )
        .arg(
            Arg::new("missing")
                .long("missing")
                .short('m')
                .help("Show patches in BRANCH not present in current branch")
                .takes_value(true)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("count")
                .long("count")
                .short('c')
                .help("Print the number of patches in the series"),
        )
        .arg(
            Arg::new("description")
                .long("description")
                .short('d')
                .help("Show short description for each patch")
                .overrides_with("no-description"),
        )
        .arg(
            Arg::new("no-description")
                .long("no-description")
                .help("Disable description")
                .overrides_with("description"),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .help("Show author name for each patch"),
        )
        .arg(
            Arg::new("empty")
                .long("empty")
                .short('e')
                .help("Show whether patches are empty")
                .long_help(
                    "Before the '+', '>', '-', and '!' prefixes, print \
                     a column that contains either '0' (for empty \
                     patches) or a space (for non-empty patches).",
                ),
        )
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .help("Prepend the branch name to the listed patches"),
        )
        .arg(
            Arg::new("no-prefix")
                .long("noprefix")
                // TODO: add short option; maybe 'N'
                .help("Do not show the patch status prefix"),
        )
        .arg(
            Arg::new("short")
                .long("short")
                .short('s')
                .help("Only show patches around the topmost patch"),
        )
        .arg(
            Arg::new("patch-range")
                .help("Range of patches to show")
                .multiple_values(true)
                .conflicts_with_all(&["all", "applied", "unapplied", "hidden", "short"]),
        )
        .group(ArgGroup::new("description-group").args(&["description", "no-description"]))
        .group(ArgGroup::new("all-short-group").args(&["all", "short"]))
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let opt_missing = matches.value_of("missing");
    let (stack, cmp_stack) = if let Some(ref_branch) = opt_missing {
        (
            Stack::from_branch(&repo, Some(ref_branch))?,
            Some(Stack::from_branch(&repo, opt_branch)?),
        )
    } else {
        (Stack::from_branch(&repo, opt_branch)?, None)
    };

    let opt_all = matches.is_present("all");
    let opt_applied = matches.is_present("applied");
    let opt_unapplied = matches.is_present("unapplied");
    let opt_hidden = matches.is_present("hidden");

    let mut patches: Vec<(PatchName, Oid, char)> = vec![];

    if let Some(patch_ranges) = matches.values_of("patch-range") {
        let top_patchname = stack.applied().last();
        for patchname in crate::patchrange::parse_contiguous_patch_range(
            patch_ranges,
            stack.all_patches(),
            stack.all_patches(),
        )? {
            let oid = stack.get_patch(&patchname).commit.id();
            let sigil = if Some(&patchname) == top_patchname {
                '>'
            } else if stack.is_applied(&patchname) {
                '+'
            } else if stack.is_unapplied(&patchname) {
                '-'
            } else {
                '!'
            };
            patches.push((patchname, oid, sigil));
        }
    } else {
        let show_applied = opt_applied || opt_all || !(opt_unapplied || opt_hidden);
        let show_unapplied = opt_unapplied || opt_all || !(opt_applied || opt_hidden);
        let show_hidden = opt_hidden || opt_all;

        if show_applied {
            if let Some((last_patchname, rest)) = stack.applied().split_last() {
                for patchname in rest {
                    let oid = stack.get_patch(patchname).commit.id();
                    patches.push((patchname.clone(), oid, '+'));
                }
                let last_oid = stack.get_patch(last_patchname).commit.id();
                patches.push((last_patchname.clone(), last_oid, '>'));
            }
        }

        if show_unapplied {
            for patchname in stack.unapplied() {
                let oid = stack.get_patch(patchname).commit.id();
                patches.push((patchname.clone(), oid, '-'));
            }
        }

        if show_hidden {
            for patchname in stack.hidden() {
                let oid = stack.get_patch(patchname).commit.id();
                patches.push((patchname.clone(), oid, '!'));
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

    if matches.is_present("short") {
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

    if matches.is_present("count") {
        println!("{}", patches.len());
        return Ok(());
    }

    let opt_description = matches.is_present("description");
    let opt_author = matches.is_present("author");

    let branch_prefix = format!("{}:", &stack.branch_name);
    let branch_prefix = if matches.is_present("show-branch") {
        branch_prefix.as_str()
    } else {
        ""
    };

    let patchname_width = if opt_description || opt_author {
        patches.iter().map(|(pn, _, _)| pn.len()).max().unwrap_or(0)
    } else {
        0
    };

    let author_width: usize = if opt_author && opt_description {
        patches
            .iter()
            .map(|(_, oid, _)| -> usize {
                if let Ok(commit) = repo.find_commit(*oid) {
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

    let opt_no_prefix = matches.is_present("no-prefix");
    let opt_empty = matches.is_present("empty");

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    for (patchname, oid, sigil) in patches {
        let commit = repo.find_commit(oid)?;

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
        write!(stdout, "{branch_prefix}{patchname:patchname_width$}")?;

        if opt_author {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
            if let Ok(author) = commit.author_strict() {
                write!(stdout, " # {:author_width$}", &author.name().unwrap(),)?;
            } else {
                let author = commit.author();
                let name = String::from_utf8_lossy(author.name_bytes());
                write!(stdout, " # {name:author_width$}")?;
            }
        }
        if opt_description {
            let suffix = match commit.summary() {
                Some(summary) => format!(" # {summary}"),
                None => " #".to_string(),
            };
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
            write!(stdout, "{suffix}")?;
        }
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
    }

    Ok(())
}
