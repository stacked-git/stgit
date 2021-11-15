use std::io::Write;

use clap::{App, Arg, ArgGroup, ArgMatches, ArgSettings, ValueHint};
use git2::Oid;
use termcolor::WriteColor;

use crate::{patchname::PatchName, stack::Stack, wrap::Repository};

use super::StGitCommand;

const UNPRINTABLE: &str = "???";

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("series", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("series")
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
                .about("Show all patches, including hidden patches")
                .conflicts_with_all(&["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .about("Show the applied patches only"),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .about("Show the unapplied patches only"),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .about("Show the hidden patches only"),
        )
        .arg(
            Arg::new("missing")
                .long("missing")
                .short('m')
                .about("Show patches in BRANCH not present in current branch")
                .setting(ArgSettings::TakesValue)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("count")
                .long("count")
                .short('c')
                .about("Print the number of patches in the series"),
        )
        .arg(
            Arg::new("description")
                .long("description")
                .short('d')
                .about("Show short description for each patch")
                .overrides_with("no-description"),
        )
        .arg(
            Arg::new("no-description")
                .long("no-description")
                .about("Disable description")
                .overrides_with("description"),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .about("Show author name for each patch"),
        )
        .arg(
            Arg::new("empty")
                .long("empty")
                .short('e')
                .about("Show whether patches are empty")
                .long_about(
                    "Before the '+', '>', '-', and '!' prefixes, print \
                     a column that contains either '0' (for empty \
                     patches) or a space (for non-empty patches).",
                ),
        )
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .about("Prepend the branch name to the listed patches"),
        )
        .arg(
            Arg::new("no-prefix")
                .long("noprefix")
                // TODO: add short option; maybe 'N'
                .about("Do not show the patch status prefix"),
        )
        .arg(
            Arg::new("short")
                .long("short")
                .short('s')
                .about("Only show patches around the topmost patch"),
        )
        .arg(&*crate::color::COLOR_ARG)
        .arg(
            Arg::new("patch-range")
                .about("Range of patches to show.")
                .multiple_values(true)
                .conflicts_with_all(&["all", "applied", "unapplied", "hidden", "short"]),
        )
        .group(ArgGroup::new("description-group").args(&["description", "no-description"]))
        .group(ArgGroup::new("all-short-group").args(&["all", "short"]))
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = Repository::open_from_env()?;
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
        let top_patchname = stack.state.applied.last();
        for patch_name in crate::patchrange::parse_contiguous_patch_range(
            patch_ranges,
            stack.state.all_patches(),
        )? {
            let oid = stack.state.patches[&patch_name].oid;
            let sigil = if Some(&patch_name) == top_patchname {
                '>'
            } else if stack.state.applied.contains(&patch_name) {
                '+'
            } else if stack.state.unapplied.contains(&patch_name) {
                '-'
            } else {
                '!'
            };
            patches.push((patch_name, oid, sigil));
        }
    } else {
        let show_applied = opt_applied || opt_all || !(opt_unapplied || opt_hidden);
        let show_unapplied = opt_unapplied || opt_all || !(opt_applied || opt_hidden);
        let show_hidden = opt_hidden || opt_all;

        if show_applied {
            if let Some((last_patch_name, rest)) = stack.state.applied.split_last() {
                for patch_name in rest {
                    let oid = stack.state.patches[patch_name].oid;
                    patches.push((patch_name.clone(), oid, '+'));
                }
                let last_oid = stack.state.patches[last_patch_name].oid;
                patches.push((last_patch_name.clone(), last_oid, '>'));
            }
        }

        if show_unapplied {
            for patch_name in stack.state.unapplied {
                let oid = stack.state.patches[&patch_name].oid;
                patches.push((patch_name, oid, '-'));
            }
        }

        if show_hidden {
            for patch_name in stack.state.hidden {
                let oid = stack.state.patches[&patch_name].oid;
                patches.push((patch_name, oid, '!'));
            }
        }
    }

    if let Some(cmp_stack) = cmp_stack {
        patches.retain(|(patch_name, _, _)| {
            cmp_stack
                .state
                .all_patches()
                .all(|cmp_patch_name| patch_name != cmp_patch_name)
        });
    }

    if matches.is_present("short") {
        let shortnr = repo
            .0
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
            patches.drain(shortnr..);
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

    let patch_name_width = if opt_description || opt_author {
        patches
            .iter()
            .map(|(patch_name, _, _)| patch_name.len())
            .max()
            .unwrap_or(0)
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

    for (patch_name, oid, sigil) in patches {
        let commit = repo.find_commit(oid)?;

        if opt_empty {
            if commit.parent_count() == 1 && commit.tree_id() == commit.parent(0)?.tree_id() {
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
            write!(stdout, "{} ", sigil)?;
            stdout.set_color(color_spec.set_fg(None))?;
        }

        match sigil {
            '+' => color_spec.set_intense(true),
            '>' => color_spec.set_bold(true),
            '-' => color_spec.set_dimmed(true),
            '!' => color_spec.set_dimmed(true).set_italic(true),
            _ => panic!("unhandled sigil {:?}", sigil),
        };
        stdout.set_color(&color_spec)?;
        write!(
            stdout,
            "{0}{1:width$}",
            branch_prefix,
            patch_name,
            width = patch_name_width
        )?;

        if opt_author {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
            let author: git2::Signature = commit.author();
            let author_name: &str = if let Some(name) = author.name() {
                name
            } else {
                UNPRINTABLE
            };
            write!(stdout, " # {:width$}", author_name, width = author_width)?;
        }
        if opt_description {
            let suffix = match commit.summary() {
                Some(summary) => format!(" # {}", summary),
                None => " #".to_string(),
            };
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
            write!(stdout, "{}", suffix)?;
        }
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
    }

    Ok(())
}
