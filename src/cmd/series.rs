use std::io::Write;

use clap::{App, Arg, ArgGroup, ArgMatches, ArgSettings, ValueHint};
use git2::{Oid, Repository};
use termcolor::WriteColor;

use crate::{patchname::PatchName, stack::Stack};

const UNPRINTABLE: &str = "???";

pub(crate) fn get_subcommand() -> App<'static> {
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
        .arg(
            Arg::new("branch")
                .long("branch")
                .short('b')
                .about("Use BRANCH instead of current branch")
                .setting(ArgSettings::TakesValue)
                .value_name("BRANCH")
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .about("Show all patches, including hidden patches"),
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
                .about("Show patches in BRANCH missing in current")
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
                .about("Append the branch name to the listed patches"),
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
        .arg(&*crate::argset::COLOR_ARG)
        .group(ArgGroup::new("description-group").args(&["description", "no-description"]))
        .group(ArgGroup::new("all-short-group").args(&["all", "short"]))
}

pub(crate) fn run(matches: &ArgMatches) -> super::Result {
    let repo = Repository::open_from_env()?;
    let opt_branch = matches.value_of("branch");
    let stack = Stack::from_branch(&repo, opt_branch)?;

    let opt_all = matches.is_present("all");
    let opt_applied = matches.is_present("applied");
    let opt_unapplied = matches.is_present("unapplied");
    let opt_hidden = matches.is_present("hidden");

    let show_applied = opt_applied || opt_all || !(opt_unapplied || opt_hidden);
    let show_unapplied = opt_unapplied || opt_all || !(opt_applied || opt_hidden);
    let show_hidden = opt_hidden || opt_all;

    let mut patches: Vec<(PatchName, Oid, char)> = vec![];

    if show_applied {
        if let Some((last_patch_name, rest)) = stack.applied.split_last() {
            for patch_name in rest {
                let oid = stack.patches[patch_name].oid;
                patches.push((patch_name.clone(), oid, '+'));
            }
            let last_oid = stack.patches[last_patch_name].oid;
            patches.push((last_patch_name.clone(), last_oid, '>'));
        }
    }

    if show_unapplied {
        for patch_name in stack.unapplied {
            let oid = stack.patches[&patch_name].oid;
            patches.push((patch_name, oid, '-'));
        }
    }

    if show_hidden {
        for patch_name in stack.hidden {
            let oid = stack.patches[&patch_name].oid;
            patches.push((patch_name, oid, '!'));
        }
    }

    if matches.is_present("count") {
        println!("{}", patches.len());
        return Ok(());
    }

    let opt_description = matches.is_present("description");
    let opt_author = matches.is_present("author");

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

    let color_choice = match matches.value_of("color").unwrap_or("auto") {
        "always" => termcolor::ColorChoice::Always,
        "ansi" => termcolor::ColorChoice::AlwaysAnsi,
        "auto" => {
            if atty::is(atty::Stream::Stdout) {
                termcolor::ColorChoice::Auto
            } else {
                termcolor::ColorChoice::Never
            }
        }
        _ => termcolor::ColorChoice::Never,
    };

    let mut stdout = termcolor::StandardStream::stdout(color_choice);
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
        write!(stdout, "{:width$}", patch_name, width = patch_name_width)?;

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
