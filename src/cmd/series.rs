// SPDX-License-Identifier: GPL-2.0-only

//! `stg series` implementation.

use std::{io::Write, str::FromStr};

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgGroup, ArgMatches, ValueHint};
use termcolor::WriteColor;

use crate::{
    argset,
    ext::{CommitExtended, RepositoryExtended},
    patch::{patchrange, PatchName},
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
};

const UNPRINTABLE: &str = "???";

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "series",
    category: super::CommandCategory::StackInspection,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Display the patch series")
        .long_about(
            "Show all the patches in the series, or just those in the given range, \
             ordered from bottom to top.\n\
             \n\
             The topmost applied patch is prefixed with '>'. All other applied patches \
             are prefixed with '+'. Unapplied patches are prefixed with '-' and hidden \
             patches with '!'.\n\
             \n\
             The --reverse option may be used to reverse the order in which patches \
             are displayed. The reversed order is more stack-like, with the base of \
             the stack appearing at the bottom of of the display.\n\
             \n\
             Empty patches are prefixed with a '0'.",
        )
        .override_usage(
            "stg series [OPTIONS] [-A] [-U] [-H]\n       \
             stg series [OPTIONS] --all\n       \
             stg series [OPTIONS] --short\n       \
             stg series [OPTIONS] [patch]...",
        )
        .arg(
            Arg::new("patchranges-all")
                .help("Patches to display")
                .value_name("patch")
                .num_args(1..)
                .value_parser(clap::value_parser!(patchrange::Specification))
                .conflicts_with_all(["all", "applied", "unapplied", "hidden", "short"]),
        )
        .arg(argset::branch_arg())
        .next_help_heading("Select Options")
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Select all patches, including hidden patches")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["applied", "unapplied", "hidden"]),
        )
        .arg(
            Arg::new("short")
                .long("short")
                .short('s')
                .help("Select patches around the topmost patch only")
                .action(clap::ArgAction::SetTrue),
        )
        .group(ArgGroup::new("all-short-group").args(["all", "short"]))
        .arg(
            Arg::new("applied")
                .long("applied")
                .short('A')
                .help("Select the applied patches only")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("unapplied")
                .long("unapplied")
                .short('U')
                .help("Select the unapplied patches only")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("hidden")
                .long("hidden")
                .short('H')
                .help("Select the hidden patches only")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("missing")
                .long("missing")
                .short('m')
                .help("Select patches in <branch> not present in current branch")
                .num_args(1)
                .value_name("branch")
                .value_hint(ValueHint::Other),
        )
        .next_help_heading("Display Options")
        .arg(
            Arg::new("author")
                .long("author")
                .help("Display author name for each patch")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-author")
                .long("no-author")
                .help("Do not display patch authors")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("author"),
        )
        .arg(
            Arg::new("count")
                .long("count")
                .short('c')
                .help("Display the number of selected patches and exit")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["description", "author", "empty", "show-branch", "no-prefix"]),
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
                .num_args(0..=1)
                .default_missing_value("full")
                .require_equals(true)
                .value_parser(clap::value_parser!(CommitIdLength)),
        )
        .arg(
            Arg::new("no-commit-id")
                .long("no-commit-id")
                .help("Do not display commit ids")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("commit-id"),
        )
        .arg(
            Arg::new("description")
                .long("description")
                .short('d')
                .help("Display short description for each patch")
                .action(clap::ArgAction::SetTrue)
                .overrides_with("no-description"),
        )
        .arg(
            Arg::new("no-description")
                .long("no-description")
                .help("Do not display the patch description")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("description"),
        )
        .arg(
            Arg::new("empty")
                .long("empty")
                .short('e')
                .help("Display whether patches are empty")
                .long_help(
                    "Before the '+', '>', '-', and '!' prefixes, print \
                     a column that contains either '0' (for empty \
                     patches) or a space (for non-empty patches).",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-empty")
                .long("no-empty")
                .help("Do not display whether patches are empty")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("empty"),
        )
        .arg(
            Arg::new("prefix")
                .long("prefix")
                .help("Display patch status prefixes (default)")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("no-prefix"),
        )
        .arg(
            Arg::new("no-prefix")
                .long("no-prefix")
                .alias("noprefix")
                .short('P')
                .help("Do not display the patch status prefix")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("indices")
                .long("indices")
                .short('I')
                .help("Display absolute patch indicies")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-indices")
                .long("no-indices")
                .help("Do not display patch indices")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("indices"),
        )
        .arg(
            Arg::new("offsets")
                .long("offsets")
                .short('O')
                .help("Display relative offsets from topmost patch")
                .long_help(
                    "Display relative offsets from topmost patch or from the stack \
                     base if no patches are applied.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-offsets")
                .long("no-offsets")
                .help("Do not display patch offsets")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("offsets"),
        )
        .arg(
            Arg::new("reverse")
                .long("reverse")
                .short('r')
                .help("Display patches in reverse order")
                .long_help(
                    "Display patches in reverse order.\n\
                     \n\
                     This causes the stack to be displayed \"right-side up\". By \
                     default, patches are printed in order from bottom to top; i.e. \
                     the first patch in the stack is printed first and the last patch \
                     is printed last. Thus with the default ordering, the stack is \
                     displayed upside down. Reversing the order flips the stack such \
                     that the bottom of the stack is spatially at the top of the \
                     display, which may be more intuitive.",
                )
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-reverse")
                .long("no-reverse")
                .help("Do not display patches in reverse order")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("reverse"),
        )
        .arg(
            Arg::new("show-branch")
                .long("showbranch")
                .help("Display the branch name with the listed patches")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-show-branch")
                .long("no-showbranch")
                .help("Do not display branch name")
                .hide(true)
                .action(clap::ArgAction::SetTrue)
                .overrides_with("show-branch"),
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
    let repo = gix::Repository::open()?;
    let opt_branch = argset::get_one_str(matches, "branch");
    let opt_missing = argset::get_one_str(matches, "missing");

    let (stack, ref_stack) = if let Some(ref_branch) = opt_missing {
        (
            Stack::from_branch(
                &repo,
                Some(ref_branch),
                InitializationPolicy::AllowUninitialized,
            )?,
            Some(Stack::from_branch(
                &repo,
                opt_branch,
                InitializationPolicy::RequireInitialized,
            )?),
        )
    } else {
        (
            Stack::from_branch(&repo, opt_branch, InitializationPolicy::AllowUninitialized)?,
            None,
        )
    };

    let all_flag = matches.get_flag("all");
    let applied_flag = matches.get_flag("applied");
    let unapplied_flag = matches.get_flag("unapplied");
    let hidden_flag = matches.get_flag("hidden");

    struct Entry {
        patchname: PatchName,
        commit_id: gix::ObjectId,
        sigil: char,
        index: usize,
        offset_from_top: isize,
    }

    let mut patches: Vec<Entry> = vec![];

    if let Some(range_specs) = matches.get_many::<patchrange::Specification>("patchranges-all") {
        let top_patchname = stack.applied().last();
        for patchname in patchrange::contiguous_patches_from_specs(
            range_specs,
            &stack,
            patchrange::Allow::AllWithAppliedBoundary,
        )? {
            let commit_id = stack.get_patch_commit_id(&patchname);
            let sigil = if Some(&patchname) == top_patchname {
                '>'
            } else if stack.is_applied(&patchname) {
                '+'
            } else if stack.is_unapplied(&patchname) {
                '-'
            } else {
                '!'
            };
            let index = stack.index_of(&patchname);
            let offset_from_top = stack.distance_from(&patchname, top_patchname);
            patches.push(Entry {
                patchname,
                commit_id,
                sigil,
                index,
                offset_from_top,
            });
        }
    } else {
        let show_applied = applied_flag || all_flag || !(unapplied_flag || hidden_flag);
        let show_unapplied = unapplied_flag || all_flag || !(applied_flag || hidden_flag);
        let show_hidden = hidden_flag || all_flag;

        if show_applied {
            if let Some((last_patchname, rest)) = stack.applied().split_last() {
                for patchname in rest {
                    patches.push(Entry {
                        patchname: patchname.clone(),
                        commit_id: stack.get_patch_commit_id(patchname),
                        sigil: '+',
                        index: stack.index_of(patchname),
                        offset_from_top: stack.distance_from(patchname, Some(last_patchname)),
                    });
                }
                patches.push(Entry {
                    patchname: last_patchname.clone(),
                    commit_id: stack.get_patch_commit_id(last_patchname),
                    sigil: '>',
                    index: stack.index_of(last_patchname),
                    offset_from_top: 0,
                });
            }
        }

        if show_unapplied {
            for (i, patchname) in stack.unapplied().iter().enumerate() {
                patches.push(Entry {
                    patchname: patchname.clone(),
                    commit_id: stack.get_patch_commit_id(patchname),
                    sigil: '-',
                    index: stack.index_of(patchname),
                    offset_from_top: (i + 1) as isize,
                });
            }
        }

        if show_hidden {
            let top_patchname = stack.applied().last();
            for patchname in stack.hidden() {
                patches.push(Entry {
                    patchname: patchname.clone(),
                    commit_id: stack.get_patch_commit_id(patchname),
                    sigil: '!',
                    index: stack.index_of(patchname),
                    offset_from_top: stack.distance_from(patchname, top_patchname),
                });
            }
        }
    }

    if let Some(ref_stack) = ref_stack {
        patches.retain(|Entry { patchname, .. }| {
            ref_stack
                .all_patches()
                .all(|ref_patchname| patchname != ref_patchname)
        });
    }

    if matches.get_flag("short") {
        let shortnr = repo.config_snapshot().integer("stgit.shortnr").unwrap_or(5);
        let shortnr: usize = if shortnr < 0 { 0 } else { shortnr as usize };

        if let Some(top_pos) = patches.iter().position(|Entry { sigil, .. }| *sigil == '>') {
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

    if matches.get_flag("count") {
        println!("{}", patches.len());
        return Ok(());
    }

    let opt_commit_id = matches.get_one::<CommitIdLength>("commit-id");
    let description_flag = matches.get_flag("description");
    let author_flag = matches.get_flag("author");

    let branch_prefix = format!("{}:", &stack.get_branch_name());
    let branch_prefix = if matches.get_flag("show-branch") {
        branch_prefix.as_str()
    } else {
        ""
    };

    let patchname_width = if opt_commit_id.is_some() || description_flag || author_flag {
        patches
            .iter()
            .map(|Entry { patchname, .. }| patchname.len())
            .max()
            .unwrap_or(0)
    } else {
        0
    };

    let author_width: usize = if author_flag && description_flag {
        patches
            .iter()
            .map(|Entry { commit_id, .. }| -> usize {
                if let Ok(commit) = repo.find_commit(*commit_id) {
                    commit
                        .author()
                        .ok()
                        .and_then(|author| author.name.to_str().ok())
                        .unwrap_or(UNPRINTABLE)
                        .len()
                } else {
                    0
                }
            })
            .max()
            .unwrap_or(0)
    } else {
        0
    };

    let no_prefix_flag = matches.get_flag("no-prefix");
    let empty_flag = matches.get_flag("empty");
    let indices_flag = matches.get_flag("indices");
    let offsets_flag = matches.get_flag("offsets");

    let index_width = (indices_flag && !patches.is_empty())
        .then(|| patches.last().unwrap().index.to_string().len())
        .unwrap_or_default();

    let offset_width = (offsets_flag && !patches.is_empty())
        .then(|| {
            [patches.first().unwrap(), patches.last().unwrap()]
                .iter()
                .map(|entry| format!("{:+}", entry.offset_from_top).len())
                .max()
                .unwrap()
        })
        .unwrap_or_default();

    let mut stdout = crate::color::get_color_stdout(matches);
    let mut color_spec = termcolor::ColorSpec::new();

    if matches.get_flag("reverse") {
        patches.reverse();
    }

    for Entry {
        patchname,
        commit_id,
        sigil,
        index,
        offset_from_top,
    } in patches
    {
        let commit = repo.find_commit(commit_id)?;
        let commit_ref = commit.decode()?;

        if empty_flag {
            if commit.is_no_change()? {
                stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Cyan)))?;
                write!(stdout, "0")?;
                stdout.set_color(color_spec.set_fg(None))?;
            } else {
                write!(stdout, " ")?;
            }
        }

        let sigil_color = match sigil {
            '+' => Some(termcolor::Color::Green),
            '>' => Some(termcolor::Color::Blue),
            '-' => Some(termcolor::Color::Magenta),
            '!' => Some(termcolor::Color::Red),
            _ => None,
        };

        if !no_prefix_flag {
            stdout.set_color(color_spec.set_fg(sigil_color))?;
            write!(stdout, "{sigil} ")?;
            stdout.set_color(color_spec.set_fg(None))?;
        }

        if indices_flag {
            stdout.set_color(color_spec.set_fg(sigil_color))?;
            write!(stdout, "{index:index_width$} ")?;
            stdout.set_color(color_spec.set_fg(None))?;
        }

        if offsets_flag {
            stdout.set_color(color_spec.set_fg(sigil_color))?;
            write!(stdout, "{offset_from_top:+offset_width$} ")?;
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

        if author_flag {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Black)))?;
            write!(stdout, " # ")?;
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
            if let Ok(author) = commit.author_strict() {
                write!(stdout, "{:author_width$}", &author.name.to_str().unwrap())?;
            } else {
                let name = commit_ref.author().name.to_str_lossy();
                write!(stdout, "{name:author_width$}")?;
            }
        }
        if description_flag {
            stdout.set_color(color_spec.set_fg(Some(termcolor::Color::Black)))?;
            write!(stdout, " #")?;
            let summary = commit_ref.message_summary();
            if !summary.is_empty() {
                if let Ok(summary) = summary.to_str() {
                    stdout.set_color(color_spec.set_fg(None))?;
                    write!(stdout, " {summary}")?;
                }
            }
        }
        color_spec.clear();
        stdout.set_color(&color_spec)?;
        writeln!(stdout)?;
    }

    Ok(())
}
