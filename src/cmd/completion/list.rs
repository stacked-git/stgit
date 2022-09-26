// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion list` implementation

use std::str::FromStr;

use anyhow::Result;

use crate::cmd::STGIT_COMMANDS;

pub(super) fn command() -> clap::Command<'static> {
    clap::Command::new("list")
        .about("List StGit command information")
        .arg(
            clap::Arg::new("style")
                .long("style")
                .help("Choose output format style")
                .value_parser(["name-only", "asciidoc", "fish", "zsh"])
                .default_value("name-only")
                .global(true),
        )
        .subcommand_required(true)
        .subcommand(
            clap::Command::new("aliases").about("List aliases").arg(
                clap::Arg::new("show-expansion")
                    .long("show-expansion")
                    .help("Show alias expansion")
                    .action(clap::ArgAction::SetTrue),
            ),
        )
        .subcommand(clap::Command::new("commands").about("List StGit commands"))
        .subcommand(
            clap::Command::new("commands-and-aliases").about("List StGit commands and aliases"),
        )
}

#[derive(Clone, Copy, Debug)]
enum OutputStyle {
    NameOnly,
    AsciiDoc,
    Fish,
    Zsh,
}

impl FromStr for OutputStyle {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "name-only" => Ok(Self::NameOnly),
            "asciidoc" => Ok(Self::AsciiDoc),
            "fish" => Ok(Self::Fish),
            "zsh" => Ok(Self::Zsh),
            s => Err(anyhow::anyhow!("Invalid output style `{s}`")),
        }
    }
}

pub(super) fn dispatch(matches: &clap::ArgMatches) -> Result<()> {
    let mut output = super::get_output_stream(matches)?;
    let style = OutputStyle::from_str(
        matches
            .get_one::<String>("style")
            .expect("has a default value defined"),
    )
    .expect("possible values are all value OutputStyle");

    match matches.subcommand() {
        Some(("aliases", sub_matches)) => {
            let show_expansion = sub_matches.get_flag("show-expansion");
            list_aliases(&mut output, style, show_expansion)
        }
        Some(("commands", _)) => list_commands(&mut output, style),
        Some(("commands-and-aliases", _)) => {
            list_commands(&mut output, style)?;
            list_aliases(&mut output, style, false)
        }
        _ => panic!("valid subcommand is required"),
    }
}

fn list_aliases(
    output: &mut Box<dyn std::io::Write>,
    style: OutputStyle,
    show_expansion: bool,
) -> Result<()> {
    let (aliases, _) = crate::get_aliases()?;

    if matches!(style, OutputStyle::AsciiDoc) {
        writeln!(
            output,
            "Aliases\n\
             ~~~~~~~\n"
        )?;
    }

    for (name, alias) in aliases {
        let description = if show_expansion {
            let expansion = alias.command.as_str();
            let prefix = match alias.kind {
                crate::alias::AliasKind::Shell => "!",
                crate::alias::AliasKind::StGit => "",
            };
            std::borrow::Cow::Owned(format!("{prefix}{expansion}"))
        } else {
            let mut cmd = alias.make();
            cmd.build();
            std::borrow::Cow::Borrowed(cmd.get_about().unwrap_or_default())
        };
        match style {
            OutputStyle::NameOnly => writeln!(output, "{name}"),
            OutputStyle::AsciiDoc => writeln!(output, "{name}::\n    {description}"),
            OutputStyle::Fish => writeln!(output, "{name}\t{description}"),
            OutputStyle::Zsh => writeln!(output, "{name}:{description}"),
        }?;
    }
    Ok(())
}

fn list_commands(output: &mut Box<dyn std::io::Write>, style: OutputStyle) -> Result<()> {
    use crate::cmd::CommandCategory;

    let mut stg_command = crate::get_full_command(crate::alias::Aliases::new(), None);
    stg_command.build();

    if matches!(style, OutputStyle::AsciiDoc) {
        let mut subcmd_cats: Vec<(CommandCategory, &clap::Command)> = Vec::new();
        for cmd in stg_command.get_subcommands() {
            let name = cmd.get_name();
            if let Some(stgit_cmd) = STGIT_COMMANDS.iter().find(|command| command.name == name) {
                subcmd_cats.push((stgit_cmd.category, cmd));
            } else if name == "help" {
                subcmd_cats.push((super::super::CommandCategory::Administration, cmd));
            } else {
                panic!("unhandled command '{name}'");
            }
        }
        subcmd_cats.sort_by_key(|(category, cmd)| (*category, cmd.get_name()));

        let mut last_category = None;
        for (category, cmd) in subcmd_cats {
            let name = cmd.get_name();
            let about = cmd.get_about().unwrap_or_default();
            if Some(category) != last_category {
                if last_category.is_some() {
                    writeln!(output)?;
                }
                match category {
                    CommandCategory::PatchInspection => {
                        writeln!(
                            output,
                            "Patch Inspection\n\
                             ~~~~~~~~~~~~~~~~\n"
                        )?;
                    }
                    CommandCategory::PatchManipulation => {
                        writeln!(
                            output,
                            "Patch Manipulation\n\
                             ~~~~~~~~~~~~~~~~~~\n"
                        )?;
                    }
                    CommandCategory::StackInspection => {
                        writeln!(
                            output,
                            "Stack Inspection\n\
                             ~~~~~~~~~~~~~~~~\n"
                        )?;
                    }
                    CommandCategory::StackManipulation => {
                        writeln!(
                            output,
                            "Stack Manipulation\n\
                             ~~~~~~~~~~~~~~~~~~\n"
                        )?;
                    }
                    CommandCategory::Administration => {
                        writeln!(
                            output,
                            "Administration\n\
                             ~~~~~~~~~~~~~~\n"
                        )?;
                    }
                }
                last_category = Some(category);
            }
            writeln!(output, "linkstg:{name}[]::\n    {about}")?;
        }
        writeln!(output)?;
        return Ok(());
    }

    for cmd in stg_command.get_subcommands() {
        let name = cmd.get_name();
        let about = cmd.get_about().unwrap_or_default();
        match style {
            OutputStyle::NameOnly => writeln!(output, "{name}"),
            OutputStyle::AsciiDoc => panic!(),
            OutputStyle::Fish => writeln!(output, "{name}\t{about}"),
            OutputStyle::Zsh => writeln!(output, "{name}:{about}"),
        }?;
    }
    Ok(())
}
