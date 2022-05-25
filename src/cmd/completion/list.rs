// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion list` implementation

use std::str::FromStr;

use anyhow::Result;

pub(super) fn command() -> clap::Command<'static> {
    clap::Command::new("list")
        .about("List StGit command information")
        .arg(
            clap::Arg::new("style")
                .long("style")
                .help("Choose output format style")
                .possible_values(["name-only", "fish", "zsh"])
                .default_value("name-only")
                .validator(OutputStyle::from_str)
                .global(true),
        )
        .subcommand_required(true)
        .subcommand(
            clap::Command::new("aliases").about("List aliases").arg(
                clap::Arg::new("show-expansion")
                    .long("show-expansion")
                    .help("Show alias expansion"),
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
    Fish,
    Zsh,
}

impl FromStr for OutputStyle {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "name-only" => Ok(Self::NameOnly),
            "fish" => Ok(Self::Fish),
            "zsh" => Ok(Self::Zsh),
            s => Err(anyhow::anyhow!("Invalid output style `{s}`")),
        }
    }
}

pub(super) fn dispatch(matches: &clap::ArgMatches) -> Result<()> {
    let mut output = super::get_output_stream(matches)?;
    let commands = super::super::get_commands();
    let style = matches.value_of_t::<OutputStyle>("style").unwrap();

    match matches.subcommand() {
        Some(("aliases", sub_matches)) => {
            let show_expansion = sub_matches.is_present("show-expansion");
            list_aliases(&commands, &mut output, style, show_expansion)
        }
        Some(("commands", _)) => list_commands(&commands, &mut output, style),
        Some(("commands-and-aliases", _)) => {
            list_commands(&commands, &mut output, style)?;
            list_aliases(&commands, &mut output, style, false)
        }
        _ => panic!("valid subcommand is required"),
    }
}

fn list_aliases(
    commands: &super::super::Commands,
    output: &mut Box<dyn std::io::Write>,
    style: OutputStyle,
    show_expansion: bool,
) -> Result<()> {
    let (aliases, _) = crate::get_aliases(commands)?;
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
            OutputStyle::Fish => writeln!(output, "{name}\t{description}"),
            OutputStyle::Zsh => writeln!(output, "{name}:{description}"),
        }?;
    }
    Ok(())
}

fn list_commands(
    commands: &super::super::Commands,
    output: &mut Box<dyn std::io::Write>,
    style: OutputStyle,
) -> Result<()> {
    let mut stg_command = crate::get_full_command(commands, crate::alias::Aliases::new(), None);
    stg_command.build();

    for cmd in stg_command.get_subcommands() {
        let name = cmd.get_name();
        let about = cmd.get_about().unwrap_or_default();
        match style {
            OutputStyle::NameOnly => writeln!(output, "{name}"),
            OutputStyle::Fish => writeln!(output, "{name}\t{about}"),
            OutputStyle::Zsh => writeln!(output, "{name}:{about}"),
        }?;
    }
    Ok(())
}
