// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion man` implementation

use std::{fmt::Write, path::Path};

use anyhow::Result;

const WIDTH: usize = 80;

pub(super) fn command() -> clap::Command<'static> {
    clap::Command::new("man")
        .about("Generate asciidoc man pages")
        .long_about(
            "Generate man pages in asciidoc format. The generated asciidoc files may \
             be further processed by asciidoc or asciidoctor to produce roff, html, or \
             other output formats.\n\
             \n\
             One file is generated for each `stg` command. The output directory \
             defaults to the current directory, but may be specified with `--output`. \
             The output files are named `stg-<command>.txt`.",
        )
        .arg(
            clap::Arg::new("output")
                .long("output")
                .short('o')
                .help("Output man pages to DIR")
                .value_name("DIR")
                .value_hint(clap::ValueHint::DirPath)
                .allow_invalid_utf8(true),
        )
}

pub(super) fn dispatch(matches: &clap::ArgMatches) -> Result<()> {
    let output_dir = if let Some(path) = matches.value_of_os("output") {
        Path::new(path)
    } else {
        Path::new("")
    };

    std::fs::create_dir_all(output_dir)?;

    let commands = crate::cmd::get_commands();
    let mut stg = crate::get_full_command(&commands, crate::alias::Aliases::new(), None);
    stg.build();

    for command in stg.get_subcommands_mut() {
        let asciidoc = generate_asciidoc(command);
        let path = output_dir.join(format!("stg-{}.txt", command.get_name()));
        if std::fs::read_to_string(&path).ok().as_ref() != Some(&asciidoc) {
            std::fs::write(path, asciidoc)?;
        }
    }

    Ok(())
}

fn generate_asciidoc(command: &mut clap::Command) -> String {
    let mut page = String::new();
    let name = command.get_name().to_string();

    write_underlined(&mut page, &format!("stg-{name}(1)"), '=');
    page.push('\n');

    write_underlined(&mut page, "NAME", '-');
    let about = command.get_about().unwrap();
    write!(&mut page, "stg-{name} - {about}\n\n").unwrap();

    write_underlined(&mut page, "SYNOPSIS", '-');
    page.push_str("[verse]\n");
    page.push_str(&get_usage(command));
    page.push('\n');

    write_underlined(&mut page, "DESCRIPTION", '-');
    page.push('\n');
    let about = make_links(
        command
            .get_long_about()
            .or_else(|| command.get_about())
            .unwrap(),
    );
    for para in paragraphs(&about) {
        for line in wrap(para, WIDTH) {
            page.push_str(line);
            page.push('\n');
        }
        page.push('\n');
    }

    if let Some(commands_section) = get_commands_section(command) {
        write_underlined(&mut page, "COMMANDS", '-');
        page.push('\n');
        page.push_str(&commands_section);
    }

    if let Some(options_section) = get_options_section(command) {
        page.push_str(&options_section);
    }

    // TODO use command.get_after_long_help()

    write_underlined(&mut page, "StGit", '-');
    page.push_str("Part of the StGit suite - see linkman:stg[1]\n");

    page
}

fn get_usage(command: &mut clap::Command) -> String {
    let mut usage = String::new();
    let name_stack = vec!["stg".to_string()];
    add_usage(&mut usage, command, &name_stack);
    usage
}

fn add_usage(usage: &mut String, command: &mut clap::Command, name_stack: &[String]) {
    let name = command.get_name().to_string();
    let usage_lines = command
        .render_usage()
        .lines()
        .filter(|&line| line != "USAGE:")
        .map(|line| line.trim_start().to_string())
        .collect::<Vec<_>>();

    if usage_lines.len() == 1 {
        let mut has_subcommands = false;
        let mut sub_name_stack = name_stack.to_vec();
        sub_name_stack.push(name);
        for subcmd in command.get_subcommands_mut() {
            has_subcommands = true;
            add_usage(usage, subcmd, &sub_name_stack);
        }
        if has_subcommands {
            return;
        }
    }

    for usage_line in &usage_lines {
        usage.push('\'');
        let mut usage_line_words = usage_line.split(' ');
        for stack_word in name_stack {
            let usage_word = usage_line_words
                .next()
                .expect("usage line has more words than name_stack");
            assert!(usage_word == stack_word);
            usage.push_str(usage_word);
            usage.push(' ');
        }
        let usage_word = usage_line_words
            .next()
            .expect("usage line has more words than name_stack");
        usage.push_str(usage_word);
        usage.push('\'');

        for usage_word in usage_line_words {
            usage.push(' ');
            if let Some(rest) = usage_word.strip_prefix("<-") {
                usage.push_str("\\<-");
                usage.push_str(rest);
            } else {
                usage.push_str(usage_word);
            }
        }
        usage.push('\n');
    }
}

fn get_commands_section(command: &clap::Command) -> Option<String> {
    let mut section = String::new();
    for subcmd in command
        .get_subcommands()
        .filter(|&subcmd| subcmd.get_name() != "help")
    {
        let subcmd_stack = vec![];
        add_command_stanza(&mut section, subcmd, &subcmd_stack);
    }
    if section.is_empty() {
        None
    } else {
        Some(section)
    }
}

fn add_command_stanza(section: &mut String, command: &clap::Command, stack: &[&str]) {
    let name = command.get_name();
    let mut has_subcommands = false;
    for subcmd in command
        .get_subcommands()
        .filter(|&subcmd| subcmd.get_name() != "help")
    {
        has_subcommands = true;
        let mut stack = stack.to_vec();
        stack.push(name);
        add_command_stanza(section, subcmd, &stack);
    }

    if !has_subcommands {
        for word in stack {
            section.push_str(word);
            section.push(' ');
        }
        section.push_str(name);
        section.push_str("::\n");

        let about = make_links(
            command
                .get_long_about()
                .or_else(|| command.get_about())
                .unwrap(),
        );

        for (i, para) in paragraphs(&about).enumerate() {
            if i > 0 {
                section.push_str("+\n");
            }
            for line in para.lines() {
                let wrap_width = if i == 0 { WIDTH - 4 } else { WIDTH };
                for wrapped_line in wrap(line, wrap_width) {
                    if i == 0 {
                        section.push_str("    ");
                    }
                    section.push_str(wrapped_line);
                    section.push('\n');
                }
            }
        }

        section.push('\n');
    }
}

fn get_options_section(command: &clap::Command) -> Option<String> {
    let mut section = String::new();
    add_options(&mut section, command, "OPTIONS", '-');
    for subcmd in command
        .get_subcommands()
        .filter(|&subcmd| subcmd.get_name() != "help")
    {
        add_subcommand_options(&mut section, subcmd, &[]);
    }

    if section.is_empty() {
        None
    } else {
        Some(section)
    }
}

fn add_subcommand_options(section: &mut String, command: &clap::Command, stack: &[&str]) {
    let name = command.get_name();
    let mut has_subcommands = false;
    for subcmd in command
        .get_subcommands()
        .filter(|&subcmd| subcmd.get_name() != "help")
    {
        has_subcommands = true;
        let mut stack = stack.to_vec();
        stack.push(name);
        add_subcommand_options(section, subcmd, &stack);
    }

    if !has_subcommands {
        let mut header = String::new();
        for &word in stack {
            header.push_str(&word.to_uppercase());
            header.push(' ');
        }
        header.push_str(&command.get_name().to_uppercase());
        header.push_str(" OPTIONS");
        add_options(section, command, &header, '~');
    }
}

fn add_options(
    section: &mut String,
    command: &clap::Command,
    header_name: &str,
    header_underline: char,
) {
    for (i, arg) in command
        .get_arguments()
        .filter(|arg| {
            arg.get_name() != "help"
                && !arg.is_hide_set()
                && !arg.is_positional()
                && !arg.is_global_set()
        })
        .enumerate()
    {
        if i == 0 {
            write_underlined(section, header_name, header_underline);
        }
        let value_str = if let Some(value_names) = arg.get_value_names() {
            let mut value_str = String::new();
            for name in value_names.iter() {
                value_str.push(' ');
                value_str.push_str(name);
            }
            value_str
        } else {
            String::new()
        };
        if let Some(shorts) = arg.get_short_and_visible_aliases() {
            for short in shorts {
                writeln!(section, "-{short}{value_str}::").unwrap();
            }
        }
        if let Some(longs) = arg.get_long_and_visible_aliases() {
            for long in longs {
                writeln!(section, "--{long}{value_str}::").unwrap();
            }
        }
        let help = make_links(arg.get_long_help().or_else(|| arg.get_help()).unwrap());
        for (i, para) in paragraphs(&help).enumerate() {
            if i > 0 {
                section.push_str("+\n");
            }
            for line in para.lines() {
                let width = if i == 0 { WIDTH - 4 } else { WIDTH };
                for wrapped_line in wrap(line, width) {
                    if i == 0 {
                        section.push_str("    ");
                    }
                    section.push_str(wrapped_line);
                    section.push('\n');
                }
            }
        }
        section.push('\n');
    }
}

fn make_links(text: &str) -> String {
    let mut output = String::new();
    let mut words = text.split_inclusive(|c| c == ' ' || c == '\n');

    while let Some(word) = words.next() {
        if word.starts_with("git-") && word.trim_end().ends_with(')') {
            let (stuff, trailing_ws) = word.rsplit_once(')').unwrap();
            if let Some((command, man_section)) =
                stuff.strip_prefix("git-").unwrap().split_once('(')
            {
                let link = format!("linkgit:git-{command}[{man_section}]");
                output.push_str(&link);
                output.push_str(trailing_ws);
            } else {
                output.push_str(word)
            }
        } else if word.starts_with("'git") {
            if let Some(next_word) = words.next() {
                if let Some((command, rest)) = next_word.split_once('\'') {
                    let link = format!("linkgit:git-{command}[1]{rest}");
                    output.push_str(&link);
                } else {
                    output.push_str(word);
                    output.push_str(next_word);
                }
            } else {
                output.push_str(word);
            }
        } else if word.starts_with("'stg") {
            if let Some(next_word) = words.next() {
                if let Some((command, rest)) = next_word.split_once('\'') {
                    let link = format!("linkstg:{command}[]{rest}");
                    output.push_str(&link);
                } else {
                    output.push_str(word);
                    output.push_str(next_word);
                }
            } else {
                output.push_str(word);
            }
        } else {
            output.push_str(word);
        }
    }

    output
}

fn write_underlined(stream: &mut String, line: &str, underline_char: char) {
    stream.push_str(line);
    stream.push('\n');
    for _ in line.chars() {
        stream.push(underline_char);
    }
    stream.push('\n');
}

struct Paragraphs<'a> {
    text: &'a str,
}

impl<'a> Iterator for Paragraphs<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.text.is_empty() {
            None
        } else if let Some((para, rest)) = self.text.split_once("\n\n") {
            self.text = rest.trim_start();
            Some(para)
        } else {
            let rest = self.text;
            self.text = "";
            Some(rest)
        }
    }
}

fn paragraphs(text: &str) -> Paragraphs {
    Paragraphs { text }
}

struct WrappedLines<'a> {
    text: &'a str,
    width: usize,
}

impl<'a> Iterator for WrappedLines<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.text.is_empty() {
            None
        } else {
            let mut last_space_index = 0;
            let mut reached_end = true;
            for (i, c) in self.text.char_indices() {
                if c == ' ' {
                    if i < self.width {
                        last_space_index = i;
                    } else {
                        reached_end = false;
                        break;
                    }
                }
            }

            if last_space_index == 0 || reached_end {
                let rest = self.text;
                self.text = "";
                Some(rest)
            } else {
                let (line, rest) = self.text.split_at(last_space_index);
                self.text = &rest[1..];
                Some(line)
            }
        }
    }
}

fn wrap(text: &str, width: usize) -> WrappedLines {
    WrappedLines { text, width }
}
