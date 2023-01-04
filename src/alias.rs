// SPDX-License-Identifier: GPL-2.0-only

//! Support for built-in and user-defined command aliases.

use std::collections::BTreeMap;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

/// Mapping of alias names to [`Alias`] structs.
pub(crate) type Aliases = BTreeMap<String, Alias>;

/// Kind of command alias: `Shell` or `StGit`.
#[derive(Debug, Clone, Copy)]
pub(crate) enum AliasKind {
    /// Shell aliases are executed with the system shell (typically `bash`).
    Shell,

    /// StGit aliases name/invoke builtin StGit subcommands.
    StGit,
}

/// Command alias
#[derive(Debug)]
pub(crate) struct Alias {
    pub kind: AliasKind,
    pub name: String,
    pub command: String,
}

impl Alias {
    /// Create new command alias.
    ///
    /// If the command string begins with '!', it will be treated as a shell alias,
    /// otherwise it will be an alias for a StGit command.
    pub(crate) fn new(name: &str, command: &str) -> Self {
        let (kind, command) = if let Some(command) = command.strip_prefix('!') {
            (AliasKind::Shell, command.to_string())
        } else {
            (AliasKind::StGit, command.to_string())
        };
        Self {
            kind,
            name: name.into(),
            command,
        }
    }

    /// Make [`clap::Command`] for the alias.
    pub(crate) fn make(&self) -> clap::Command {
        let about = match self.kind {
            AliasKind::StGit => format!("Alias for `stg {}`", &self.command),
            AliasKind::Shell => format!("Alias for shell command `{}`", &self.command),
        };
        clap::Command::new(self.name.clone())
            .about(about)
            .trailing_var_arg(true)
            .arg(
                clap::Arg::new("args")
                    .help("Extra arguments to aliased command")
                    .num_args(1..)
                    .allow_hyphen_values(true),
            )
    }

    /// Split the alias' command line into words vector.
    pub(crate) fn split(&self) -> Result<Vec<String>, String> {
        split_command_line(&self.command)
    }
}

/// Generate mapping of built-in aliases.
pub(crate) fn get_default_aliases() -> Aliases {
    let aliases: Aliases = BTreeMap::from(
        [
            ("add", "!git -C \"$GIT_PREFIX\" add"),
            ("mv", "!git -C \"$GIT_PREFIX\" mv"),
            ("resolved", "!git -C \"$GIT_PREFIX\" add"),
            ("rm", "!git -C \"$GIT_PREFIX\" rm"),
            ("status", "!git status -s"),
        ]
        .map(|(name, command)| (name.into(), Alias::new(name, command))),
    );
    aliases
}

/// Get user-defined aliases from the git configuration.
///
/// The `exclude` closure is intended to prevent names of builtin StGit subcommands from
/// being shadowed by aliases.
pub(crate) fn get_aliases<F>(
    config_file: Option<&git_repository::config::File>,
    exclude: F,
) -> Result<Aliases>
where
    F: Fn(&str) -> bool,
{
    let mut aliases = get_default_aliases();

    if let Some(config_file) = config_file {
        if let Some(sections) = config_file.sections_by_name("stgit") {
            for section in sections
                .filter(|section| section.header().subsection_name() == Some("alias".into()))
            {
                for key in section.keys() {
                    let name = key.to_str().map_err(|_| {
                        anyhow!(
                            "Alias name `{}` in {} is not valid UTF-8",
                            key.to_str_lossy(),
                            config_source_str(section.meta().source),
                        )
                    })?;
                    if let Some(value) = section
                        .value(key)
                        .and_then(|v| (!v.is_empty()).then_some(v))
                    {
                        if !exclude(name) {
                            let command = value.to_str().map_err(|_| {
                                anyhow!(
                                    "Alias value for `{name}` in {} is not valid UTF-8",
                                    config_source_str(section.meta().source)
                                )
                            })?;
                            let alias = Alias::new(name, command);
                            aliases.insert(name.to_string(), alias);
                        }
                    } else {
                        aliases.remove(name);
                    }
                }
            }
        }
    }

    Ok(aliases)
}

/// Split command line string into words.
///
/// Single- and double-quoted substrings are preserved.
fn split_command_line(line: &str) -> Result<Vec<String>, String> {
    let mut argv = Vec::new();
    let mut quote: char = '\0';
    let mut skip_spaces = true;
    let mut post_backspace = false;
    let mut word = String::new();

    for c in line.chars() {
        if post_backspace {
            word.push(c);
            post_backspace = false;
        } else if c.is_ascii_whitespace() {
            if !skip_spaces {
                if quote == '\0' {
                    let completed_word = std::mem::take(&mut word);
                    argv.push(completed_word);
                    skip_spaces = true;
                } else {
                    word.push(c);
                }
            }
        } else {
            skip_spaces = false;
            if quote == '\0' && (c == '\'' || c == '"') {
                quote = c;
            } else if c == quote {
                quote = '\0';
            } else if c == '\\' && quote != '\'' {
                post_backspace = true;
            } else {
                word.push(c);
            }
        }
    }

    if post_backspace {
        Err("command line ends with \\".to_string())
    } else if quote != '\0' {
        Err("unclosed quote".to_string())
    } else {
        argv.push(word);
        Ok(argv)
    }
}

/// Map [`git_repository::config::Source`] to user-facing strings.
fn config_source_str(source: git_repository::config::Source) -> &'static str {
    use git_repository::config::Source;
    match source {
        Source::GitInstallation => "git installed config",
        Source::System => "system config",
        Source::Git => "git config",
        Source::User => "user global config",
        Source::Local => "repository local config",
        Source::Worktree => "worktree config",
        Source::Env => "environment config",
        Source::Cli => "cli config",
        Source::Api => "api config",
        Source::EnvOverride => "environment override config",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_command_lines() {
        assert_eq!(
            split_command_line("git status"),
            Ok(vec![String::from("git"), String::from("status")])
        );
        assert_eq!(
            split_command_line("cat \"foo bar\""),
            Ok(vec![String::from("cat"), String::from("foo bar")])
        );
        assert_eq!(
            split_command_line("cat \"foo 'bar'\""),
            Ok(vec![String::from("cat"), String::from("foo 'bar'")])
        );
        assert_eq!(
            split_command_line("cat \"foo \\\" 'bar'\""),
            Ok(vec![String::from("cat"), String::from("foo \" 'bar'")])
        );
        assert_eq!(
            split_command_line("cat 'dog"),
            Err("unclosed quote".to_string()),
        );
        assert_eq!(
            split_command_line("cat \"dog'"),
            Err("unclosed quote".to_string()),
        );
        assert_eq!(
            split_command_line("cat dog\\"),
            Err("command line ends with \\".to_string()),
        );
    }
}
