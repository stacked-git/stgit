//! Support for built-in and user-defined command aliases.

use std::collections::BTreeMap;

use anyhow::{anyhow, Result};
use bstr::ByteSlice;
use git2::{Config, ConfigLevel};

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
    pub(crate) fn make(&self) -> clap::Command<'static> {
        let about = match self.kind {
            AliasKind::StGit => format!("Alias for `stg {0}`", &self.command),
            AliasKind::Shell => format!("Alias for shell command `{}`", &self.command),
        };
        // TODO: future versions of clap may allow about argument to be something other
        // than Into<&'help str>, which could avoid having to do this leak trick.
        let about: &'static str = Box::leak(about.into_boxed_str());
        clap::Command::new(&self.name)
            .about(about)
            .trailing_var_arg(true)
            .arg(
                clap::Arg::new("args")
                    .help("Extra arguments to aliased command")
                    .multiple_values(true)
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
            ("add", "!git add"),
            ("mv", "!git mv"),
            ("resolved", "!git add"),
            ("rm", "!git rm"),
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
pub(crate) fn get_aliases<F>(config: &Config, exclude: F) -> Result<Aliases>
where
    F: Fn(&str) -> bool,
{
    let mut aliases = get_default_aliases();

    for entry in config.entries(None)?.flatten() {
        if let Some(name) = entry.name_bytes().strip_prefix(b"stgit.alias.") {
            let name = name.to_str().map_err(|_| {
                anyhow!(
                    "Alias name `{}` in {} is not valid UTF-8",
                    name.to_str_lossy(),
                    config_level_to_str(entry.level()),
                )
            })?;
            if entry.has_value() {
                if !exclude(name) {
                    let command = entry.value().ok_or_else(|| {
                        anyhow!(
                            "Alias value for `{name}` in {} is not valid UTF-8",
                            config_level_to_str(entry.level()),
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
                    word.push(c)
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

/// Map [`git2::ConfigLevel`] to user-facing strings.
fn config_level_to_str(level: ConfigLevel) -> &'static str {
    match level {
        git2::ConfigLevel::ProgramData => "program data config",
        git2::ConfigLevel::System => "system config",
        git2::ConfigLevel::XDG => "XDG config",
        git2::ConfigLevel::Global => "global config",
        git2::ConfigLevel::Local => "local config",
        git2::ConfigLevel::App => "app config",
        git2::ConfigLevel::Highest => "highest config",
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
