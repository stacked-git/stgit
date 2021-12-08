use std::collections::BTreeMap;

use git2::{Config, ConfigLevel};

use crate::error::Error;

pub(crate) type Aliases = BTreeMap<String, Alias>;

#[derive(Debug, Clone, Copy)]
pub(crate) enum AliasKind {
    Shell,
    StGit,
}

#[derive(Debug)]
pub(crate) struct Alias {
    pub kind: AliasKind,
    pub name: String,
    pub command: String,
}

impl Alias {
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

    pub(crate) fn get_app(&self) -> clap::App<'static> {
        let about = match self.kind {
            AliasKind::StGit => format!("Alias for `stg {0}`", &self.command),
            AliasKind::Shell => format!("Alias for shell command `{}`", &self.command),
        };
        // TODO: future versions of clap may allow about argument to be something other
        // than Into<&'help str>, which could avoid having to do this leak trick.
        let about: &'static str = Box::leak(about.into_boxed_str());
        clap::App::new(&self.name)
            .about(about)
            .setting(clap::AppSettings::TrailingVarArg)
            .arg(
                clap::Arg::new("args")
                    .help("Extra arguments to aliased command")
                    .setting(clap::ArgSettings::MultipleValues)
                    .setting(clap::ArgSettings::AllowHyphenValues),
            )
    }

    pub(crate) fn split(&self) -> Result<Vec<String>, String> {
        split_command_line(&self.command)
    }
}

pub(crate) fn get_aliases(
    config: Option<&Config>,
    excluded: Vec<&'static str>,
) -> Result<Aliases, Error> {
    let mut aliases: Aliases = BTreeMap::from(
        [
            ("add", "!git add"),
            ("mv", "!git mv"),
            ("resolved", "!git add"),
            ("rm", "!git rm"),
            ("status", "!git status -s"),
        ]
        .map(|(name, command)| (name.into(), Alias::new(name, command))),
    );

    if let Some(config) = config {
        if let Ok(entries) = config.entries(Some("stgit.alias.*")) {
            for entry in entries.flatten() {
                if let Some(config_key) = entry.name() {
                    let name = config_key
                        .strip_prefix("stgit.alias.")
                        .expect("stgit.alias.* glob problem");
                    if entry.has_value() {
                        if let Some(command) = entry.value() {
                            if !excluded.iter().any(|n| *n == name) {
                                let key = name.to_string();
                                let alias = Alias::new(name, command);
                                aliases.insert(key, alias);
                            }
                        } else {
                            return Err(Error::NonUtf8AliasValue(
                                name.to_string(),
                                config_level_to_str(entry.level()).to_string(),
                            ));
                        }
                    } else {
                        aliases.remove(name);
                    }
                } else {
                    return Err(Error::NonUtf8AliasName(
                        String::from_utf8_lossy(entry.name_bytes()).to_string(),
                        config_level_to_str(entry.level()).to_string(),
                    ));
                }
            }
        }
    }

    Ok(aliases)
}

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
