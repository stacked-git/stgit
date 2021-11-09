use std::collections::BTreeMap;

use git2::{Config, ConfigEntry, ConfigLevel};

use crate::error::Error;

pub(crate) struct StGitAlias(pub String);
pub(crate) struct ShellAlias(pub String);

pub(crate) enum Alias {
    StGit(StGitAlias),
    Shell(ShellAlias),
}

pub(crate) fn get_aliases(
    config: Option<&Config>,
    excluded: Vec<&'static str>,
) -> Result<BTreeMap<String, Alias>, Error> {
    let mut aliases: BTreeMap<String, Alias> = BTreeMap::from(
        [
            ("add", "git add"),
            ("mv", "git mv"),
            ("resolved", "git add"),
            ("rm", "git rm"),
            ("status", "git status -s"),
        ]
        .map(|(name, command)| (name.into(), Alias::Shell(ShellAlias(command.into())))),
    );

    if let Some(config) = config {
        if let Ok(entries) = config.entries(Some("stgit.alias.*")) {
            for entry in &entries {
                if let Ok(entry) = entry {
                    let (name, alias) = make_alias(entry)?;
                    if !excluded.iter().any(|n| n == &name) {
                        if let Some(alias) = alias {
                            aliases.insert(name, alias);
                        } else {
                            aliases.remove(&name);
                        }
                    }
                }
            }
        }
    }

    Ok(aliases)
}

fn make_alias(entry: ConfigEntry<'_>) -> Result<(String, Option<Alias>), Error> {
    if let Some(config_key) = entry.name() {
        let (_, name) = config_key
            .split_once("stgit.alias.")
            .expect("stgit.alias.* glob problem");
        let name: String = name.into();
        if entry.has_value() {
            if let Some(value) = entry.value() {
                if let Some((_, shell_command)) = value.split_once('!') {
                    Ok((name, Some(Alias::Shell(ShellAlias(shell_command.into())))))
                } else {
                    Ok((name, Some(Alias::StGit(StGitAlias(value.into())))))
                }
            } else {
                Err(Error::NonUtf8AliasValue(
                    name,
                    config_level_to_str(entry.level()).to_string(),
                ))
            }
        } else {
            Ok((name, None))
        }
    } else {
        Err(Error::NonUtf8AliasName(
            String::from_utf8_lossy(entry.name_bytes()).to_string(),
            config_level_to_str(entry.level()).to_string(),
        ))
    }
}

pub(crate) fn get_app(name: &str, alias: &Alias) -> clap::App<'static> {
    let about = match alias {
        Alias::StGit(alias) => format!("Alias for `stg {0}`", alias.command()),
        Alias::Shell(alias) => format!("Alias for shell command `{}`", alias.command()),
    };
    // TODO: future versions of clap may allow about() argument to
    // be something other than &'help str, which could avoid having
    // to do this leak trick.
    let about: &'static str = Box::leak(about.into_boxed_str());
    clap::App::new(name)
        .about(about)
        .setting(clap::AppSettings::TrailingVarArg)
        .arg(
            clap::Arg::new("args")
                .about("Extra arguments to aliased command")
                .setting(clap::ArgSettings::MultipleValues)
                .setting(clap::ArgSettings::AllowHyphenValues),
        )
}

impl StGitAlias {
    pub(crate) fn command(&self) -> &str {
        &self.0
    }

    pub(crate) fn split(&self) -> Result<Vec<String>, String> {
        split_command_line(&self.0)
    }
}

impl ShellAlias {
    pub(crate) fn command(&self) -> &str {
        &self.0
    }

    pub(crate) fn run(
        &self,
        args: clap::OsValues,
        repo: Option<&git2::Repository>,
    ) -> crate::cmd::Result {
        let mut args = args;
        let mut command = if self.command().find(SHELL_CHARS).is_some() {
            // Need to wrap in shell command
            let mut command = std::process::Command::new(SHELL_PATH);
            command.arg("-c");
            if let Some(first_arg) = args.next() {
                command.arg(&format!("{} \"$@\"", self.command()));
                command.arg(self.command());
                command.arg(first_arg);
            } else {
                command.arg(self.command());
            }
            command
        } else {
            std::process::Command::new(self.command())
        };
        command.args(args);

        if let Some(repo) = repo {
            if let Some(workdir) = repo.workdir() {
                command.current_dir(workdir);
                if let Ok(cur_dir) = std::env::current_dir() {
                    if let Ok(prefix) = cur_dir.strip_prefix(workdir) {
                        if !cur_dir.starts_with(repo.path()) {
                            let mut prefix = prefix.as_os_str().to_os_string();
                            if !prefix.is_empty() {
                                prefix.push("/");
                            }
                            command.env("GIT_PREFIX", &prefix);
                        }
                    }
                }
                if let Ok(rel_dir) = repo.path().strip_prefix(workdir) {
                    if rel_dir == std::path::PathBuf::from(".git") {
                        command.env_remove("GIT_DIR");
                    } else {
                        command.env("GIT_DIR", rel_dir);
                    }
                } else {
                    command.env("GIT_DIR", repo.path());
                }
            } else {
                command.env("GIT_DIR", repo.path());
            }
        }
        // TODO: map to custom error
        let status = command
            .status()
            .map_err(|e| Error::ExecuteAlias(self.command().to_string(), e.to_string()))?;
        if status.success() {
            Ok(())
        } else {
            std::process::exit(status.code().unwrap_or(-1));
        }
    }
}

// TODO: Git chooses its shell path at compile time based on OS or user override.
const SHELL_PATH: &str = "sh";
const SHELL_CHARS: &[char] = &[
    '|', '&', ';', '<', '>', '(', ')', '$', '`', ' ', '*', '?', '[', '#', '~', '=', '%', '\\', '"',
    '\'', '\t', '\n',
];

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
