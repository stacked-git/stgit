// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion fish` implementation

use std::{format as f, path::PathBuf};

use anyhow::Result;

use super::shstream::ShStream;

pub(super) fn command() -> clap::Command {
    clap::Command::new("fish")
        .about("Generate fish shell completion script")
        .arg(
            clap::Arg::new("output")
                .long("output")
                .short('o')
                .help("Output completion script to <path>")
                .value_name("path")
                .value_hint(clap::ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf)),
        )
}

pub(super) fn dispatch(matches: &clap::ArgMatches) -> Result<()> {
    let mut stream = super::get_output_stream(matches)?;

    let mut script = ShStream::new();

    script.raw(HEADER);

    let mut stg = crate::get_full_command(crate::alias::Aliases::new(), None);
    stg.build();

    for command in stg.get_subcommands() {
        write_command_completions(&mut script, command, None);
    }

    stream.write_all(script.as_bytes())?;

    Ok(())
}

const HEADER: &str = r#"# SPDX-License-Identifier: GPL-2.0-only
#
# Fish shell completion for StGit (stg)
#
# To use, copy this file to one of the paths in $fish_complete_path, e.g.:
#
#   ~/.config/fish/completions
#
# This file is autogenerated.

function __fish_stg
    set -l cd_args (__fish_stg_get_cd_args)
    eval command stg $cd_args $argv
end

function __fish_stg_git
    set -l cd_args (__fish_stg_get_cd_args)
    eval command git $cd_args $argv
end

function __fish_stg_get_cd_args
    set -l cmd (commandline -opc)
    # Erase 'stg' from command line
    set -e cmd[1]
    if argparse -s C=+ -- $cmd 2>/dev/null
        for path in $_flag_C
            echo -- -C
            echo -- $path
        end
    end
end

function __fish_stg_needs_command
    set -l cmd (commandline -opc)
    set -e cmd[1]
    argparse -s C=+ color= h/help version -- $cmd 2>/dev/null
    or return 0
    set -q _flag_version; and return 1
    if set -q argv[1]
        echo $argv[1]
        return 1
    end
    return 0
end

function __fish_stg_is_alias
    set -l alias_name (__fish_stg_needs_command)
    test -z "$alias_name"
    and return 1
    contains -- $alias_name (__fish_stg completion list aliases)
    and return 0
end

function __fish_stg_complete_alias
    set -l cmd (commandline -opc) (commandline -ct)
    set -l alias_name (__fish_stg_needs_command)
    set -e cmd[1]  # Erase 'stg'
    while test "$cmd[1]" != "$alias_name"
        set -e cmd[1]
    end
    set -e cmd[1]
    __fish_stg completion list aliases --style=fish --show-expansion | \
    while read -l -d \t aname expansion
        if test "$alias_name" = "$aname"
            if test (string sub --length=4 $expansion) = '!git'
                set -l cd_args (__fish_stg_get_cd_args)
                set -l expansion (string sub --start=5 $expansion)
                complete -C "git $cd_args $expansion $cmd"
            else if test (string sub --length=1 $expansion) = '!'
                set -l expansion (string sub --start=2 $expansion)
                complete -C "$expansion $cmd"
            else
                set -l cd_args (__fish_stg_get_cd_args)
                complete -C "stg $cd_args $expansion $cmd"
            end
            break
        end
    end
end

function __fish_stg_all_branches
    __fish_stg_git for-each-ref --format='%\(refname\)' \
        refs/heads/ refs/remotes/ 2>/dev/null \
        | string replace -r '^refs/heads/(.*)$' '$1\tLocal Branch' \
        | string replace -r '^refs/remotes/(.*)$' '$1\tRemote Branch'
end

function __fish_stg_stg_branches
    __fish_stg branch --list 2>/dev/null \
        | string match -r ". s.\t\S+" \
        | string replace -r ". s.\t" ""
end

function __fish_stg_patches
    set -l index 1
    __fish_stg series --color=never --no-description $argv 2>/dev/null | \
    while read -d ' ' sigil patch
        echo -e "$patch\t$index $sigil"
        set index (math $index + 1)
    end
end

function __fish_stg_tags
    __fish_stg_git tag --sort=-creatordate 2>/dev/null
end

function __fish_stg_commit
    __fish_stg_all_branches __fish_stg_tags
end

function __fish_stg_conflicting_files
    __fish_stg_git ls-files --unmerged \
        | string replace -rf '^.*\t(.*)$' '$1' \
        | sort -u
end

function __fish_stg_dirty_files
    __fish_stg_git diff-index --name-only HEAD 2>/dev/null
end

function __fish_stg_unknown_files
    __fish_stg_git ls-files --others --exclude-standard 2>/dev/null
end

function __fish_stg_known_files
    __fish_stg_git ls-files 2>/dev/null
end

function __fish_stg_mail_aliases
    __fish_stg_git config --name-only --get-regexp "^mail\.alias\." \
    | cut -d. -f 3
end

function __fish_stg_git_diff_opts
    __fish_stg_git diff-tree --git-completion-helper \
    | string split ' ' \
    | string match --regex '\--.+'
end

function __fish_stg_git_format_patch_opts
    __fish_stg_git format-patch --git-completion-helper \
    | string split ' ' \
    | string match --regex '\--.+'
end

function __fish_stg_git_send_email_opts
    __fish_stg_git send-email --git-completion-helper \
    | string split ' ' \
    | string match --regex '\--.+'
end

complete -c stg -n __fish_stg_needs_command -xa '(stg completion list commands-and-aliases --style=fish)'
complete -c stg -n __fish_stg_needs_command -s C -xa '(__fish_complete_directories)' -d 'Run as if started in this directory'
complete -c stg -n __fish_stg_needs_command -l color -a 'auto always ansi never' -d 'When to colorize output'
complete -c stg -n __fish_stg_needs_command -l version -d 'Print version information'
complete -c stg -n __fish_stg_needs_command -s h -l help -d 'Print help information'

complete -c stg -n __fish_stg_is_alias -xa '(__fish_stg_complete_alias)'

"#;

fn write_command_completions(
    script: &mut ShStream,
    command: &clap::Command,
    parent_subcmds: Option<Vec<String>>,
) {
    let name = command.get_name();

    script.ensure_blank_line();
    script.line(&f!("### {name}"));

    let mut condition = ShStream::new();

    if let Some(ref parent_subcmds) = parent_subcmds {
        for (i, parent_subcmd_ids) in parent_subcmds.iter().enumerate() {
            if i != 0 {
                condition.word("; and");
            }
            condition.word("__fish_seen_subcommand_from");
            condition.word(parent_subcmd_ids);
        }
        condition.word("; and");
    }

    let command_ids = get_command_ids(command);
    let command_ids = command_ids.as_ref().to_string();
    condition.word("__fish_seen_subcommand_from");
    condition.word(&command_ids);

    let mut child_subcmd_ids = ShStream::new();
    for subcommand in command.get_subcommands() {
        child_subcmd_ids.word(get_command_ids(subcommand).as_ref());
    }
    if !child_subcmd_ids.as_ref().is_empty() {
        condition.word("; and not __fish_seen_subcommand_from");
        condition.word(child_subcmd_ids.as_ref());
    }

    let mut complete_common = ShStream::new();
    complete_common.word("complete -c stg -n");
    complete_common.word(&f!("'{condition}'"));

    let mut has_positionals = false;
    for arg in command.get_positionals() {
        script.word(complete_common.as_ref());
        script.word(get_arg_completion_params(arg).as_ref());
        script.word(get_arg_help_param(arg).as_ref());
        script.end_line();
        has_positionals = true;
    }

    for arg in command.get_opts().filter(|arg| !arg.is_hide_set()) {
        script.word(complete_common.as_ref());
        script.word(get_arg_flags_params(arg).as_ref());
        script.word(get_arg_completion_params(arg).as_ref());
        script.word(get_arg_help_param(arg).as_ref());
        script.end_line();
    }

    // Flags
    for arg in command.get_arguments().filter(|arg| {
        !arg.is_positional()
            && !arg
                .get_num_args()
                .expect("num_args is some for built arg")
                .takes_values()
            && !arg.is_hide_set()
    }) {
        script.word(complete_common.as_ref());
        script.word(get_arg_flags_params(arg).as_ref());
        script.word(get_arg_help_param(arg).as_ref());
        script.end_line();
    }

    let mut has_subcommands = false;
    for subcommand in command.get_subcommands() {
        let sub_name = subcommand.get_name();

        if sub_name.starts_with("--") {
            script.word(complete_common.as_ref());
            if let Some(c) = subcommand.get_short_flag() {
                script.word("-s");
                script.word(&f!("{c}"));
                for c in subcommand.get_visible_short_flag_aliases() {
                    script.word("-s");
                    script.word(&f!("{c}"));
                }
            }
            script.word("-l");
            script.word(sub_name.strip_prefix("--").unwrap());
            for long in subcommand.get_visible_long_flag_aliases() {
                script.word("-l");
                script.word(long);
            }
        } else if sub_name.starts_with('-') {
            todo!();
        } else {
            script.word(complete_common.as_ref());
            script.word("-xa");
            script.word(&f!("'{}'", subcommand.get_name()));
        }

        script.word("-d");
        script.word(&f!("'{}'", subcommand.get_about().unwrap_or_default()));
        script.end_line();
        has_subcommands = true;
    }

    if !has_positionals && !has_subcommands {
        script.word(complete_common.as_ref());
        script.word("-f");
        script.end_line();
    }

    let parent_subcmds = if let Some(mut parent_subcmds) = parent_subcmds {
        parent_subcmds.push(command_ids);
        parent_subcmds
    } else {
        vec![command_ids]
    };

    for subcommand in command.get_subcommands() {
        write_command_completions(script, subcommand, Some(parent_subcmds.clone()));
    }
}

fn get_arg_flags_params(arg: &clap::Arg) -> ShStream {
    let mut params = ShStream::new();
    if let Some(shorts) = arg.get_short_and_visible_aliases() {
        for c in shorts {
            params.word(&f!("-s {c}"));
        }
    } else {
        params.word("     ");
    }
    if let Some(longs) = arg.get_long_and_visible_aliases() {
        for long in longs {
            params.word(&f!("-l {long}"));
        }
    }
    params
}

fn get_arg_completion_params(arg: &clap::Arg) -> ShStream {
    assert!(arg.get_num_args().unwrap().takes_values());

    let mut params = ShStream::new();

    if let Some(possible_values) = arg.get_value_parser().possible_values() {
        let mut possibles = ShStream::new();
        for pv in possible_values {
            possibles.word(pv.get_name());
        }
        params.word(&f!("-xa '{possibles}'"));
    } else if matches!(
        arg.get_value_hint(),
        clap::ValueHint::Unknown | clap::ValueHint::Other
    ) {
        match arg.get_id().as_str() {
            "branch" | "ref-branch" => params.word("-xa '(__fish_stg_stg_branches)'"),
            "branch-any" => params.word("-xa '(__fish_stg_all_branches)'"),
            "committish" => params.word("-xa '(__fish_stg_commit)'"),
            "git-diff-opts" => params.word("-xa '(__fish_stg_git_diff_opts)'"),
            "git-format-patch-opts" => params.word("-xa '(__fish_stg_git_format_patch_opts)'"),
            "git-send-email-opts" => params.word("-xa '(__fish_stg_git_send_email_opts)'"),
            "patch" | "patchranges" => params.word("-kxa '(__fish_stg_patches -A -U)'"),
            "patchranges-all" | "set-tree" | "stgit-revision" => {
                params.word("-kxa '(__fish_stg_patches --all)'")
            }
            "patchranges-applied" => params.word("-kxa '(__fish_stg_patches --applied)'"),
            "patchranges-hidden" => params.word("-kxa '(__fish_stg_patches --hidden)'"),
            "patchranges-unapplied" => params.word("-kxa '(__fish_stg_patches --unapplied)'"),
            "pathspecs" => params.word("-F"),
            "subcommand" => {
                params.word("-xa '(stg completion list commands-and-aliases --style=fish)'")
            }
            _ => params.word("-x"),
        };
    } else {
        params.word(match arg.get_value_hint() {
            clap::ValueHint::Unknown => "-r",
            clap::ValueHint::AnyPath
            | clap::ValueHint::ExecutablePath
            | clap::ValueHint::FilePath => "-rF",
            clap::ValueHint::DirPath => "-xa '(__fish_complete_directories)'",
            clap::ValueHint::CommandString | clap::ValueHint::CommandName => {
                "-xa '(__fish_complete_command)'"
            }
            clap::ValueHint::Username => "-xa '(__fish_complete_users)'",
            clap::ValueHint::Hostname => "-xa '(__fish_print_hostnames)'",
            _ => "-x",
        });
    }
    params
}

fn get_arg_help_param(arg: &clap::Arg) -> ShStream {
    let mut params = ShStream::new();
    let help = arg
        .get_help()
        .unwrap_or_default()
        .to_string()
        .replace('\\', "\\\\")
        .replace('\'', "\\'");
    params.word(&f!("-d '{help}'"));
    params
}

fn get_command_ids(command: &clap::Command) -> ShStream {
    let mut ids = ShStream::new();
    ids.word(command.get_name());
    for alias in command.get_all_aliases() {
        ids.word(alias);
    }
    if let Some(long) = command.get_long_flag() {
        ids.word(&f!("--{long}"));
        for long in command.get_all_long_flag_aliases() {
            ids.word(&f!("--{long}"));
        }
    }
    if let Some(c) = command.get_short_flag() {
        ids.word(&f!("-{c}"));
        for c in command.get_all_short_flag_aliases() {
            ids.word(&f!("-{c}"));
        }
    }
    ids
}
