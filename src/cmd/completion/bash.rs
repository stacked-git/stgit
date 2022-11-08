// SPDX-License-Identifier: GPL-2.0-only

//! `stg completion bash` implementation.

use std::{format as f, path::PathBuf};

use anyhow::Result;

use super::shstream::ShStream;

pub(super) fn command() -> clap::Command {
    clap::Command::new("bash")
        .about("Generate bash completion script")
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

    let mut stg = crate::get_full_command(&crate::alias::Aliases::new(), None);
    stg.build();

    for command in stg.get_subcommands() {
        write_command_func(&mut script, &f!("_stg-{}", command.get_name()), command);
    }

    script.raw(MAIN);

    stream.write_all(script.as_bytes())?;

    Ok(())
}

/// Compose completion function for the given subcommand.
fn write_command_func(script: &mut ShStream, fname: &str, command: &clap::Command) {
    let subcommand_names = get_subcommand_names(command);
    let (short_flags, long_flags) = get_flags(command);
    let options_pattern = get_options_pattern(command);
    let flags_pattern = get_cmd_flags_pattern(command);
    let opts_with_values = command.get_opts().collect::<Vec<_>>();

    script.ensure_blank_line();
    script.line(&f!("{fname} ()"));
    script.line("{");
    script.indent();
    script.lines(&[
        &f!("local short_flags=\"{short_flags}\""),
        &f!("local long_flags=\"{long_flags}\""),
        &f!("local options=\"{options_pattern}\""),
        "local pos_index=0",
    ]);

    if !subcommand_names.as_ref().is_empty() {
        script.line(&f!("local __subcommands=\"{subcommand_names}\""));
    }
    script.ensure_blank_line();

    script.lines(&["local i", "for ((i=cmd_index+1; i <= cword; i++)); do"]);
    script.indent();
    script.lines(&[
        "if [[ \"${words[i-1]}\" == @($options) ]]; then",
        "    continue",
        "fi",
    ]);
    script.line("case \"${words[i]}\" in");
    for subcommand in command.get_subcommands() {
        let subcommand_pattern = get_subcommand_pattern(subcommand);
        let subcommand_name = subcommand.get_name();
        script.line(&f!("{subcommand_pattern})"));
        script.indent();
        script.lines(&["cmd_index=$i", &f!("{fname}-{subcommand_name}; return;;")]);
        script.dedent();
    }
    if !flags_pattern.as_ref().is_empty() {
        script.lines(&[&f!("{flags_pattern})"), "    ;;"]);
    }
    script.line("--)");
    script.indent();
    script.line("if (( i < cword )); then");
    script.indent();
    if let Some(arg) = command.get_positionals().find(|arg| arg.is_last_set()) {
        insert_compreply(script, arg);
    }
    script.line("return");
    script.dedent();
    script.line("fi");
    script.line(";;");
    script.dedent();
    script.lines(&[
        "*)",
        "    if (( i < cword )); then",
        "        (( pos_index++ ))",
        "    fi",
        "    ;;",
        "esac",
    ]);
    script.dedent();
    script.line("done");
    script.line("");

    if !opts_with_values.is_empty() {
        script.ensure_blank_line();
        script.line("case \"$prev\" in");
        for arg in opts_with_values {
            let flags_pattern = get_arg_flags_pattern(arg);
            script.line(&f!("{flags_pattern})"));
            script.indent();
            insert_compreply(script, arg);
            script.line("return;;");
            script.dedent();
        }
        script.line("esac");
        script.line("");
    }

    script.ensure_blank_line();
    script.lines(&[
        "case \"$cur\" in",
        "--*)",
        "    mapfile -t COMPREPLY < <(compgen -W \"${long_flags[*]}\" -- \"$cur\")",
        "    return;;",
        "-*)",
        "    mapfile -t COMPREPLY < <(compgen -W \"${short_flags[*]} ${long_flags[*]}\" -- \"$cur\")",
        "    return;;",
        "esac",
    ]);

    let mut pos_index = 0;
    for arg in command.get_positionals() {
        let value_range = arg.get_num_args().expect("num_args is some for built arg");
        let num_vals = (value_range.max_values() > 1 && value_range.max_values() < usize::MAX)
            .then_some(value_range.max_values());
        script.ensure_blank_line();
        if matches!(arg.get_action(), clap::ArgAction::Append)
            || (num_vals.is_some() && arg.get_value_delimiter() == Some(' '))
        {
            if let Some(num_vals) = num_vals {
                script.line(&f!(
                    "if (( pos_index >= {pos_index} && pos_index < {})); then",
                    pos_index + num_vals
                ));
            } else {
                script.line(&f!("if (( pos_index >= {pos_index} )); then"));
            }
        } else {
            script.line(&f!("if (( pos_index == {pos_index} )); then"));
        }

        script.indent();
        insert_compreply(script, arg);

        if pos_index == 0 && !subcommand_names.as_ref().is_empty() {
            // Append subcommands to COMPREPLY
            script.line(
                "mapfile -O ${#COMPREPLY[@]} -t COMPREPLY < \
                 <(compgen -S ' ' -W \"$__subcommands\" -- \"$cur\")",
            );
        }
        script.dedent();

        pos_index += 1 + num_vals.unwrap_or(0);

        script.line("fi");
    }

    // The command has no positionals, but does have subcommands.
    if pos_index == 0 && !subcommand_names.as_ref().is_empty() {
        script.ensure_blank_line();
        script.line(
            "mapfile -t COMPREPLY < \
             <(compgen -S ' ' -W \"$__subcommands\" -- \"$cur\")",
        );
    }

    script.dedent();
    script.line("}");

    for subcommand in command.get_subcommands() {
        write_command_func(script, &f!("{fname}-{}", subcommand.get_name()), subcommand);
    }
}

/// Insert COMPREPLY line into script for given arg.
fn insert_compreply(script: &mut ShStream, arg: &clap::Arg) {
    assert!(arg.get_num_args().unwrap().takes_values());

    if let Some(possible_values) = arg.get_value_parser().possible_values() {
        let mut possible = ShStream::new();
        for pv in possible_values {
            possible.word(pv.get_name());
        }
        script.line(&f!(
            "mapfile -t COMPREPLY < <(compgen -W \"{possible}\" -- \"$cur\")"
        ));
    } else if matches!(
        arg.get_value_hint(),
        clap::ValueHint::Unknown | clap::ValueHint::Other
    ) {
        match arg.get_id().as_str() {
            "branch" | "ref-branch" => {
                script
                    .line("mapfile -t COMPREPLY < <(compgen -W \"$(_stg_branches)\" -- \"$cur\")");
            }

            "branch-any" => {
                script
                    .line("mapfile -t COMPREPLY < <(compgen -W \"$(_all_branches)\" -- \"$cur\")");
            }
            "committish" => {
                script.line(
                "mapfile -t COMPREPLY < <(compgen -W \"$(_all_branches) $(_tags) $(_remotes)\")",
            );
            }
            "git-diff-opt" => {
                script
                    .line("mapfile -t COMPREPLY < <(compgen -W \"$(_git_diff_opts)\" -- \"$cur\")");
            }
            "git-format-patch-opt" => {
                script.line(
                "mapfile -t COMPREPLY < <(compgen -W \"$(_git_format_patch_opts)\" -- \"$cur\")",
            );
            }
            "git-send-email-opt" => {
                script.line(
                    "mapfile -t COMPREPLY < <(compgen -W \"$(_git_send_email_opts)\" -- \"$cur\")",
                );
            }
            "patch" => {
                script.line(
                    "mapfile -t COMPREPLY < <(compgen -W \"$(_visible_patches)\" -- \"$cur\")",
                );
            }
            "patchranges" => {
                script.line("_patch_range \"$(_visible_patches)\"");
            }
            "patchranges-all" | "set-tree" | "stgit-revision" => {
                script.line("_patch_range \"$(_all_patches)\"");
            }
            "patchranges-applied" => {
                script.line("_patch_range \"$(_applied_patches)\"");
            }
            "patchranges-hidden" => {
                script.line("_patch_range \"$(_hidden_patches)\"");
            }
            "patchranges-unapplied" => {
                script.line("_patch_range \"$(_unapplied_patches)\"");
            }
            "pathspecs" => {
                script.line("mapfile -t COMPREPLY < <(compgen -o filenames -A file -- \"$cur\")");
            }
            "subcommand" => {
                // N.B. the `$__subcommands` here refers to a variable in the *calling
                // function's* scope, which will be absent for the top-level _stg-help
                // completion function.
                script.line("if [ -n \"$__subcommands\" ]; then");
                script.indent();
                script.line(
                    "mapfile -t COMPREPLY < \
                     <(compgen -S ' ' -W \"$__subcommands\" -- \"$cur\")",
                );
                script.dedent();
                script.line("else");
                script.indent();
                script.line(
                    "mapfile -t COMPREPLY < \
                     <(compgen -S ' ' \
                     -W \"$(__stg completion list commands-and-aliases)\" -- \"$cur\")",
                );
                script.dedent();
                script.line("fi");
            }
            _ => {
                script.line(":");
            }
        };
    } else {
        match arg.get_value_hint() {
            clap::ValueHint::Unknown | clap::ValueHint::Other => panic!(),
            clap::ValueHint::AnyPath => {
                script.line("mapfile -t COMPREPLY < <(compgen -o default -- \"$cur\")");
            }
            clap::ValueHint::FilePath => {
                script.line("mapfile -t COMPREPLY < <(compgen -o filenames -A file -- \"$cur\")");
            }
            clap::ValueHint::DirPath => {
                script.line(
                    "mapfile -t COMPREPLY < <(compgen -o directory -A directory -- \"$cur\")",
                );
            }
            clap::ValueHint::EmailAddress => {
                script.line(":");
            }
            clap::ValueHint::CommandName => {
                script.line("mapfile -t COMPREPLY < <(compgen -A command -- \"$cur\")");
            }
            clap::ValueHint::Username => {
                script.line("mapfile -t COMPREPLY < <(compgen -A user -- \"$cur\")");
            }
            clap::ValueHint::Hostname => {
                script.line("mapfile -t COMPREPLY < <(compgen -A hostname -- \"$cur\")");
            }
            clap::ValueHint::ExecutablePath => todo!(),
            clap::ValueHint::CommandString => todo!(),
            clap::ValueHint::CommandWithArguments => todo!(),
            clap::ValueHint::Url => todo!(),
            _ => todo!(),
        };
    }
}

/// Get short and long flags for a command.
fn get_flags(command: &clap::Command) -> (ShStream, ShStream) {
    let mut short_flags = ShStream::new();
    let mut long_flags = ShStream::new();

    for arg in command.get_arguments().filter(|arg| !arg.is_hide_set()) {
        if let Some(shorts) = arg.get_short_and_visible_aliases() {
            for c in shorts {
                short_flags.word(&f!("'-{c} '"));
            }
        }
        if let Some(longs) = arg.get_long_and_visible_aliases() {
            for name in longs {
                if arg
                    .get_num_args()
                    .map_or(false, |value_range| value_range.takes_values())
                {
                    long_flags.word(&f!("--{name}="));
                } else {
                    long_flags.word(&f!("'--{name} '"));
                }
            }
        }
    }

    for subcommand in command.get_subcommands().filter(|cmd| !cmd.is_hide_set()) {
        let name = subcommand.get_name();
        if name.starts_with("--") {
            long_flags.word(&f!("'{name} '"));
        } else if name.starts_with('-') {
            short_flags.word(&f!("'{name} '"));
        }

        if let Some(long) = subcommand.get_long_flag() {
            long_flags.word(&f!("'--{long} '"));
            for long_alias in subcommand.get_visible_long_flag_aliases() {
                long_flags.word(&f!("'--{long_alias} '"));
            }
        }

        if let Some(short) = subcommand.get_short_flag() {
            short_flags.word(&f!("'-{short} '"));
            for short_alias in subcommand.get_visible_short_flag_aliases() {
                short_flags.word(&f!("'-{short_alias} '"));
            }
        }
    }

    if command.get_positionals().any(clap::Arg::is_last_set) {
        long_flags.word("'-- '");
    }

    (short_flags, long_flags)
}

/// Get space separated list of subcommand names.
fn get_subcommand_names(command: &clap::Command) -> ShStream {
    let mut names = ShStream::new();
    for subcommand in command.get_subcommands() {
        names.word(subcommand.get_name());
        for alias in subcommand.get_visible_aliases() {
            names.word(alias);
        }
        if let Some(long) = subcommand.get_long_flag() {
            names.word(&f!("--{long}"));
            for long in subcommand.get_visible_long_flag_aliases() {
                names.word(&f!("--{long}"));
            }
        }
        if let Some(c) = subcommand.get_short_flag() {
            names.word(&f!("-{c}"));
            for c in subcommand.get_visible_short_flag_aliases() {
                names.word(&f!("-{c}"));
            }
        }
    }
    names
}

/// Get pipe separated list of subcommands suitable for use as a pattern.
fn get_subcommand_pattern(command: &clap::Command) -> ShStream {
    let mut pattern = ShStream::new();
    pattern.word_sep('|');
    pattern.word(command.get_name());
    for alias in command.get_all_aliases() {
        pattern.word(alias);
    }
    if let Some(long) = command.get_long_flag() {
        pattern.word(&f!("--{long}"));
        for long in command.get_all_long_flag_aliases() {
            pattern.word(&f!("--{long}"));
        }
    }
    if let Some(c) = command.get_short_flag() {
        pattern.word(&f!("-{c}"));
        for c in command.get_all_short_flag_aliases() {
            pattern.word(&f!("-{c}"));
        }
    }
    pattern
}

/// Get pipe separated list of command's visible long and short flags.
fn get_options_pattern(command: &clap::Command) -> ShStream {
    let mut options = ShStream::new();
    options.word_sep('|');
    for arg in command.get_opts() {
        options.word(get_arg_flags_pattern(arg).as_ref());
    }
    options
}

fn get_cmd_flags_pattern(command: &clap::Command) -> ShStream {
    let mut flags = ShStream::new();
    flags.word_sep('|');
    for arg in command.get_arguments().filter(|arg| {
        !arg.is_positional()
            && !arg
                .get_num_args()
                .expect("num_args is some for built arg")
                .takes_values()
    }) {
        flags.word(get_arg_flags_pattern(arg).as_ref());
    }
    flags
}

/// Get pipe separated list of arg's visible short and long flags.
fn get_arg_flags_pattern(arg: &clap::Arg) -> ShStream {
    let mut pattern = ShStream::new();
    pattern.word_sep('|');
    if let Some(longs) = arg.get_long_and_visible_aliases() {
        for long in longs {
            pattern.word(&f!("--{long}"));
        }
    }
    if let Some(shorts) = arg.get_short_and_visible_aliases() {
        for c in shorts {
            pattern.word(&f!("-{c}"));
        }
    }
    pattern
}

const HEADER: &str = r#"# -*- shell-script -*-
#
# SPDX-License-Identifier: GPL-2.0-only
#
# bash completion script for StGit (automatically generated)
#
# To use these routines:
#
#    1. Copy this file to somewhere (e.g. ~/.stgit-completion.bash).
#
#    2. Add the following line to your .bashrc:
#         . ~/.stgit-completion.bash

shopt -s extglob

__stg ()
{
    stg ${__C_args:+"${__C_args[@]}"} "$@" 2>/dev/null
}

__git ()
{
    git ${__C_args:+"${__C_args[@]}"} "$@" 2>/dev/null
}

# The path to .git, or empty if we're not in a repository.
_gitdir ()
{
    __git rev-parse --git-dir 2>/dev/null
}

# Name of the current branch, or empty if there isn't one.
_current_branch ()
{
    local b
    b=$(__git symbolic-ref HEAD 2>/dev/null)
    echo "${b#refs/heads/}"
}

_patch_range ()
{
    local patches="$1"

    case "$cur" in
    *..*)
        local cur="$cur"
        local pfx="${cur%..*}.."
        cur="${cur#*..}"
        mapfile -t COMPREPLY < <(compgen -o nosort -P "$pfx" -W "$patches" -- "$cur")
        ;;
    *)
        mapfile -t COMPREPLY < <(compgen -o nosort -W "$patches" -- "$cur")
        ;;
    esac
}

_stg_branches ()
{
    __stg branch --list 2>/dev/null | grep ". s" | cut -f2 | cut -d" " -f1
}

_all_branches ()
{
    __stg branch --list 2>/dev/null | cut -f2 | cut -d" " -f1
}

_mail_aliases ()
{
    __git config --name-only --get-regexp "^mail\.alias\." | cut -d. -f 3
}

_tags ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git show-ref  | grep ' refs/tags/' | sed 's,.* refs/tags/,,'
}

_remotes ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git show-ref  | grep ' refs/remotes/' | sed 's,.* refs/remotes/,,'
}

_all_patches ()
{
    __stg series --no-description --noprefix --all
}

_applied_patches ()
{
    __stg series --no-description --noprefix --applied
}

_unapplied_patches ()
{
    __stg series --no-description --noprefix --unapplied
}

_visible_patches ()
{
    __stg series --no-description --noprefix --applied --unapplied
}

_hidden_patches ()
{
    __stg series --no-description --noprefix --hidden
}

_conflicting_files ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git ls-files --unmerged | sed 's/.*\t//g' | sort -u
}

_dirty_files ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git diff-index --name-only HEAD
}

_unknown_files ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git ls-files --others --exclude-standard
}

_known_files ()
{
    local g
    g=$(_gitdir)
    test "$g" && __git ls-files "${cur}*"
}

_git_diff_opts ()
{
    __git diff-tree --git-completion-helper
}

_git_format_patch_opts ()
{
    __git format-patch --git-completion-helper
}

_git_send_email_opts()
{
    __git send-email --git-completion-helper
}

"#;

const MAIN: &str = r#"
_stg ()
{
    # Use bash-completion helper to clean-up the bash-builtin COMP_WORD, COMP_CWORD,
    # etc. variables.
    local cur prev words cword split
    _init_completion -s || return

    local i
    local command
    local cmd_index
    local __C_args C_args_count=0

    for ((i=1; i < cword; i++)); do
        case "${words[i]}" in
        --color=*)
            ;;
        --color)
            ((i++))
            ;;
        -C)
            __C_args[C_args_count++]=-C
            ((i++))
            __C_args[C_args_count++]="${words[i]}"
            ;;
        *)
            command="${words[i]}"
            cmd_index=$i
            break;;
        esac
    done

    if [ -z "${command-}" ]; then
        case "$prev" in
        -C)
            mapfile -t COMPREPLY < <(compgen -A directory -- "$cur")
            return;;
        --color)
            mapfile -t COMPREPLY < <(compgen -W "always ansi auto never" -- "$cur")
            return;;
        esac

        case "$cur" in
        --*)
            mapfile -t COMPREPLY < <(compgen -W "'--version ' --color= '--help " -- "$cur")
            ;;
        *)
            mapfile -t COMPREPLY < <(compgen -S ' ' -W "$(__stg completion list commands-and-aliases)" -- "$cur")
            ;;
        esac
        return
    fi

    local command_completion_func="_stg-${command}"
    if [ "$(type -t "$command_completion_func")" = function ]; then
        $command_completion_func && return
    else
        local a
        for a in $(__stg completion list aliases --style=zsh --show-expansion); do
            local name expansion
            name=${a%%:*}
            expansion=${a#*:}
            if [[ "$command" == "$name" ]]; then
                if [[ ${expansion:0:1} == "!" ]]; then
                    # Shell alias, fallback to simple filename completion
                    mapfile -t COMPREPLY < <(compgen -o filenames -A file -- "$cur")
                else
                    # StGit alias
                    command_completion_func="_stg-${expansion%% *}"
                    if [ "$(type -t "$command_completion_func")" = function ]; then
                        $command_completion_func && return
                    fi
                fi
                return
            fi
        done
    fi
}

complete -o nospace -F _stg stg
"#;
