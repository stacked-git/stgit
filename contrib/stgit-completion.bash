# bash completion support for StGIT                        -*- shell-script -*-
#
# Copyright (C) 2006, Karl Hasselström <kha@treskal.com>
# Based on git-completion.sh
#
# To use these routines:
#
#    1. Copy this file to somewhere (e.g. ~/.stgit-completion.bash).
#
#    2. Add the following line to your .bashrc:
#         . ~/.stgit-completion.bash

# The path to .git, or empty if we're not in a repository.
_gitdir ()
{
    echo "$(git rev-parse --git-dir 2>/dev/null)"
}

# Name of the current branch, or empty if there isn't one.
_current_branch ()
{
    local b=$(git symbolic-ref HEAD 2>/dev/null)
    echo ${b#refs/heads/}
}

# List of all applied patches.
_applied_patches ()
{
    local g=$(_gitdir)
    [ "$g" ] && cat "$g/patches/$(_current_branch)/applied"
}

# List of all unapplied patches.
_unapplied_patches ()
{
    local g=$(_gitdir)
    [ "$g" ] && cat "$g/patches/$(_current_branch)/unapplied"
}

# List of all patches.
_all_patches ()
{
    local b=$(_current_branch)
    local g=$(_gitdir)
    [ "$g" ] && cat "$g/patches/$b/applied" "$g/patches/$b/unapplied"
}

# List of all patches except the current patch.
_all_other_patches ()
{
    local b=$(_current_branch)
    local g=$(_gitdir)
    [ "$g" ] && cat "$g/patches/$b/applied" "$g/patches/$b/unapplied" \
        | grep -v "^$(< $g/patches/$b/current)$"
}

# Generate completions for patches and patch ranges from the given
# patch list function, and options from the given list.
_complete_patch_range ()
{
    local patchlist="$1" options="$2"
    local pfx cur="${COMP_WORDS[COMP_CWORD]}"
    case "$cur" in
        *..*)
            pfx="${cur%..*}.."
            cur="${cur#*..}"
            COMPREPLY=($(compgen -P "$pfx" -W "$($patchlist)" -- "$cur"))
            ;;
        *)
            COMPREPLY=($(compgen -W "$options $($patchlist)" -- "$cur"))
            ;;
    esac
}

# Generate completions for options from the given list.
_complete_options ()
{
    local options="$1"
    COMPREPLY=($(compgen -W "$options" -- "${COMP_WORDS[COMP_CWORD]}"))
}

_stg_delete ()
{
    _complete_patch_range _all_patches "--branch --help"
}

_stg_goto ()
{
    _complete_patch_range _all_other_patches "--help"
}

_stg_mail ()
{
    _complete_patch_range _all_patches \
        "--all --to --cc --bcc --auto --noreply --version --prefix --template \
         --cover --edit-cover --edit-patches --sleep --refid --smtp-user \
         --smtp-password --branch --mbox --help"
}

_stg_new ()
{
    _complete_options "--message --showpatch --author --authname --authemail \
                       --authdate --commname --commemail --help"
}

_stg_pop ()
{
    _complete_patch_range _applied_patches "--all --number --keep --help"
}

_stg_push ()
{
    _complete_patch_range _unapplied_patches "--all --number --reverse \
                                              --merged --undo --help"
}

_stg_status ()
{
    _complete_options "--modified --new --deleted --conflict --unknown \
                       --noexclude --reset --help"
}

_stg ()
{
    local i c=1 command

    while [ $c -lt $COMP_CWORD ]; do
        if [ $c == 1 ]; then
            command="${COMP_WORDS[c]}"
        fi
        c=$((++c))
    done

    # Complete name of subcommand.
    if [ $c -eq $COMP_CWORD -a -z "$command" ]; then
        COMPREPLY=($(compgen \
            -W "--help --version \
                $(stg help|grep '^ '|sed 's/ *\([^ ]\) .*/\1/')" \
            -- "${COMP_WORDS[COMP_CWORD]}"))
        return;
    fi

    # Complete arguments to subcommands.
    case "$command" in
        delete) _stg_delete ;;
        goto)   _stg_goto ;;
        mail)   _stg_mail ;;
        new)    _stg_new ;;
        pop)    _stg_pop ;;
        push)   _stg_push ;;
        status) _stg_status ;;
        *)      COMPREPLY=() ;;
    esac
}

complete -o default -F _stg stg
