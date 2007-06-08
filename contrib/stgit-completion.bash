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

_stg_commands="
    add
    applied
    assimilate
    branch
    delete
    diff
    clean
    clone
    commit
    cp
    export
    files
    float
    fold
    goto
    hide
    id
    import
    init
    log
    mail
    new
    patches
    pick
    pop
    pull
    push
    rebase
    refresh
    rename
    resolved
    rm
    series
    show
    sink
    status
    sync
    top
    unapplied
    uncommit
    unhide
"

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

# List of all applied patches.
_hidden_patches ()
{
    local g=$(_gitdir)
    [ "$g" ] && cat "$g/patches/$(_current_branch)/hidden"
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
        | grep -v "^$(cat $g/patches/$b/current 2> /dev/null)$"
}

_all_branches ()
{
    local g=$(_gitdir)
    [ "$g" ] && (cd .git/patches/ && echo *)
}

# List the command options
_cmd_options ()
{
    stg $1 --help 2>/dev/null | grep -e " --[A-Za-z]" | sed -e "s/.*\(--[^ =]\+\).*/\1/"
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

_complete_patch_range_options ()
{
    local patchlist="$1" options="$2" patch_options="$3"
    local prev="${COMP_WORDS[COMP_CWORD-1]}"
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local popt
    for popt in $patch_options; do
        if [ $prev == $popt ]; then
            _complete_patch_range $patchlist
            return
        fi
    done
    COMPREPLY=($(compgen -W "$options" -- "$cur"))
}

_complete_branch ()
{
     COMPREPLY=($(compgen -W "$(_cmd_options $1) $($2)" -- "${COMP_WORDS[COMP_CWORD]}"))
}

# Generate completions for options from the given list.
_complete_options ()
{
    local options="$1"
    COMPREPLY=($(compgen -W "$options" -- "${COMP_WORDS[COMP_CWORD]}"))
}

_stg_common ()
{
    _complete_options "$(_cmd_options $1)"
}

_stg_patches ()
{
    _complete_patch_range "$2" "$(_cmd_options $1)"
}

_stg_patches_options ()
{
    _complete_patch_range_options "$2" "$(_cmd_options $1)" "$3"
}

_stg_help ()
{
    _complete_options "$_stg_commands"
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
            -W "--help --version copyright help $_stg_commands" \
            -- "${COMP_WORDS[COMP_CWORD]}"))
        return;
    fi

    # Complete arguments to subcommands.
    case "$command" in
        # generic commands
        help)   _stg_help ;;
        # repository commands
        id)     _stg_patches $command _all_patches ;;
        # stack commands
        float)  _stg_patches $command _all_patches ;;
        goto)   _stg_patches $command _all_other_patches ;;
        hide)   _stg_patches $command _unapplied_patches ;;
        pop)    _stg_patches $command _applied_patches ;;
        push)   _stg_patches $command _unapplied_patches ;;
        series) _stg_patches $command _all_patches ;;
        sink)   _stg_patches $command _all_patches ;;
        unhide) _stg_patches $command _hidden_patches ;;
        # patch commands
        delete) _stg_patches $command _all_patches ;;
        export) _stg_patches $command _applied_patches ;;
        files)  _stg_patches $command _all_patches ;;
        log)    _stg_patches $command _all_patches ;;
        mail)   _stg_patches $command _all_patches ;;
        pick)   _stg_patches $command _unapplied_patches ;;
        refresh)_stg_patches_options $command _applied_patches "-p --patch" ;;
        rename) _stg_patches $command _all_patches ;;
        show)   _stg_patches $command _all_patches ;;
        sync)   _stg_patches $command _applied_patches ;;
        # working-copy commands
        diff)   _stg_patches_options $command _applied_patches "-r --range" ;;
	# commands that usually raher accept branches
	branch) _complete_branch $command _all_branches ;;
	rebase) _complete_branch $command _all_branches ;;
        # all the other commands
        *)      _stg_common $command ;;
    esac
}

complete -o default -F _stg stg
