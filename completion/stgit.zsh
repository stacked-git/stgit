#compdef stg

# This script implements zsh completions for StGit (stg).
#
# To use these completions, copy to a directory in $fpath as _stgit.
# For example:
#
#     $ mkdir ~/.zsh.d
#     $ stg completion zsh >~/.zsh.d/_stgit
#     $ $EDITOR ~/.zshrc
#
#       fpath=("$HOME/.zsh.d" $fpath)
#       autoload -U compinit
#

_stg-branch() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    integer ret=1

    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        ': :->command'
        '*:: :->option-or-argument'
    )

    _arguments -C $subcmd_args && ret=0

    case $state in
        (command)
            declare -a commands switch_options
            commands=(
                {-l,--list}':list branches'
                {-c,--create}':create and switch to new branch'
                {-C,--clone}':clone current branch to new branch'
                {-r,--rename}':rename existing branch'
                {-p,--protect}':prevent stg from modifying branch'
                {-u,--unprotect}':allow stg to modify branch'
                {-D,--delete}':delete branch'
                '--cleanup:cleanup stg metadata for branch'
                {-d,--describe}':set branch description'
            )
            switch_options=(
                '--merge:merge worktree changes into other branch'
            )
            _alternative \
                'command: : _describe -t commands command commands' \
                'switch-option: : _describe -t switch-options switch-option switch_options' \
                'branch: : __stg_git_branch_names' && ret=0
            ;;

        (option-or-argument)
            curcontext=${curcontext%:*}-$line[1]
            case $line[1] in
                (--cleanup)
                    _call_function ret _stg-branch-cleanup ;;
                (-C|--clone)
                    _call_function ret _stg-branch-clone ;;
                (-c|--create)
                    _call_function ret _stg-branch-create ;;
                (-D|--delete)
                    _call_function ret _stg-branch-delete ;;
                (-d|--describe|--description)
                    _call_function ret _stg-branch-describe ;;
                (-l|--list)
                    _call_function ret _stg-branch-list ;;
                (-p|--protect)
                    _call_function ret _stg-branch-protect ;;
                (-r|--rename)
                    _call_function ret _stg-branch-rename ;;
                (-u|--unprotect)
                    _call_function ret _stg-branch-unprotect ;;

                # Options and arguments for the default command (switch branch).
                (--merge)
                    _call_function ret _stg-branch-switch has-merge ;;
                (*)
                    _call_function ret _stg-branch-switch has-branch ;;
            esac
            ;;
    esac

    return ret
}

_stg-branch-switch() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    if [ "$1" != "has-merge" ]; then
        subcmd_args+=('--merge[merge worktree changes into other branch]')
    fi
    if [ "$1" != "has-branch" ]; then
        subcmd_args+=(':branch:__stg_git_branch_names')
    fi
    _arguments -S $subcmd_args
}

_stg-branch-cleanup() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '--force[force cleanup when series is non-empty]'
        ':stgit branch:__stg_stgit_branch_names'
    )
    _arguments -S $subcmd_args
}

_stg-branch-clone() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments $subcmd_args ':new-branch:'
}

_stg-branch-create() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments -s -S $subcmd_args ':new-branch:' ':committish:'
}

_stg-branch-delete() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '--force[force cleanup when series is non-empty]'
        ':branch:__stg_git_branch_names'
    )
    _arguments -s -S $subcmd_args
}

_stg-branch-describe() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments -s -S $subcmd_args ':description:' ':branch:__stg_git_branch_names'
}

_stg-branch-list() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments $subcmd_args
}

_stg-branch-protect() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments $subcmd_args ':branch:__stg_stgit_branch_names'
}

_stg-branch-rename() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        ':old or new branch name:__stg_git_branch_names'
        '::new branch name:__stg_git_branch_names'
    )
    _arguments $subcmd_args
}

_stg-branch-unprotect() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    _arguments $subcmd_args ':branch:__stg_stgit_branch_names'
}

_stg-clean() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '(-A --applied)'{-A,--applied}'[delete empty applied patches]'
        '(-U --unapplied)'{-U,--unapplied}'[delete empty unapplied patches]'
    )
    _arguments -s -S $subcmd_args
}

_stg-commit() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '--allow-empty[allow committing empty patches]'
        - group-all
        '(-a --all)'{-a,--all}'[commit all applied patches]'
        - group-number
        '(-n --number)'{-n+,--number=}'[commit specified number of patches]:number'
        - group-patches
        '*:applied patches:__stg_dedup_inside_arguments __stg_patchrange --suggest-range --applied'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                bash:'generate bash completion script'
                fish:'generate fish shell completion script'
                zsh:'generate zsh completion script'
                list:'list StGit command information'
                man:'generate asciidoc man pages'
                help:'show help for given subcommand'
            )
            _describe -t commands 'completion command' command_list
            ;;
        (option-or-argument)
            curcontext=${curcontext%:*:*}:stg-completion-$words[1]
            if ! _call_function ret _stg-completion-$words[1]; then
                _message "unknown subcommand: $words[1]"
            fi
    esac
    return ret
}

_stg-completion-bash() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-fish() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-zsh() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-man() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_directories'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-help() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    subcmd_args+=(
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                bash:'generate bash completion script'
                fish:'generate fish shell completion script'
                zsh:'generate zsh completion script'
                list:'list StGit command information'
                help:'show help for given subcommand'
            )
            _describe -t commands 'completion command' command_list
            ;;
        (option-or-argument)
            curcontext=${curcontext%:*:*}:stg-completion-$words[1]-help
            _call_function ret _stg-completion-$words[1]-help
            ;;
    esac
    return ret
}

_stg-completion-list() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
        '--style=-[output format style]:style:(name-only fish zsh asciidoc)'
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                aliases:'list aliases'
                commands:'list StGit commands'
                commands-and-aliases:'list StGit commands and aliases'
                help:'show help for given subcommand'
            )
            _describe -t commands 'completion command' command_list
            ;;
        (option-or-argument)
            curcontext=${curcontext%:*:*}:stg-completion-list-$words[1]
            if ! _call_function ret _stg-completion-list-$words[1]; then
                _message "unknown subcommand: $words[1]"
            fi
            ;;
    esac
    return ret
}

_stg-completion-list-help() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    subcmd_args+=(
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                aliases:'list aliases'
                commands:'list StGit commands'
                commands-and-aliases:'list StGit commands and aliases'
                help:'show help for given subcommand'
            )
            _describe -t commands 'completion command' command_list
            ;;
        (option-or-argument)
            ;;
    esac
    return ret
}

_stg-completion-list-aliases() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
        '--style=-[output format style]:style:(name-only fish zsh asciidoc)'
        '--show-expansion[show alias expansion]'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-list-commands() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
        '--style=-[output format style]:style:(name-only fish zsh asciidoc)'
    )
    _arguments -s -S $subcmd_args
}

_stg-completion-list-commands-and-aliases() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-o --output)'{-o,--output=}'[output to path]: :_files'
        '--style=-[output format style]:style:(name-only fish zsh asciidoc)'
    )
    _arguments -s -S $subcmd_args
}

_stg-delete() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    __stg_add_args_push_conflicts
    subcmd_args+=(
        '--spill[spill patch contents to worktree and index]'
        - group-top
        '(-t --top)'{-t,--top}'[delete top patch]'
        - group-patchnames
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-diff() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_diffopt
    subcmd_args+=(
        '(-r --range)'{-r,--range=}'[show diff between revisions]: :__stg_patchrange --suggest-range --all'
        '(-s --stat)'{-s,--stat}'[show stat instead of diff]'
        '*:files:__stg_changed_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-edit() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_author
    __stg_add_args_edit
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_hook
    __stg_add_args_savetemplate
    __stg_add_args_trailers
    subcmd_args+=(
        '(-d --diff)'{-d,--diff}'[edit patch diff]'
        '(-t --set-tree)'{-t,--set-tree=}'[set git tree of patch]:treeish'
        ':patch:__stg_patch --all'
    )
    __stg_add_args_message
    _arguments -s -S $subcmd_args
}

_stg-email() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    __stg_add_args_help
    __stg_add_args_color
    subcmd_args+=(
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                format:'format patches as email files'
                send:'send patches as emails'
                help:'show help for given subcommand'
            )
            _describe -t commands 'email command' command_list
            ;;
        (option-or-argument)
            curcontext=${curcontext%:*:*}:stg-email-$words[1]
            if ! _call_function ret _stg-email-$words[1]; then
                _message "unknown subcommand: $words[1]"
            fi
            ;;
    esac
    return ret
}

_stg-email-format() {
    local curcontext=$curcontext state line ret=1
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    subcmd_args+=(
        '*'{-G+,--git-opt=}'[extra option for git-format-patch]:opt:__stg_git_format_patch_opts'
        '(-o --output-directory)'{-o+,--output-directory=}'[store resulting files in given directory]: :_directories'
        '(-n --numbered -N --no-numbered -k --keep-subject)'{-n,--numbered}'[name output in \[PATCH n/m\] format]'
        '(-n --numbered -N --no-numbered -k --keep-subject)'{-N,--no-numbered}'[name output in \[PATCH\] format]'
        '--start-number=[start numbering patches at given number]: :_numbers -l 1 "patch number"'
        '--numbered-files[use only number for file name]'
        '(-n --numbered -N --no-numbered -k --keep-subject --rfc --subject-prefix)'{-k,--keep-subject}"[don't strip/add \[PATCH\] from the first line of the commit message]"
        '(-s --signoff)'{-s,--signoff}'[add Signed-off-by: trailer to the commit message]'
        '(         --inline)--attach[create attachments instead of inlining patches]'
        '(--attach         )--inline[inline patches]'
        '(--thread            )--no-thread[do not thread messages]'
        '(         --no-thread)--thread=-[make the second and subsequent mails refer to the first]::style:((shallow\:"all refer to the first"
                                                                                                            deep\:"each refers to the previous"))'
        '--in-reply-to=[make the first mail a reply to the given message]:message id'
        '(-v --reroll-count)'{-v+,--reroll-count=}'[mark the series as the <n>-th iteration of the topic]: :_numbers iteration'
        '(-k --keep-subject --subject-prefix)--rfc[use \[RFC PATCH\] instead of \[PATCH\]]'
        '(-k --keep-subject --rfc)--subject-prefix=[use the given prefix instead of \[PATCH\]]:prefix'
        '(--no-to)*--to=[add To: header to email headers]: :_email_addresses'
        '--no-to[discard all To: headers added so far]'
        '(--no-cc)*--cc=[add Cc: header to email headers]: :_email_addresses'
        '--no-cc[discard all Cc: headers added so far]'
        '*--add-header=[add an arbitrary header to email headers]:header' \
        '--cover-letter[generate a cover letter]'
        '(            --no-signature --signature-file)--signature=[add a signature]:signature'
        '(--signature                --signature-file)--no-signature[do not add a signature]'
        '(--signature --no-signature                 )--signature-file=[use contents of file as signature]: :_files'
        '--base=[add prerequisite tree info to the patch series]:prereq commit:__stg_revisions'
        '--suffix=[use the given suffix for filenames]:filename suffix'
        '(-q --quiet)'{-q,--quiet}'[suppress the output of the names of generated files]'
        '--no-binary[do not output contents of changes in binary files, only note that they differ]'
        '--zero-commit[output all-zero hash in From header]'
        '--progress[show progress while generating patches]'
        '--interdiff=[insert interdiff against previous patch series in cover letter or single patch]:reference to tip of previous series:__stg_revisions'
        '--range-diff=[insert range-diff against previous patch series in cover letter or single patch]:reference to tip of previous series:__stg_revisions'
        '--creation-factor=[for range-diff, specify weighting for creation]:weighting (percent)'
        + '(sources)'
        '(-a --all)'{-a,--all}'[format all applied patches]'
        ': :->patch-or-patch-range'
    )
    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (patch-or-patch-range)
            __stg_patchrange --suggest-range && ret=0
            ;;
    esac

    return ret
}

_stg-email-send() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    subcmd_args+=(
        '*'{-G+,--git-opt=}'[extra option for git-send-email]:opt:__stg_git_send_email_opts'
        '--from=[specify sender]:email address:_email_addresses'
        '--to=[specify the primary recipient of the emails]: :_email_addresses'
        '--cc=[starting Cc: value for each email]: :_email_addresses'
        '--bcc=[Bcc: value for each email]: :_email_addresses'
        '--subject=[specify the initial subject of the email thread]:subject'
        '--reply-to=[specify Reply-To address]:email address:_email_addresses'
        '--in-reply-to=[specify contents of first In-Reply-To header]:message-id'
        '--compose[edit introductory message for patch series]'
        '--annotate[review each patch in an editor]'
        '--identity=[specify configuration identity]: :__stg_email_send_identities'
        '--no-thread[do not set In-Reply-To: and References: headers]'
        '--confirm[specify type of confirmation required before sending]: :((
            always\:"always confirm before sending"
            never\:"never confirm before sending"
            cc\:"confirm before sending to automatically added Cc-addresses"
            compose\:"confirm before sending first message when using --compose"
            auto\:"same as cc together with compose"
        ))'
        '--quiet[be less verbose]'
        '--dry-run[do everything except actually sending the emails]'
        + '(sources)'
        '(-a --all)'{-a,--all}'[send all applied patches]'
        '(- *)--dump-aliases[dump configured aliases and exit]'
        '*: : _alternative -O expl
            "files:file:_files"
            "patchrange::__stg_patchrange --suggest-range"'
    )
    _arguments -s -S $subcmd_args
}

_stg-email-help() {
    local -a subcmd_args
    local curcontext="$curcontext" state line
    subcmd_args+=(
        '(-): :->command'
        '(-)*:: :->option-or-argument'
    )

    integer ret=1

    _arguments -s -S $subcmd_args && ret=0

    case $state in
        (command)
            local -a command_list=(
                format:'format patches as email files'
                send:'send patches as emails'
                help:'show help for given subcommand'
            )
            _describe -t commands 'email command' command_list
            ;;
        (option-or-argument)
            curcontext=${curcontext%:*:*}:stg-email-$words[1]-help
            _call_function ret _stg-email-$words[1]-help
            ;;
    esac
    return ret
}

_stg-export() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    __stg_add_args_diffopt
    subcmd_args+=(
        '(-d --dir)'{-d,--dir}'[export patches to directory]: :_directories'
        '(-n --numbered)'{-n,--numbered}'[prefix patch names with order numbers]'
        '(-s --stdout)'{-s,--stdout}'[dump patches to standard output]'
        '(-t --template)'{-t,--template=}'[use template file]: :_files'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange'
        + '(suffix)'
        '(-e --extension)'{-e,--extension=}'[extension to append to patch names]:extension'
        '(-p --patch)'{-p,--patch}'[append .patch to patch names]'
    )
    _arguments -s -S $subcmd_args
}

_stg-files() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '--bare[bare file names]'
        '(-s --stat)'{-s,--stat}'[show diff stat]'
        ':patches:__stg_patch --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-float() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_keep
    __stg_add_args_committer_date_is_author_date
    subcmd_args+=(
        '--noapply[Reorder patches by floating without applying]'
        '(-S --series)'{-S,--series=}'[arrange according to series file]: :_files'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-fold() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '(-b --base)'{-b,--base=}'[apply on base commit instead of HEAD]:commit'
        '(-p --strip)'{-p+,--strip=}'[remove N leading directories from diff paths]:num'
        '-C=[ensure N lines of surrounding context for each change]:num'
        '--reject[leave rejected hunks in .rej files]'
        ':file:_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-goto() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_keep
    __stg_add_args_merged
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_push_conflicts
    subcmd_args+=(
        ':patches:__stg_patch --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-help() {
    _arguments -s ':commands:__stg_subcommands'
}

_stg-hide() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    subcmd_args+=(
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange'
    )
    _arguments -s -S $subcmd_args
}

_stg-id() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    subcmd_args+=(
        ':references:__stg_patch --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-import() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_author
    __stg_add_args_edit
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_trailers
    subcmd_args+=(
        '(-n --name)'{-n,--name}'[name for imported patch]'
        '(-p --strip)'{-p+,--strip=}'[remove N leading directories from diff paths]:num'
        '--directory[prepend root to all filenames]:root:_directories'
        '(-t --stripname)'{-t,--stripname}'[strip number and extension from patch name]'
        '-C=[ensure N lines of surrounding context for each change]:num'
        '(-3 --3way)'{-3,--3way}'[attempt three-way merge]'
        '(-i --ignore)'{-i,--ignore}'[ignore applied patches in series]'
        '--replace[replace unapplied patches in series]'
        '--reject[leave rejected hunks in .rej files]'
        '--keep-cr[do not remove CR from email lines ending with CRLF]'
        '--message-id[create Message-ID trailer from email header]'
        '(-d --showdiff)'{-d,--showdiff}'[show patch content in editor buffer]'
        ':file:_files'
        + '(source)'
        '(-m --mail)'{-m,--mail}'[import from standard email file]'
        '(-M --mbox)'{-M,--mbox}'[import from mbox file]'
        '(-S --series)'{-S,--series}'[import from series file]'
        '(-u --url)'{-u,--url}'[import patch from URL]'
    )
    _arguments -s -S $subcmd_args
}

_stg-init() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    _arguments -s $subcmd_args
}

_stg-log() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    subcmd_args+=(
        '--clear[clear log history]'
        '(-d --diff)'{-d,--diff}'[show refresh diffs]'
        '(-f --full)'{-f,--full}'[show full commit ids]'
        '(-g --graphical)'{-g,--graphical}'[show log in gitk]'
        '(-n --number)'{-n+,--number=}'[limit to number of commits]'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-new() {
    local curcontext=$curcontext state line ret=1
    local -a subcmd_args
    declare -A opt_args

    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_edit
    __stg_add_args_author
    __stg_add_args_trailers
    __stg_add_args_hook
    __stg_add_args_savetemplate
    subcmd_args+=(
        '(-d --diff)'{-d,--diff}'[show diff when editing patch message]'
        '(-n --name)'{-n,--name=}'[name for new patch]:patchname'
        '(-r --refresh)'{-r,--refresh}'[refresh new patch]'
        '(-F --force)'{-F,--force}'[force refresh even if index is dirty]'
        '(-i --index)'{-i,--index}'[refresh from index instead of worktree]'
        '(-)--[start file arguments]: :->modified-file'
    )
    if [[ $words[(I)--] = "0" && ${words[(I)-n|--name(=*|)]} = "0" ]]; then
        subcmd_args+=(':: :_guard "([^-]?#|)" patchname')
    fi
    __stg_add_args_message
    _arguments -C -s $subcmd_args && ret=0

    case $state in
        (modified-file)
            __stg_dedup __stg_modified_files && ret=0
            ;;
    esac

    return ret
}

_stg-next() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    _arguments -s -S $subcmd_args
}

_stg-patches() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    __stg_add_args_diffopt
    subcmd_args+=(
        '(-d --diff)'{-d,--diff}'[show diffs of given files]'
        '*:files:__stg_cached_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-pick() {
    local -a subcmd_args
    # TODO: complete --parent commit id
    __stg_add_args_help
    __stg_add_args_committer_date_is_author_date
    subcmd_args+=(
        '(-n --name)'{-n,--name=}'[name for picked patch]:name'
        '(-B --ref-branch)'{-B,--ref-branch=}'[pick patches from branch]: :__stg_stgit_branch_names'
        '(-r --revert)'{-r,--revert}'[revert given commit object]'
        '(-p --parent=)'{-p,--parent}'[use commit id as parent]:commit'
        '(-x --expose)'{-x,--expose}'[append imported commit id to patch log]'
        '--noapply[keep patch unapplied]'
        '*'{-f,--file=}'[only fold given file]: :_files'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --use-ref-branch'
        + '(mode)'
        '--fold[fold the commit into current patch]'
        '--update[fold limited to current patch files]'
    )
    _arguments -s -S $subcmd_args
}

_stg-pop() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_keep
    subcmd_args+=(
        '(-s --spill)'{-s,--spill}'[pop a patch keeping its modifications in the tree]'
        - group-number
        '(-n --number)'{-n+,--number=}'[push specified number of patches]:number'
        - group-all
        '(-a --all)'{-a,--all}'[push all unapplied patches]'
        - group-patches
        '*:applied patches:__stg_dedup_inside_arguments __stg_patchrange --applied'
    )
    _arguments -s -S $subcmd_args
}

_stg-prev() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    _arguments -s -S $subcmd_args
}

_stg-pull() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_merged
    __stg_add_args_push_conflicts
    subcmd_args+=(
        '(-n --nopush)'{-n,--nopush}'[do not push patches after rebasing]'
        ':repository:__stg_remotes'
    )
    _arguments -s -S $subcmd_args
}

_stg-push() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_keep
    __stg_add_args_merged
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_push_conflicts
    subcmd_args+=(
        '--reverse[push patches in reverse order]'
        '--noapply[push without applying]'
        '--set-tree[push patch with the original tree]'
        - group-all
        '(-a --all)'{-a,--all}'[push all unapplied patches]'
        - group-number
        '(-n --number)'{-n+,--number=}'[push specified number of patches]:number'
        - group-patches
        '*:unapplied patches:__stg_dedup_inside_arguments __stg_patchrange --unapplied'
    )
    _arguments -s -S $subcmd_args
}

_stg-rebase() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_merged
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_push_conflicts
    subcmd_args+=(
        '(-n --nopush)'{-n,--nopush}'[do not push patches after rebasing]'
        '(-i --interactive)'{-i,--interactive}'[interactively manipulate patches in editor]'
        '--autostash[Stash changes before rebase and reapply them after]'
        ':new-base-id:__stg_heads'
    )
    _arguments -s -S $subcmd_args
}

_stg-redo() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '--hard[discard changes in index/worktree]'
        '(-n --number)'{-n+,--number=}'[number of undos to redo]:number'
    )
    _arguments -s -S $subcmd_args
}

_stg-refresh() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_author
    __stg_add_args_edit
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_hook
    __stg_add_args_trailers
    __stg_add_args_diffopt
    __stg_add_args_push_conflicts
    subcmd_args+=(
        '(-a --annotate)'{-a,--annotate=}'[annotate patch log entry]:note'
        '(-d --diff)'{-d,--diff}'[show diff when editing patch message]'
        '(-F --force)'{-F,--force}'[force refresh even if index is dirty]'
        '(-i --index)'{-i,--index}'[refresh from index instead of worktree]'
        '(-p --patch)'{-p,--patch=}'[refresh patch other than top patch]: :__stg_patch --all'
        '--spill[Spill patch contents to worktree and index, and erase patch content]'
        + '(update-files)'
        '(-u --update)'{-u,--update}'[only update current patch files]'
        '*:files:__stg_modified_files'
        + '(submodules)'
        '(-s --submodules)'{-s,--submodules}'[include submodules in refresh]'
        '--no-submodules[exclude submodules from refresh]'
    )
    __stg_add_args_message
    _arguments -s -S $subcmd_args
}

_stg-rename() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    __stg_add_args_color
    subcmd_args+=(
        ':old-patch:__stg_patch --all'
        ':new patch name:'
    )
    _arguments -s -S $subcmd_args
}

_stg-repair() {
    local -a subcmd_args
    __stg_add_args_help
    _arguments -s $subcmd_args
}

_stg-reset() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '--hard[discard changes in index/worktree]'
        ':state:'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --all'
    )
    _arguments -s -S $subcmd_args
}

_stg-series() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    __stg_add_args_color
    subcmd_args+=(
        '--author[display the author name for each patch]'
        '(-c --count)'{-c,--count}'[print number of patches]'
        '(-i --commit-id)'{-i,--commit-id}=-'[display commit ids]::length'
        '(-d --description)'{-d,--description}'[display short descriptions]'
        '(-e --empty)'{-e,--empty}'[identify empty patches]'
        '(-I --indices)'{-I,--indices}'[display absolute indices of patches]'
        '(-m --missing)'{-m,--missing=}'[show patches from branch missing in current]: :__stg_stgit_branch_names'
        '(-O --offsets)'{-O,--offsets}'[display relative offsets of patches]'
        '--prefix[display patch status prefix]'
        '(-P --no-prefix)'{-P,--no-prefix}'[do not display the patch status prefix]'
        '(-r --reverse)'{-s,--reverse}'[display in reverse order]'
        '(-s --short)'{-s,--short}'[list just patches around the topmost patch]'
        '--showbranch[display branch name of listed patches]'
        '--no-author[do not display patch author]'
        '--no-commit-id[do not display commit ids]'
        '--no-description[do not display patch descriptions]'
        '--no-empty[do not identify empty patches]'
        '--no-indices[do not display patch indices]'
        '--no-offsets[do not display patch offsets]'
        '--no-reverse[do not display in reverse order]'
        '--no-showbranch[do not display branch name]'
        - group-ahu
        '(-A --applied)'{-A,--applied}'[show applied patches]'
        '(-H --hidden)'{-H,--hidden}'[show hidden patches]'
        '(-U --unapplied)'{-U,--unapplied}'[show unapplied patches]'
        - group-all
        '(-a --all)'{-a,--all}'[show all patches including hidden]'
        - group-patches
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --all --suggest-range'
    )
    _arguments -s -S $subcmd_args
}

_stg-show() {
    local -a subcmd_args
    local curcontext=$curcontext state line ret=1
    declare -A opt_args

    __stg_add_args_help
    __stg_add_args_branch
    __stg_add_args_diffopt
    subcmd_args+=(
        '(*)'{-p,--patch=}'[patch or revision to show]: :__stg_dedup_inside_arguments __stg_patchrange --all'
        '(-s --stat)'{-s,--stat}'[show diff stat]'
        '(-)--[start file arguments]: :->cached-files'
        '(-A --applied *)'{-A,--applied}'[show applied patches]'
        '(-U --unapplied *)'{-U,--unapplied}'[show unapplied patches]'
        '(-H --hidden *)'{-H,--hidden}'[show hidden patches]'
        '(-A --applied -U --unapplied -H --hidden -p --patch)*:patches:__stg_dedup_inside_arguments __stg_patchrange --all'
    )
    _arguments -C -s $subcmd_args && ret=0

    case $state in
        (cached-files)
            __stg_dedup __stg_cached_files
            ;;
    esac

    return ret
}

_stg-sink() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_keep
    __stg_add_args_committer_date_is_author_date
    subcmd_args+=(
        '(-n --nopush)'{-n,--nopush}'[do not push patches after sinking]'
        '(-T --above -t --to --below)'{-t,--to=,--below=}'[sink patches below target patch]: :__stg_patch --applied'
        '(-T --above -t --to --below)'{-T,--above=}'[sink patches above target patch]: :__stg_patch --applied'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange'
    )
    _arguments -s -S $subcmd_args
}

_stg-spill() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_committer_date_is_author_date
    subcmd_args+=(
        '(-a --annotate)'{-a,--annotate}'[annotate patch log entry]:annotation'
        '(-r --reset)'{-r,--reset}'[also reset the index]'
        '(-)--[start file arguments]: :->patch-files'
        '*:: :->patch-files'
    )

    _arguments -C -s $subcmd_args && ret=0

    case $state in
        (patch-files)
            __stg_dedup __stg_patch_files
            ;;
    esac

    return ret
}

_stg-squash() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_author
    __stg_add_args_edit
    __stg_add_args_committer_date_is_author_date
    __stg_add_args_hook
    __stg_add_args_savetemplate
    __stg_add_args_trailers
    subcmd_args+=(
        '(-n --name)'{-n,--name=}'[name for squashed patch]: :__stg_patch --all'
        '*:patches:__stg_dedup_inside_arguments __stg_patch --all'
    )
    __stg_add_args_message
    _arguments -s -S $subcmd_args
}

_stg-sync() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_committer_date_is_author_date
    subcmd_args+=(
        + '(patches)'
        '(-a --all)'{-a,--all}'[synchronize all applied patches]'
        '*:patches:__stg_dedup_inside_arguments __stg_patchrange --suggest-range --use-ref-branch'
        + '(source)'
        '(-B --ref-branch)'{-B,--ref-branch}'[synchronize patches with branch]: :__stg_stgit_branch_names'
        '(-S --series)'{-S,--series=}'[synchronize patches with series]: :_files'
    )
    _arguments -s -S $subcmd_args
}

_stg-top() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_color
    __stg_add_args_branch
    _arguments -s -S $subcmd_args
}

_stg-uncommit() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        - group-number
        '(-n --number)'{-n+,--number=}'[push specified number of patches]:number'
        ':prefix:'
        - group-to
        '(-t --to)'{-t,--to=}'[uncommit to the specified commit]:commit'
        '(-x --exclusive)'{-x,--exclusive}'[exclude the commit specified by --to]'
        - group-names
        '*: :_guard "([^-]?#|)" names'
    )
    _arguments -s -S $subcmd_args
}

_stg-undo() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '--hard[discard changes in index/worktree]'
        '(-n --number)'{-n+,--number=}'[number commands to undo]:number'
    )
    _arguments -s -S $subcmd_args
}

_stg-unhide() {
    local -a subcmd_args
    __stg_add_args_help
    __stg_add_args_branch
    subcmd_args+=(
        ':patches:__stg_dedup_inside_arguments __stg_patchrange --hidden'
    )
    _arguments -s -S $subcmd_args
}

_stg-version() {
    local -a subcmd_args
    __stg_add_args_help
    subcmd_args+=(
        '(-s --short)'{-s,--short}'[show abbreviated version information]'
    )
    _arguments -s -S $subcmd_args
}

__stg_add_args_author() {
    subcmd_args+=(
        '--author=[set author details]'
        '--authdate=[set author date]:date'
        '--authemail=[set author email]:email'
        '--authname=[set author name]:name'
    )
}

__stg_add_args_branch() {
    subcmd_args+=(
        '(-b --branch)'{-b,--branch=}'[specify another branch]: :__stg_stgit_branch_names'
    )
}

__stg_add_args_color() {
    subcmd_args+=(
        '--color=-[when to colorize output]:when:((
            auto\:"color when outputting to a TTY"
            always\:"always use color"
            ansi\:"force color with ANSI escape sequences"
            never\:"never use color"))'
    )
}

__stg_add_args_diffopt() {
    subcmd_args+=(
        '*'{-O+,--diff-opt=}'[extra option for git diff]:opt:__stg_git_diff_opts'
    )
}

__stg_add_args_edit() {
    subcmd_args+=(
        '(-e --edit)'{-e,--edit}'[invoke interactive editor]'
    )
}

__stg_add_args_help() {
    subcmd_args+=(
        '(- *)'{-h,--help}'[show help message and exit]'
    )
}

__stg_add_args_hook() {
    subcmd_args+=(
        '--no-verify[disable commit-msg hook]'
    )
}

__stg_add_args_keep() {
    subcmd_args+=(
        '(-k --keep)'{-k,--keep}'[keep local changes]'
    )
}

__stg_add_args_merged() {
    subcmd_args+=(
        '(-m --merged)'{-m,--merged}'[check for patches merged upstream]'
    )
}

__stg_add_args_push_conflicts() {
    subcmd_args+=(
        '--conflicts=-[allow pushing patches that may result in merge conflicts]:policy:((
            allow\:"allow pushing patches with conflicts"
            disallow\:"disallow pushing patches with conflicts"))'
    )
}

__stg_add_args_message() {
    subcmd_args+=(
        + '(message)'
        '(-f --file)'{-f,--file=}'[use message file instead of invoking editor]: :_files'
        '(-m --message)'{-m,--message=}'[specify message instead of invoking editor]:message'
    )
}

__stg_add_args_savetemplate() {
    subcmd_args+=(
        '--save-template=[save message template to file and exit]: :_files'
    )
}

__stg_add_args_trailers() {
    subcmd_args+=(
        '--ack=-[add Acked-by trailer]'
        '--review=-[add Reviewed-by trailer]'
        '--signoff=-[add Signed-off-by trailer]'
    )
}

__stg_add_args_committer_date_is_author_date() {
    subcmd_args+=(
        '--committer-date-is-author-date[use author date as committer date]'
        '--cdiad[use author date as committer date]'
    )
}

__stg_complete_git_opts() {
    local git_cmd short long i
    git_cmd=$1
    short=$2
    long=$3

    # Parse short and long options (e.g. -O and --diff-opt) from $words.
    declare -a git_opts git_opts_after
    zmodload -F 'zsh/zutil' 'b:zparseopts'
    () { zparseopts -E -D -a git_opts ${short}+: -${long}+: 2>/dev/null; } \
        ${(@)words[1,CURRENT]}
    () { zparseopts -E -D -a git_opts_after ${short}+: -${long}+: 2>/dev/null; } \
        ${(@)words[CURRENT+1,-1]}

    # Compose git command line
    words=('git' ${(@)__stg_C_args} ${git_cmd})

    # The option values are at the even indexes in the git_opts array.
    # The last option is skipped because it is the partially specified one being
    # completed.
    if (( $#git_opts > 2 )); then
        for i in {1..$(($#git_opts - 2))..2}; do
            # Need to strip any leading '=' because zparseopts will parse, for example,
            # `--diff-opt=foo` as ('--diff-opt' '=foo').
            words+=(${git_opts[i+1]#=})
        done
    fi

    # If the user has not started typing the value, prime it with '-' to force
    # completing only the options (and not arguments) to the git command.
    : ${SUFFIX:-${PREFIX:=-}}
    words+=("$PREFIX$SUFFIX")
    (( CURRENT = $#words ))

    if (( $#git_opts_after > 2 )); then
        for i in {1..$(($#git_opts_after - 2))..2}; do
            words+=(${git_opts_after[i+1]#=})
        done
    fi

    _message -e "git-$git_cmd-option" "git $git_cmd" &&
        _dispatch git git $commands[git]
}

__stg_git_diff_opts() {
    __stg_complete_git_opts diff-tree O diff-opt
}

__stg_git_format_patch_opts() {
    __stg_complete_git_opts format-patch G git-opt
}

__stg_git_send_email_opts() {
    __stg_complete_git_opts send-email G git-opt
}

__stg_revisions () {
    _alternative \
        "heads::__stg_heads" \
        "commit-tags::__stg_commit_tags" \
        "patch-refs::__stg_patch_refs"
}

__stg_commit_tags () {
    local expl
    declare -a tags

    tags=(${${(M)${(f)"$(_call_program commit-tag-refs "git ${__stg_C_args} for-each-ref --format='%(*objecttype)%(objecttype) %(refname)' refs/tags 2>/dev/null")"}:#commit(tag|) *}#commit(tag|) refs/tags/})
    __stg_git_command_successful $pipestatus || return 1

    _wanted commit-tags expl "commit tag" compadd -M 'r:|/=* r:|=*' "$@" -o numeric -a - tags
}

__stg_patch_refs () {
    local expl
    declare -a refs

    refs=(${(f)"$(_call_program patch-refs "git ${__stg_C_args} for-each-ref --format='%(refname:lstrip=2)' refs/patches 2>/dev/null")"})
    __stg_git_command_successful $pipestatus || return 1

    _wanted commit-tags expl "patch ref" compadd -p refs/patches/ -M 'r:|/=* r:|=*' "$@" -o numeric -a - refs
}

__stg_heads () {
    _alternative 'heads-local::__stg_heads_local' 'heads-remote::__stg_heads_remote'
}

__stg_heads_local () {
    local f gitdir
    declare -a heads

    heads=(${(f)"$(_call_program headrefs git ${__stg_C_args} for-each-ref --format='"%(refname:short)"' refs/heads 2>/dev/null)"})
    gitdir=$(_call_program gitdir git rev-parse --git-dir 2>/dev/null)
    if __stg_git_command_successful $pipestatus; then
        for f in HEAD FETCH_HEAD ORIG_HEAD MERGE_HEAD; do
            [[ -f $gitdir/$f ]] && heads+=$f
        done
        [[ -f $gitdir/refs/stash ]] && heads+=stash
        [[ -f $gitdir/refs/bisect/bad ]] && heads+=bisect/bad
    fi

    __stg_git_describe_commit heads heads-local "local head" "$@"
}

__stg_heads_remote () {
  declare -a heads

  heads=(${(f)"$(_call_program headrefs git ${__stg_C_args} for-each-ref --format='"%(refname:short)"' refs/remotes 2>/dev/null)"})

  __stg_git_describe_commit heads heads-remote "remote head" "$@"
}

__stg_command_successful () {
    if (( ${#*:#0} > 0 )); then
        _message 'not a StGit branch'
        return 1
    fi
    return 0
}

__stg_git_command_successful () {
  if (( ${#*:#0} > 0 )); then
    _message 'not a git repository'
    return 1
  fi
  return 0
}

__stg_config_sections () {
    local regex tag desc
    local -a groups

    regex=$1
    tag=$2
    desc=$3

    groups=(${${${(0)"$(_call_program $tag "git ${__stg_C_args} config -z --get-regexp -- ${(q)regex}")"}#*.}%%.[^.]##$'\n'*})
    _describe -t $tag $desc groups
}

__stg_email_send_identities () {
    __stg_config_sections '^sendemail\..+\.[^.]+$' identities 'sendemail identity'
}

__stg_git_describe_commit () {
  __stg_git_describe_branch $1 $2 $3 -M 'r:|/=* r:|=*' "${(@)argv[4,-1]}"
}

__stg_git_describe_branch () {
  local __commits_in=$1
  local __tag=$2
  local __desc=$3
  shift 3

  integer maxverbose
  if zstyle -s :completion:$curcontext: max-verbose maxverbose &&
    (( ${compstate[nmatches]} <= maxverbose )); then
    local __c
    local -a __commits
    for __c in ${(P)__commits_in}; do
      __commits+=("${__c}:${$(_call_program describe git ${__stg_C_args} rev-list -1 --oneline $__c)//:/\\:}")
    done
    _describe -t $__tag $__desc __commits "$@"
  else
    local expl
    _wanted $__tag expl $__desc compadd "$@" -a - $__commits_in
  fi
}

__stg_stgit_branch_names () {
    local expl
    declare -a branch_names

    stgit_branches=(
        ${${(f)"$(_call_program branchrefs git ${__stg_C_args} for-each-ref --format='"%(refname)"' refs/stacks 2>/dev/null)"}#refs/stacks/}
    )
    __stg_git_command_successful $pipestatus || return 1

    __stg_git_describe_commit stgit_branches branch-names 'stgit branch name' "$@"
}

__stg_git_branch_names () {
  local expl
  declare -a branch_names

  branch_names=(${${(f)"$(_call_program branchrefs git for-each-ref --format='"%(refname)"' refs/heads 2>/dev/null)"}#refs/heads/})
  __stg_git_command_successful $pipestatus || return 1

  __stg_git_describe_commit branch_names branch-names 'branch name' "$@"
}

__stg_files_relative() {
    local prefix
    prefix=$(_call_program gitprefix git ${__stg_C_args} rev-parse --show-prefix 2>/dev/null)
    if (( $#prefix == 0 )); then
        print $1
        return
    fi

    local file
    local -a files

    files=()
    # Collapse "//" and "/./" into "/". Strip any remaining "/." and "/".
    for file in ${${${${${(0)1}//\/\///}//\/.\///}%/.}%/}; do
        integer i n
        (( n = $#file > $#prefix ? $#file : $#prefix ))
        for (( i = 1; i <= n; i++ )); do
            if [[ $file[i] != $prefix[i] ]]; then
                while (( i > 0 )) && [[ $file[i-1] != / ]]; do
                    (( i-- ))
                done
                break
            fi
        done

        files+=${(l@${#prefix[i,-1]//[^\/]}*3@@../@)}${file[i,-1]}
    done

    print ${(pj:\0:)files}
}

__stg_files () {
  local compadd_opts opts tag description gitcdup gitprefix files expl

  zparseopts -D -E -a compadd_opts V+: J+: 1 2 o+: n f x+: X+: M+: P: S: r: R: q F:
  zparseopts -D -E -a opts -- -cached -deleted -modified -others -ignored -unmerged -killed x+: --exclude+:
  tag=$1 description=$2; shift 2

  gitcdup=$(_call_program gitcdup git ${__stg_C_args} rev-parse --show-cdup 2>/dev/null)
  __stg_git_command_successful $pipestatus || return 1

  gitprefix=$(_call_program gitprefix git ${__stg_C_args} rev-parse --show-prefix 2>/dev/null)
  __stg_git_command_successful $pipestatus || return 1

  # TODO: --directory should probably be added to $opts when --others is given.

  local pref=$gitcdup$gitprefix$PREFIX

  # First allow ls-files to pattern-match in case of remote repository
  files=(${(0)"$(_call_program files git ${__stg_C_args} ls-files -z --exclude-standard ${(q)opts} -- ${(q)${pref:+$pref\*}:-.} 2>/dev/null)"})
  __stg_git_command_successful $pipestatus || return

  # If ls-files succeeded but returned nothing, try again with no pattern
  if [[ -z "$files" && -n "$pref" ]]; then
    files=(${(0)"$(_call_program files git ${__stg_C_args} ls-files -z --exclude-standard ${(q)opts} -- 2>/dev/null)"})
    __stg_git_command_successful $pipestatus || return
  fi

 # _wanted $tag expl $description _files -g '{'${(j:,:)files}'}' $compadd_opts -
  _wanted $tag expl $description _multi_parts -f $compadd_opts - / files
}

__stg_cached_files () {
  __stg_files --cached cached-files 'cached file' $*
}

__stg_modified_files () {
  __stg_files --modified modified-files 'modified file' $*
}

__stg_diff-index_files () {
  local tree=$1 description=$2 tag=$3; shift 3
  local files expl

  # $tree needs to be escaped for _call_program; matters for $tree = "HEAD^"
  files=$(_call_program files git ${__stg_C_args} diff-index -z --name-only --no-color --cached ${(q)tree} 2>/dev/null)
  __stg_git_command_successful $pipestatus || return 1
  files=(${(0)"$(__stg_files_relative $files)"})
  __stg_git_command_successful $pipestatus || return 1

  _wanted $tag expl $description _multi_parts $@ - / files
}

__stg_changed-in-index_files () {
  __stg_diff-index_files HEAD 'changed in index file' changed-in-index-files "$@"
}

__stg_changed-in-working-tree_files () {
  local files expl

  files=$(_call_program changed-in-working-tree-files git ${__stg_C_args} diff -z --name-only --no-color 2>/dev/null)
  __stg_git_command_successful $pipestatus || return 1
  files=(${(0)"$(__stg_files_relative $files)"})
  __stg_git_command_successful $pipestatus || return 1

  _wanted changed-in-working-tree-files expl 'changed in working tree file' _multi_parts $@ -f - / files
}

__stg_changed_files () {
  _alternative \
    'changed-in-index-files::__stg_changed-in-index_files' \
    'changed-in-working-tree-files::__stg_changed-in-working-tree_files'
}

__stg_patch_files () {
    local files expl top_id
    top_id=$(_call_program id stg ${__stg_C_args} id 2>/dev/null)
    __stg_git_command_successful $pipestatus || return 1
    files=$(_call_program patch-files git ${__stg_C_args} diff -z --name-only --no-color $top_id~ $top_id 2>/dev/null)
    __stg_git_command_successful $pipestatus || return 1
    files=(${(0)"$(__stg_files_relative $files)"})
    __stg_git_command_successful $pipestatus || return 1

    _wanted patch-files expl "patch files" _multi_parts $@ -f - / files
}

__stg_get_branch_opt() {
    local use_ref_branch short long i
    zparseopts -- -use-ref-branch=use_ref_branch
    if [[ -n "$use_ref_branch" ]]; then
        short="-B"
        long="--ref-branch"
    else
        short="-b"
        long="--branch"
    fi
    i=${words[(I)$short|$long(=*|)]}
    if (( i > 0 )); then
        case ${words[i]} in
        $short|$long)
            if (( i < $#words )); then
                echo "--branch=${words[i + 1]}"
            fi
            ;;
        *)
            echo "--branch=${words[i]#*=}"
            ;;
        esac
    fi
}

__stg_patch() {
    declare -a compadd_opts
    zparseopts -D -E -a compadd_opts V+: J+: 1 2 o+: n f x+: X+: M+: P: S: r: R: q F:

    local use_ref_branch branch_opt
    zparseopts -D -E -- -use-ref-branch=use_ref_branch
    branch_opt="$(__stg_get_branch_opt $use_ref_branch)"

    local expl
    declare -a patchlines patchnames
    local desc_flag
    zstyle -T ":completion:${curcontext}:" verbose && desc_flag="--description"
    patchlines=(${(f)"$(_call_program patches stg ${__stg_C_args} series $desc_flag $branch_opt $@ 2>/dev/null)"})
    __stg_command_successful $pipestatus || return 1
    local patchline
    for patchline in $patchlines; do
        patchnames+=("${(MS)${patchline[3,-1]%%\#*}##[[:graph:]]*[[:graph:]]}")
    done
    _wanted patches expl 'patch' compadd $compadd_opts -o nosort -l -d patchlines -a patchnames
}

__stg_patchrange() {
    # Remove/capture compadd options
    declare -a compadd_opts
    zparseopts -D -E -a compadd_opts V+: J+: 1 2 o+: n f x+: X+: M+: P: S: r: R: q F:

    # Remove/capture patch selection, --suggest-range, and --use-ref-branch options
    declare -a selection_opt
    local use_ref_branch suggest_range branch_opt
    zparseopts -D -E -a selection_opt -- -suggest-range=suggest_range -use-ref-branch=use_ref_branch -applied -unapplied -hidden -all
    branch_opt="$(__stg_get_branch_opt $use_ref_branch)"

    # Consult zstyle to determine whether to use verbose patch listings
    local desc_flag
    zstyle -T ":completion:${curcontext}:" verbose && desc_flag="--description"

    local expl
    declare -a patchlines patchnames
    if compset -P '*..'; then
        if [[ $IPREFIX != ".." ]]; then
            # If the command line has 'patch..' (but not plain '..'), use that
            # open-ended range as the selection. N.B. any leading '--option=' is
            # trimmed. This affects, e.g. `stg diff --range`.
            selection_opt=("${IPREFIX#--*=}")
        fi
        # Otherwise for plain '..', we leave the nominal selection as-is.
    elif [[ -n "$suggest_range" ]]; then
        # If --suggest-range is specified, suffix the initial patch with '..' to start a
        # range.
        compadd_opts+=(-S ..)
    fi
    patchlines=(${(f)"$(_call_program patches stg ${__stg_C_args} series $desc_flag $branch_opt $selection_opt 2>/dev/null)"})
    __stg_command_successful $pipestatus || return 1
    local patchline
    for patchline in $patchlines; do
        patchnames+=("${(MS)${patchline[3,-1]%%\#*}##[[:graph:]]*[[:graph:]]}")
    done
    _wanted patches expl 'patch' compadd $compadd_opts -o nosort -l -d patchlines -a patchnames
}

__stg_remotes() {
    local remotes expl
    remotes=(${(f)"$(_call_program remotes git ${__stg_C_args} remote 2>/dev/null)"})
    __stg_git_command_successful $pipestatus || return 1
    _wanted remotes expl remote compadd "$@" -a - remotes
}

__stg_subcommands() {
    local -a command_list
    command_list=(${(f)"$(_call_program commands stg ${__stg_C_args} completion list commands-and-aliases --style=zsh)"})
    __stg_git_command_successful $pipestatus || return 1
    _describe -t commands 'stgit command' command_list
}

# Used to filter already-used completions.
__stg_dedup () {
    local -a ignored=(${line:#${words[CURRENT]}})
    $* -F ignored
}

# Like __stg_dedup, but for use inside _arguments specs.
__stg_dedup_inside_arguments () {
    declare -a compadd_opts
    zparseopts -D -E -a compadd_opts V+: J+: 1 2 o+: n f x+: X+: M+: P: S: r: R: q F:
    __stg_dedup $* $compadd_opts
}

_stgit() {
    integer ret=1

    local curcontext="$curcontext" state line
    typeset -A opt_args

    _arguments -0 -C \
        '(- :)--help[print help information]' \
        '(- :)--version[display version information]' \
        '*-C[run as if stg was started in given path]: :_directories' \
        '--color=-[when to colorize output]:when:((
            auto\:"color when outputting to a TTY"
            always\:"always use color"
            ansi\:"force color with ANSI escape sequences"
            never\:"never use color"))' \
        '(-): :->command' \
        '(-)*:: :->option-or-argument' && ret=0

    local -a __stg_C_args __stg_C_dirs
    local p
    for p in ${(0)opt_args[-C]}; do
        __stg_C_args+=("-C" "$p")
        __stg_C_dirs+=("$p")
    done
    unset p

    case $state in
        (command)
            __stg_subcommands && ret=0
            ;;
        (option-or-argument)
            local a
            local -a alias_list
            alias_list=(${(f)"$(_call_program alias-list stg ${__stg_C_args} completion list aliases --show-expansion --style=zsh)"})
            __stg_git_command_successful $pipestatus || return 1
            for a in $alias_list; do
                if [[ $words[1] = "${a%%:*}" ]]; then
                    local -a aliasexp tmpwords
                    aliasexp=(${(z)"${a#*:}"})
                    tmpwords=($aliasexp)
                    [[ -z "${words[2,-1]}" ]] || tmpwords+=(${words[2,-1]})
                    [[ -n ${words[CURRENT]} ]] || tmpwords+=('')
                    (( CURRENT += ${#aliasexp} - 1 ))
                    words=("${tmpwords[@]}")
                    unset aliasexp tmpwords

                    if [[ $words[1] = \!* ]]; then
                        words[1]=${words[1]##\!}
                        local p
                        integer push_count=0
                        for p in ${__stg_C_dirs}; do
                            pushd -q $p && (( push_count++ ))
                        done
                        _normal && ret=0
                        while (( push_count )); do
                            popd -q
                            (( push_count-- ))
                        done
                        unset p push_count
                        return ret
                    fi

                    break
                fi
            done
            unset a alias_list

            curcontext=${curcontext%:*:*}:stg-$words[1]:
            local -a subcmd_args
            if ! _call_function ret _stg-$words[1]; then
                if zstyle -T :completion:$curcontext: use-fallback; then
                    _default && ret=0
                else
                    _message "unknown subcommand: $words[1]"
                fi
            fi
            ;;
    esac

    return ret
}

_stgit
