import textwrap
import stgit.commands
from stgit import argparse
import itertools

def fun(name, *body):
    return ['%s ()' % name, '{', list(body), '}']

def fun_desc(name, desc, *body):
    return ['# %s' % desc] + fun(name, *body)

def flatten(stuff, sep):
    r = stuff[0]
    for s in stuff[1:]:
        r.append(sep)
        r.extend(s)
    return r

def write(f, stuff, indent = 0):
    for s in stuff:
        if isinstance(s, str):
            f.write((' '*4*indent + s).rstrip() + '\n')
        else:
            write(f, s, indent + 1)

def patch_list_fun(type):
    return fun('_%s_patches' % type, 'local g=$(_gitdir)',
               'test "$g" && cat "$g/patches/$(_current_branch)/%s"' % type)

def file_list_fun(name, cmd):
    return fun('_%s_files' % name, 'local g=$(_gitdir)',
               'test "$g" && %s' % cmd)

def ref_list_fun(name, prefix):
    return fun(name, 'local g=$(_gitdir)',
               ("test \"$g\" && git show-ref | grep ' %s/' | sed 's,.* %s/,,'"
                % (prefix, prefix)))

def util():
    r = [fun_desc('_gitdir',
                  "The path to .git, or empty if we're not in a repository.",
                  'echo "$(git rev-parse --git-dir 2>/dev/null)"'),
         fun_desc('_current_branch',
                  "Name of the current branch, or empty if there isn't one.",
                  'local b=$(git symbolic-ref HEAD 2>/dev/null)',
                  'echo ${b#refs/heads/}'),
         fun_desc('_other_applied_patches',
                  'List of all applied patches except the current patch.',
                  'local b=$(_current_branch)',
                  'local g=$(_gitdir)',
                  ('test "$g" && cat "$g/patches/$b/applied" | grep -v'
                   ' "^$(tail -n 1 $g/patches/$b/applied 2> /dev/null)$"')),
         fun('_patch_range', 'local patches="$1"', 'local cur="$2"',
             'case "$cur" in', [
                '*..*)', ['local pfx="${cur%..*}.."', 'cur="${cur#*..}"',
                          'compgen -P "$pfx" -W "$patches" -- "$cur"', ';;'],
                '*)', ['compgen -W "$patches" -- "$cur"', ';;']],
             'esac'),
         fun('_stg_branches',
             'local g=$(_gitdir)', 'test "$g" && (cd $g/patches/ && echo *)'),
         ref_list_fun('_all_branches', 'refs/heads'),
         ref_list_fun('_tags', 'refs/tags'),
         ref_list_fun('_remotes', 'refs/remotes')]
    for type in ['applied', 'unapplied', 'hidden']:
        r.append(patch_list_fun(type))
    for name, cmd in [('conflicting',
                       r"git ls-files --unmerged | sed 's/.*\t//g' | sort -u"),
                      ('dirty', 'git diff-index --name-only HEAD'),
                      ('unknown', 'git ls-files --others --exclude-standard'),
                      ('known', 'git ls-files')]:
        r.append(file_list_fun(name, cmd))
    return flatten(r, '')

def command_list(commands):
    return ['_stg_commands="%s"\n' % ' '.join(sorted(commands.iterkeys()))]

def command_fun(cmd, modname):
    mod = stgit.commands.get_command(modname)
    def cg(args, flags):
        return argparse.compjoin(list(args) + [argparse.strings(*flags)]
                                 ).command('$cur')
    return fun(
        '_stg_%s' % cmd,
        'local flags="%s"' % ' '.join(sorted(
                itertools.chain(
                    ('--help',),
                    (flag for opt in mod.options
                     for flag in opt.flags if flag.startswith('--'))))),
        'local prev="${COMP_WORDS[COMP_CWORD-1]}"',
        'local cur="${COMP_WORDS[COMP_CWORD]}"',
        'case "$prev" in', [
            '%s) COMPREPLY=($(%s)) ;;' % ('|'.join(opt.flags), cg(opt.args, []))
            for opt in mod.options if opt.args] + [
            '*) COMPREPLY=($(%s)) ;;' % cg(mod.args, ['$flags'])],
        'esac')

def main_switch(commands):
    return fun(
        '_stg',
        'local i',
        'local c=1',
        'local command',
        '',
        'while test $c -lt $COMP_CWORD; do', [
            'if test $c == 1; then', [
                'command="${COMP_WORDS[c]}"'],
            'fi',
            'c=$((++c))'],
        'done',
        '',
        ('# Complete name of subcommand if the user has not finished'
         ' typing it yet.'),
        'if test $c -eq $COMP_CWORD -a -z "$command"; then', [
            ('COMPREPLY=($(compgen -W "help version copyright $_stg_commands" --'
             ' "${COMP_WORDS[COMP_CWORD]}"))'),
            'return'],
        'fi',
        '',
        '# Complete arguments to subcommands.',
        'case "$command" in', [
            'help) ', [
            ('COMPREPLY=($(compgen -W "$_stg_commands" --'
             ' "${COMP_WORDS[COMP_CWORD]}"))'),
            'return ;;'],
            'version) return ;;',
            'copyright) return ;;'], [
            '%s) _stg_%s ;;' % (cmd, cmd)
            for cmd in sorted(commands.iterkeys())],
        'esac')

def install():
    return ['complete -o bashdefault -o default -F _stg stg 2>/dev/null \\', [
            '|| complete -o default -F _stg stg' ] ]

def write_completion(f):
    commands = stgit.commands.get_commands(allow_cached = False)
    r = [["""# -*- shell-script -*-
# bash completion script for StGit (automatically generated)
#
# To use these routines:
#
#    1. Copy this file to somewhere (e.g. ~/.stgit-completion.bash).
#
#    2. Add the following line to your .bashrc:
#         . ~/.stgit-completion.bash"""]]
    r += [util(), command_list(commands)]
    for cmd, (modname, _, _) in sorted(commands.iteritems()):
        r.append(command_fun(cmd, modname))
    r += [main_switch(commands), install()]
    write(f, flatten(r, ''))
