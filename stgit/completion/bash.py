# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import itertools

from stgit import argparse
from stgit.compat import text
import stgit.commands


class CompgenBase(object):
    def actions(self, var):
        return []

    def words(self, var):
        return []

    def command(self, var):
        cmd = ['compgen']
        for act in self.actions(var):
            cmd += ['-A', act]
        words = self.words(var)
        if words:
            cmd += ['-W', '"%s"' % ' '.join(words)]
        cmd += ['--', '"%s"' % var]
        return ' '.join(cmd)


class CompgenJoin(CompgenBase):
    def __init__(self, a, b):
        assert isinstance(a, CompgenBase)
        assert isinstance(b, CompgenBase)
        self.__a = a
        self.__b = b

    def words(self, var):
        union_words = self.__a.words(var)
        for b_word in self.__b.words(var):
            if b_word not in union_words:
                union_words.append(b_word)
        return union_words

    def actions(self, var):
        union_actions = self.__a.actions(var)
        for b_action in self.__b.actions(var):
            if b_action not in union_actions:
                union_actions.append(b_action)
        return union_actions


class Compgen(CompgenBase):
    def __init__(self, words=(), actions=()):
        self.__words = list(words)
        self.__actions = list(actions)

    def actions(self, var):
        return self.__actions

    def words(self, var):
        return self.__words


class patch_range(CompgenBase):
    def __init__(self, *endpoints):
        self.__endpoints = endpoints

    def words(self, var):
        words = []
        for e in self.__endpoints:
            assert not e.actions(var)
            for e_word in e.words(var):
                if e_word not in words:
                    words.append(e_word)
        return ['$(_patch_range "%s" "%s")' % (' '.join(words), var)]


def compjoin(compgens):
    comp = Compgen()
    for c in compgens:
        comp = CompgenJoin(comp, c)
    return comp


_arg_to_compgen = dict(
    all_branches=Compgen(['$(_all_branches)']),
    stg_branches=Compgen(['$(_stg_branches)']),
    applied_patches=Compgen(['$(_applied_patches)']),
    other_applied_patches=Compgen(['$(_other_applied_patches)']),
    unapplied_patches=Compgen(['$(_unapplied_patches)']),
    hidden_patches=Compgen(['$(_hidden_patches)']),
    commit=Compgen(['$(_all_branches) $(_tags) $(_remotes)']),
    conflicting_files=Compgen(['$(_conflicting_files)']),
    dirty_files=Compgen(['$(_dirty_files)']),
    unknown_files=Compgen(['$(_unknown_files)']),
    known_files=Compgen(['$(_known_files)']),
    repo=Compgen(actions=['directory']),
    dir=Compgen(actions=['directory']),
    files=Compgen(actions=['file']),
    mail_aliases=Compgen(['$(_mail_aliases)']),
)


def arg_to_compgen(arg):
    if isinstance(arg, argparse.patch_range):
        range_args = []
        for range_arg in arg:
            range_args.append(_arg_to_compgen[range_arg])
        return patch_range(*range_args)
    elif isinstance(arg, argparse.strings):
        return Compgen(arg)
    else:
        return _arg_to_compgen[arg]


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


def write(f, stuff, indent=0):
    for s in stuff:
        if isinstance(s, text):
            f.write((' ' * 4 * indent + s).rstrip() + '\n')
        else:
            write(f, s, indent + 1)


def patch_list_fun(type):
    return fun('_%s_patches' % type, 'stg series --noprefix --%s' % type)


def file_list_fun(name, cmd):
    return fun('_%s_files' % name, 'local g', 'g=$(_gitdir)', 'test "$g" && %s' % cmd)


def ref_list_fun(name, prefix):
    return fun(
        name,
        'local g',
        'g=$(_gitdir)',
        (
            "test \"$g\""
            " && git show-ref "
            " | grep ' %s/' | sed 's,.* %s/,,'" % (prefix, prefix)
        ),
    )


def util():
    r = [
        fun_desc(
            '_gitdir',
            "The path to .git, or empty if we're not in a repository.",
            'git rev-parse --git-dir 2>/dev/null',
        ),
        fun_desc(
            '_current_branch',
            "Name of the current branch, or empty if there isn't one.",
            'local b',
            'b=$(git symbolic-ref HEAD 2>/dev/null)',
            'echo "${b#refs/heads/}"',
        ),
        fun_desc(
            '_other_applied_patches',
            'List of all applied patches except the current patch.',
            'stg series --noprefix --applied | grep -v "^$(stg top)$"',
        ),
        fun(
            '_patch_range',
            'local patches="$1"',
            'local cur="$2"',
            'case "$cur" in',
            [
                '*..*)',
                [
                    'local pfx="${cur%..*}.."',
                    'cur="${cur#*..}"',
                    'compgen -P "$pfx" -W "$patches" -- "$cur"',
                    ';;',
                ],
                '*)',
                ['compgen -W "$patches" -- "$cur"', ';;'],
            ],
            'esac',
        ),
        fun(
            '_stg_branches',
            ('stg branch --list 2>/dev/null' ' | grep ". s" | cut -f2 | cut -d" " -f1'),
        ),
        fun('_all_branches', 'stg branch --list 2>/dev/null | cut -f2 | cut -d" " -f1'),
        fun(
            '_mail_aliases',
            (
                'git config --name-only --get-regexp "^mail\\.alias\\." '
                '| cut -d. -f 3'
            ),
        ),
        ref_list_fun('_tags', 'refs/tags'),
        ref_list_fun('_remotes', 'refs/remotes'),
    ]
    for type in ['applied', 'unapplied', 'hidden']:
        r.append(patch_list_fun(type))
    for name, cmd in [
        ('conflicting', r"git ls-files --unmerged | sed 's/.*\t//g' | sort -u"),
        ('dirty', 'git diff-index --name-only HEAD'),
        ('unknown', 'git ls-files --others --exclude-standard'),
        ('known', 'git ls-files'),
    ]:
        r.append(file_list_fun(name, cmd))
    return flatten(r, '')


def command_list(commands):
    return ['_stg_commands="%s"\n' % ' '.join(cmd for cmd, _, _, _ in commands)]


def command_fun(cmd, modname):
    mod = stgit.commands.get_command(modname)

    def cg(args, flags):
        return compjoin(
            [arg_to_compgen(arg) for arg in args] + [Compgen(flags)]
        ).command('$cur')

    return fun(
        '_stg_%s' % cmd,
        'local flags="%s"'
        % ' '.join(
            sorted(
                itertools.chain(
                    ('--help',),
                    (
                        flag
                        for opt in mod.options
                        for flag in opt.flags
                        if flag.startswith('--')
                    ),
                )
            )
        ),
        'local prev="${COMP_WORDS[COMP_CWORD-1]}"',
        'local cur="${COMP_WORDS[COMP_CWORD]}"',
        'case "$prev" in',
        [
            '%s) COMPREPLY=($(%s)) ;;' % ('|'.join(opt.flags), cg(opt.args, []))
            for opt in mod.options
            if opt.args
        ]
        + ['*) COMPREPLY=($(%s)) ;;' % cg(mod.args, ['$flags'])],
        'esac',
    )


def main_switch(commands):
    return fun(
        '_stg',
        'local c=1',
        'local command',
        '',
        'while test $c -lt $COMP_CWORD; do',
        ['if test $c == 1; then', ['command="${COMP_WORDS[c]}"'], 'fi', 'c=$((++c))'],
        'done',
        '',
        (
            '# Complete name of subcommand if the user has not finished'
            ' typing it yet.'
        ),
        'if test $c -eq $COMP_CWORD -a -z "$command"; then',
        [
            (
                'COMPREPLY=($(compgen -W "help version copyright $_stg_commands" '
                '-- "${COMP_WORDS[COMP_CWORD]}"))'
            ),
            'return',
        ],
        'fi',
        '',
        '# Complete arguments to subcommands.',
        'case "$command" in',
        [
            'help) ',
            [
                (
                    'COMPREPLY=($(compgen -W "$_stg_commands" --'
                    ' "${COMP_WORDS[COMP_CWORD]}"))'
                ),
                'return ;;',
            ],
            'version) return ;;',
            'copyright) return ;;',
        ],
        ['%s) _stg_%s ;;' % (cmd, cmd) for cmd, _, _, _ in commands],
        'esac',
    )


def install():
    return [
        'complete -o bashdefault -o default -F _stg stg 2>/dev/null \\',
        ['|| complete -o default -F _stg stg'],
    ]


def write_bash_completion(f):
    commands = stgit.commands.get_commands(allow_cached=False)
    r = [
        [
            """# -*- shell-script -*-
# bash completion script for StGit (automatically generated)
#
# To use these routines:
#
#    1. Copy this file to somewhere (e.g. ~/.stgit-completion.bash).
#
#    2. Add the following line to your .bashrc:
#         . ~/.stgit-completion.bash"""
        ]
    ]
    r += [util(), command_list(commands)]
    for cmd, modname, _, _ in commands:
        r.append(command_fun(cmd, modname))
    r += [main_switch(commands), install()]
    write(f, flatten(r, ''))


if __name__ == '__main__':
    import sys

    write_bash_completion(sys.stdout)
