"""This module provides a layer on top of the standard library's
C{optparse} module, so that we can easily generate both interactive
help and asciidoc documentation (such as man pages)."""


import io
import optparse
import sys
import textwrap

from stgit import utils
from stgit.config import config
from stgit.lib.git import Date
from stgit.out import out


def _splitlist(lst, split_on):
    """Iterate over the sublists of lst that are separated by an element e
    such that split_on(e) is true."""
    current = []
    for e in lst:
        if split_on(e):
            yield current
            current = []
        else:
            current.append(e)
    yield current


def _paragraphs(s):
    """Split a string s into a list of paragraphs, each of which is a list
    of lines."""
    lines = [line.rstrip() for line in textwrap.dedent(s).strip().splitlines()]
    return [p for p in _splitlist(lines, lambda line: not line.strip()) if p]


class opt:
    """Represents a command-line flag."""

    def __init__(self, *pargs, **kwargs):
        self.pargs = pargs
        self.kwargs = kwargs

    def get_option(self):
        kwargs = dict(self.kwargs)
        kwargs['help'] = kwargs['short']
        for k in ['short', 'long', 'args']:
            kwargs.pop(k, None)
        return optparse.make_option(*self.pargs, **kwargs)

    def metavar(self):
        o = self.get_option()
        if not o.takes_value():
            return None
        if o.metavar:
            return o.metavar
        for flag in self.pargs:
            if flag.startswith('--'):
                return utils.strip_prefix('--', flag).upper()
        raise Exception('Cannot determine metavar')

    def write_asciidoc(self, f):
        for flag in self.pargs:
            f.write(flag)
            m = self.metavar()
            if m:
                f.write(' ' + m)
            f.write('::\n')
        paras = _paragraphs(self.kwargs.get('long', self.kwargs['short'] + '.'))
        for line in paras[0]:
            f.write(' ' * 8 + line + '\n')
        for para in paras[1:]:
            f.write('+\n')
            for line in para:
                f.write(line + '\n')

    @property
    def flags(self):
        return self.pargs

    @property
    def args(self):
        if self.kwargs.get('action', None) in ['store_true', 'store_false']:
            default = []
        else:
            default = ['files']
        return self.kwargs.get('args', default)


def _cmd_name(cmd_mod):
    return getattr(cmd_mod, 'name', cmd_mod.__name__.split('.')[-1])


def make_option_parser(cmd):
    pad = ' ' * len('Usage: ')
    return optparse.OptionParser(
        prog='stg %s' % _cmd_name(cmd),
        usage=(
            ('\n' + pad).join('%%prog %s' % u for u in cmd.usage) + '\n\n' + cmd.help
        ),
        option_list=[o.get_option() for o in cmd.options],
    )


def _write_underlined(s, u, f):
    f.write(s + '\n')
    f.write(u * len(s) + '\n')


def write_asciidoc(cmd, f):
    _write_underlined('stg-%s(1)' % _cmd_name(cmd), '=', f)
    f.write('\n')
    _write_underlined('NAME', '-', f)
    f.write('stg-%s - %s\n\n' % (_cmd_name(cmd), cmd.help))
    _write_underlined('SYNOPSIS', '-', f)
    f.write('[verse]\n')
    for u in cmd.usage:
        f.write("'stg %s' %s\n" % (_cmd_name(cmd), u))
    f.write('\n')
    _write_underlined('DESCRIPTION', '-', f)
    f.write('\n%s\n\n' % cmd.description.strip('\n'))
    if cmd.options:
        _write_underlined('OPTIONS', '-', f)
        for o in cmd.options:
            o.write_asciidoc(f)
            f.write('\n')
    _write_underlined('StGit', '-', f)
    f.write('Part of the StGit suite - see linkman:stg[1]\n')


def sign_options():
    def callback(option, opt_str, value, parser, sign_str):
        if parser.values.sign_str not in [None, sign_str]:
            raise optparse.OptionValueError(
                'Cannot give more than one of --ack, --sign, --review'
            )
        parser.values.sign_str = sign_str

    return [
        opt(
            '--sign',
            action='callback',
            dest='sign_str',
            args=[],
            callback=callback,
            callback_args=('Signed-off-by',),
            short='Add "Signed-off-by:" line',
            long='Add a "Signed-off-by:" to the end of the patch.',
        ),
        opt(
            '--ack',
            action='callback',
            dest='sign_str',
            args=[],
            callback=callback,
            callback_args=('Acked-by',),
            short='Add "Acked-by:" line',
            long='Add an "Acked-by:" line to the end of the patch.',
        ),
        opt(
            '--review',
            action='callback',
            dest='sign_str',
            args=[],
            callback=callback,
            callback_args=('Reviewed-by',),
            short='Add "Reviewed-by:" line',
            long='Add a "Reviewed-by:" line to the end of the patch.',
        ),
    ]


def hook_options():
    return [
        opt(
            '--no-verify',
            action='store_true',
            dest='no_verify',
            default=False,
            short='Disable commit-msg hook',
            long="This option bypasses the commit-msg hook.",
        )
    ]


def message_options(save_template):
    def no_dup(parser):
        if parser.values.message is not None:
            raise optparse.OptionValueError(
                'Cannot give more than one --message or --file'
            )

    def no_combine(parser):
        if (
            save_template
            and parser.values.message is not None
            and parser.values.save_template is not None
        ):
            raise optparse.OptionValueError(
                'Cannot give both --message/--file and --save-template'
            )

    def msg_callback(option, opt_str, value, parser):
        no_dup(parser)
        if value and not value.endswith('\n'):
            value += '\n'
        parser.values.message = value
        no_combine(parser)

    def file_callback(option, opt_str, value, parser):
        no_dup(parser)
        if value == '-':
            parser.values.message = sys.stdin.read()
        else:
            with open(value) as f:
                parser.values.message = f.read()
        no_combine(parser)

    def templ_callback(option, opt_str, value, parser):
        if value == '-':
            parser.values.save_template = out.stdout_bytes
        else:

            def write_file(s):
                with io.open(value, 'wb') as f:
                    f.write(s)

            parser.values.save_template = write_file
        no_combine(parser)

    opts = [
        opt(
            '-m',
            '--message',
            action='callback',
            callback=msg_callback,
            dest='message',
            type='string',
            short='Use MESSAGE instead of invoking the editor',
        ),
        opt(
            '-f',
            '--file',
            action='callback',
            callback=file_callback,
            dest='message',
            type='string',
            args=['files'],
            metavar='FILE',
            short='Use FILE instead of invoking the editor',
            long="""
            Use the contents of FILE instead of invoking the editor.
            (If FILE is "-", write to stdout.)""",
        ),
    ]
    if save_template:
        opts.append(
            opt(
                '--save-template',
                action='callback',
                dest='save_template',
                callback=templ_callback,
                metavar='FILE',
                type='string',
                short='Save the message template to FILE and exit',
                long="""
                Instead of running the command, just write the message
                template to FILE, and exit. (If FILE is "-", write to
                stdout.)

                When driving StGit from another program, it is often
                useful to first call a command with '--save-template',
                then let the user edit the message, and then call the
                same command with '--file'.""",
            )
        )
    return opts


def diff_opts_option():
    def diff_opts_callback(option, opt_str, value, parser):
        if value:
            parser.values.diff_flags.extend(value.split())
        else:
            parser.values.diff_flags = []

    return [
        opt(
            '-O',
            '--diff-opts',
            dest='diff_flags',
            default=(config.get('stgit.diff-opts') or '').split(),
            action='callback',
            callback=diff_opts_callback,
            type='string',
            metavar='OPTIONS',
            args=[strings('-M', '-C')],
            short='Extra options to pass to "git diff"',
        )
    ]


def author_options():
    """Sets options.author to a function that modifies a Person
    according to the commandline options."""

    def short_callback(option, opt_str, value, parser, field):
        f = parser.values.author
        if field == "date":
            value = Date(value)
        parser.values.author = lambda p: getattr(f(p), 'set_' + field)(value)

    def full_callback(option, opt_str, value, parser):
        ne = utils.parse_name_email(value)
        if not ne:
            raise optparse.OptionValueError(
                'Bad %s specification: %r' % (opt_str, value)
            )
        name, email = ne
        short_callback(option, opt_str, name, parser, 'name')
        short_callback(option, opt_str, email, parser, 'email')

    return [
        opt(
            '--author',
            metavar='"NAME <EMAIL>"',
            type='string',
            action='callback',
            callback=full_callback,
            dest='author',
            default=lambda p: p,
            short='Set the author details',
        )
    ] + [
        opt(
            '--auth%s' % field,
            metavar=field.upper(),
            type='string',
            action='callback',
            callback=short_callback,
            dest='author',
            callback_args=(field,),
            short='Set the author %s' % field,
        )
        for field in ['name', 'email', 'date']
    ]


def keep_option():
    return [
        opt(
            '-k',
            '--keep',
            action='store_true',
            short='Keep the local changes',
            default=config.get('stgit.autokeep') == 'yes',
        )
    ]


def merged_option():
    return [
        opt(
            '-m',
            '--merged',
            action='store_true',
            short='Check for patches merged upstream',
        )
    ]


class strings(list):
    def __init__(self, *args):
        super().__init__(args)


class patch_range(list):
    def __init__(self, *args):
        super().__init__(args)
