"""Utility functions for command-line option parsing."""

import optparse, sys
from stgit import utils
from stgit.config import config

def sign_options():
    def callback(option, opt_str, value, parser, sign_str):
        if parser.values.sign_str not in [None, sign_str]:
            raise optparse.OptionValueError(
                '--ack and --sign were both specified')
        parser.values.sign_str = sign_str
    return [optparse.make_option('--sign', action = 'callback',
                                 callback = callback, dest = 'sign_str',
                                 callback_args = ('Signed-off-by',),
                                 help = 'add Signed-off-by line'),
            optparse.make_option('--ack', action = 'callback',
                                 callback = callback, dest = 'sign_str',
                                 callback_args = ('Acked-by',),
                                 help = 'add Acked-by line')]

def message_options():
    def no_dup(parser):
        if parser.values.message != None:
            raise optparse.OptionValueError(
                'Cannot give more than one --message or --file')
    def no_combine(parser):
        if (parser.values.message != None
            and parser.values.save_template != None):
            raise optparse.OptionValueError(
                'Cannot give both --message/--file and --save-template')
    def msg_callback(option, opt_str, value, parser):
        no_dup(parser)
        parser.values.message = value
        no_combine(parser)
    def file_callback(option, opt_str, value, parser):
        no_dup(parser)
        if value == '-':
            parser.values.message = sys.stdin.read()
        else:
            f = file(value)
            parser.values.message = f.read()
            f.close()
        no_combine(parser)
    def templ_callback(option, opt_str, value, parser):
        if value == '-':
            def w(s):
                sys.stdout.write(s)
        else:
            def w(s):
                f = file(value, 'w+')
                f.write(s)
                f.close()
        parser.values.save_template = w
        no_combine(parser)
    m = optparse.make_option
    return [m('-m', '--message', action = 'callback', callback = msg_callback,
              dest = 'message', type = 'string',
              help = 'use MESSAGE instead of invoking the editor'),
            m('-f', '--file', action = 'callback', callback = file_callback,
              dest = 'message', type = 'string', metavar = 'FILE',
              help = 'use FILE instead of invoking the editor'),
            m('--save-template', action = 'callback', callback = templ_callback,
              metavar = 'FILE', dest = 'save_template', type = 'string',
              help = 'save the message template to FILE and exit')]

def diff_opts_option():
    def diff_opts_callback(option, opt_str, value, parser):
        if value:
            parser.values.diff_flags.extend(value.split())
        else:
            parser.values.diff_flags = []
    return [optparse.make_option(
        '-O', '--diff-opts', dest = 'diff_flags',
        default = (config.get('stgit.diff-opts') or '').split(),
        action = 'callback', callback = diff_opts_callback,
        type = 'string', metavar = 'OPTIONS',
        help = 'extra options to pass to "git diff"')]

def person_opts(person, short):
    """Sets options.<person> to a function that modifies a Person
    according to the commandline options."""
    def short_callback(option, opt_str, value, parser, field):
        f = getattr(parser.values, person)
        setattr(parser.values, person,
                lambda p: getattr(f(p), 'set_' + field)(value))
    def full_callback(option, opt_str, value, parser):
        ne = utils.parse_name_email(value)
        if not ne:
            raise optparse.OptionValueError(
                'Bad %s specification: %r' % (opt_str, value))
        name, email = ne
        short_callback(option, opt_str, name, parser, 'name')
        short_callback(option, opt_str, email, parser, 'email')
    return ([optparse.make_option(
                '--%s' % person, metavar = '"NAME <EMAIL>"', type = 'string',
                action = 'callback', callback = full_callback, dest = person,
                default = lambda p: p, help = 'set the %s details' % person)]
            + [optparse.make_option(
                '--%s%s' % (short, f), metavar = f.upper(), type = 'string',
                action = 'callback', callback = short_callback, dest = person,
                callback_args = (f,), help = 'set the %s %s' % (person, f))
               for f in ['name', 'email', 'date']])

def author_committer_options():
    return person_opts('author', 'auth') + person_opts('committer', 'comm')
