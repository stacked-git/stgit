"""This module provides a layer on top of the standard library's
C{optparse} module, so that we can easily generate both interactive
help and asciidoc documentation (such as man pages)."""

import optparse, sys, textwrap
from stgit import utils
from stgit.config import config
from stgit.lib import git

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

class opt(object):
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
            f.write(' '*8 + line + '\n')
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
            default = [files]
        return self.kwargs.get('args', default)

def _cmd_name(cmd_mod):
    return getattr(cmd_mod, 'name', cmd_mod.__name__.split('.')[-1])

def make_option_parser(cmd):
    pad = ' '*len('Usage: ')
    return optparse.OptionParser(
        prog = 'stg %s' % _cmd_name(cmd),
        usage = (('\n' + pad).join('%%prog %s' % u for u in cmd.usage) +
                 '\n\n' + cmd.help),
        option_list = [o.get_option() for o in cmd.options])

def _write_underlined(s, u, f):
    f.write(s + '\n')
    f.write(u*len(s) + '\n')

def write_asciidoc(cmd, f):
    _write_underlined('stg-%s(1)' % _cmd_name(cmd), '=', f)
    f.write('\n')
    _write_underlined('NAME', '-', f)
    f.write('stg-%s - %s\n\n' % (_cmd_name(cmd), cmd.help))
    _write_underlined('SYNOPSIS', '-', f)
    f.write('[verse]\n')
    for u in cmd.usage:
        f.write("'stg' %s %s\n" % (_cmd_name(cmd), u))
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
                '--ack and --sign were both specified')
        parser.values.sign_str = sign_str
    return [
        opt('--sign', action = 'callback', dest = 'sign_str', args = [],
            callback = callback, callback_args = ('Signed-off-by',),
            short = 'Add "Signed-off-by:" line', long = """
            Add a "Signed-off-by:" to the end of the patch."""),
        opt('--ack', action = 'callback', dest = 'sign_str', args = [],
            callback = callback, callback_args = ('Acked-by',),
            short = 'Add "Acked-by:" line', long = """
            Add an "Acked-by:" line to the end of the patch.""")]

def message_options(save_template):
    def no_dup(parser):
        if parser.values.message != None:
            raise optparse.OptionValueError(
                'Cannot give more than one --message or --file')
    def no_combine(parser):
        if (save_template and parser.values.message != None
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
    opts = [
        opt('-m', '--message', action = 'callback',
            callback = msg_callback, dest = 'message', type = 'string',
            short = 'Use MESSAGE instead of invoking the editor'),
        opt('-f', '--file', action = 'callback', callback = file_callback,
            dest = 'message', type = 'string', args = [files],
            metavar = 'FILE',
            short = 'Use FILE instead of invoking the editor', long = """
            Use the contents of FILE instead of invoking the editor.
            (If FILE is "-", write to stdout.)""")]
    if save_template:
        opts.append(
            opt('--save-template', action = 'callback', dest = 'save_template',
                callback = templ_callback, metavar = 'FILE', type = 'string',
                short = 'Save the message template to FILE and exit', long = """
                Instead of running the command, just write the message
                template to FILE, and exit. (If FILE is "-", write to
                stdout.)

                When driving StGit from another program, it is often
                useful to first call a command with '--save-template',
                then let the user edit the message, and then call the
                same command with '--file'."""))
    return opts

def diff_opts_option():
    def diff_opts_callback(option, opt_str, value, parser):
        if value:
            parser.values.diff_flags.extend(value.split())
        else:
            parser.values.diff_flags = []
    return [
        opt('-O', '--diff-opts', dest = 'diff_flags',
            default = (config.get('stgit.diff-opts') or '').split(),
            action = 'callback', callback = diff_opts_callback,
            type = 'string', metavar = 'OPTIONS',
            args = [strings('-M', '-C')],
            short = 'Extra options to pass to "git diff"')]

def _person_opts(person, short):
    """Sets options.<person> to a function that modifies a Person
    according to the commandline options."""
    def short_callback(option, opt_str, value, parser, field):
        f = getattr(parser.values, person)
        if field == "date":
            value = git.Date(value)
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
    return (
        [opt('--%s' % person, metavar = '"NAME <EMAIL>"', type = 'string',
             action = 'callback', callback = full_callback, dest = person,
             default = lambda p: p, short = 'Set the %s details' % person)] +
        [opt('--%s%s' % (short, f), metavar = f.upper(), type = 'string',
             action = 'callback', callback = short_callback, dest = person,
             callback_args = (f,), short = 'Set the %s %s' % (person, f))
         for f in ['name', 'email', 'date']])

def author_options():
    return _person_opts('author', 'auth')

def keep_option():
    return [opt('-k', '--keep', action = 'store_true',
                short = 'Keep the local changes',
                default = config.get('stgit.autokeep') == 'yes')]

def merged_option():
    return [opt('-m', '--merged', action = 'store_true',
                short = 'Check for patches merged upstream')]

class CompgenBase(object):
    def actions(self, var): return set()
    def words(self, var): return set()
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
    def words(self, var): return self.__a.words(var) | self.__b.words(var)
    def actions(self, var): return self.__a.actions(var) | self.__b.actions(var)

class Compgen(CompgenBase):
    def __init__(self, words = frozenset(), actions = frozenset()):
        self.__words = set(words)
        self.__actions = set(actions)
    def actions(self, var): return self.__actions
    def words(self, var): return self.__words

def compjoin(compgens):
    comp = Compgen()
    for c in compgens:
        comp = CompgenJoin(comp, c)
    return comp

all_branches = Compgen(['$(_all_branches)'])
stg_branches = Compgen(['$(_stg_branches)'])
applied_patches = Compgen(['$(_applied_patches)'])
other_applied_patches = Compgen(['$(_other_applied_patches)'])
unapplied_patches = Compgen(['$(_unapplied_patches)'])
hidden_patches = Compgen(['$(_hidden_patches)'])
commit = Compgen(['$(_all_branches) $(_tags) $(_remotes)'])
conflicting_files = Compgen(['$(_conflicting_files)'])
dirty_files = Compgen(['$(_dirty_files)'])
unknown_files = Compgen(['$(_unknown_files)'])
known_files = Compgen(['$(_known_files)'])
repo = Compgen(actions = ['directory'])
dir = Compgen(actions = ['directory'])
files = Compgen(actions = ['file'])
def strings(*ss): return Compgen(ss)
class patch_range(CompgenBase):
    def __init__(self, *endpoints):
        self.__endpoints = endpoints
    def words(self, var):
        words = set()
        for e in self.__endpoints:
            assert not e.actions(var)
            words |= e.words(var)
        return set(['$(_patch_range "%s" "%s")' % (' '.join(words), var)])
