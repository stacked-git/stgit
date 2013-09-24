"""Basic quilt-like functionality
"""

__copyright__ = """
Copyright (C) 2005, Catalin Marinas <catalin.marinas@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

import sys, os, traceback

import stgit.commands
from stgit.out import *
from stgit import argparse, run, utils
from stgit.config import config

class CommandAlias(object):
    def __init__(self, name, command):
        self.__command = command
        self.__name__ = name
        self.usage = ['<arguments>']
        self.help = 'Alias for "%s <arguments>".' % self.__command
        self.options = []

    def func(self, args):
        cmd = self.__command.split() + args
        p = run.Run(*cmd)
        p.discard_exitcode().run()
        return p.exitcode

def is_cmd_alias(cmd):
    return isinstance(cmd, CommandAlias)

def append_alias_commands(cmd_list):
    for (name, command) in config.getstartswith('stgit.alias.'):
        name = utils.strip_prefix('stgit.alias.', name)
        cmd_list[name] = (CommandAlias(name, command),
                          'Alias commands', command)

#
# The commands map
#
class Commands(dict):
    """Commands class. It performs on-demand module loading
    """
    def canonical_cmd(self, key):
        """Return the canonical name for a possibly-shortenned
        command name.
        """
        candidates = [cmd for cmd in self.keys() if cmd.startswith(key)]

        if not candidates:
            out.error('Unknown command: %s' % key,
                      'Try "%s help" for a list of supported commands' % prog)
            sys.exit(utils.STGIT_GENERAL_ERROR)
        elif len(candidates) > 1:
            out.error('Ambiguous command: %s' % key,
                      'Candidates are: %s' % ', '.join(candidates))
            sys.exit(utils.STGIT_GENERAL_ERROR)

        return candidates[0]
        
    def __getitem__(self, key):
        cmd_mod = self.get(key) or self.get(self.canonical_cmd(key))
        if is_cmd_alias(cmd_mod):
            return cmd_mod
        else:
            return stgit.commands.get_command(cmd_mod)

cmd_list = stgit.commands.get_commands()
append_alias_commands(cmd_list)
commands = Commands((cmd, mod) for cmd, (mod, kind, help)
                    in cmd_list.iteritems())

def print_help():
    print 'usage: %s <command> [options]' % os.path.basename(sys.argv[0])
    print
    print 'Generic commands:'
    print '  help        print the detailed command usage'
    print '  version     display version information'
    print '  copyright   display copyright information'
    print
    stgit.commands.pretty_command_list(cmd_list, sys.stdout)

#
# The main function (command dispatcher)
#
def _main():
    """The main function
    """
    global prog

    prog = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print >> sys.stderr, 'usage: %s <command>' % prog
        print >> sys.stderr, \
              '  Try "%s --help" for a list of supported commands' % prog
        sys.exit(utils.STGIT_GENERAL_ERROR)

    cmd = sys.argv[1]

    if cmd in ['-h', '--help']:
        if len(sys.argv) >= 3:
            cmd = commands.canonical_cmd(sys.argv[2])
            sys.argv[2] = '--help'
        else:
            print_help()
            sys.exit(utils.STGIT_SUCCESS)
    if cmd == 'help':
        if len(sys.argv) == 3 and not sys.argv[2] in ['-h', '--help']:
            cmd = commands.canonical_cmd(sys.argv[2])
            if not cmd in commands:
                out.error('%s help: "%s" command unknown' % (prog, cmd))
                sys.exit(utils.STGIT_GENERAL_ERROR)

            sys.argv[0] += ' %s' % cmd
            command = commands[cmd]
            parser = argparse.make_option_parser(command)
            if is_cmd_alias(command):
                parser.remove_option('-h')
            from pydoc import pager
            pager(parser.format_help())
        else:
            print_help()
        sys.exit(utils.STGIT_SUCCESS)
    if cmd in ['-v', '--version', 'version']:
        from stgit.version import version
        print 'Stacked GIT %s' % version
        os.system('git --version')
        print 'Python version %s' % sys.version
        sys.exit(utils.STGIT_SUCCESS)
    if cmd in ['copyright']:
        print __copyright__
        sys.exit(utils.STGIT_SUCCESS)

    # re-build the command line arguments
    cmd = commands.canonical_cmd(cmd)
    sys.argv[0] += ' %s' % cmd
    del(sys.argv[1])

    command = commands[cmd]
    if is_cmd_alias(command):
        sys.exit(command.func(sys.argv[1:]))

    parser = argparse.make_option_parser(command)
    directory = command.directory

    # These modules are only used from this point onwards and do not
    # need to be imported earlier
    from stgit.exception import StgException
    from stgit.config import config_setup
    from ConfigParser import ParsingError, NoSectionError
    from stgit.stack import Series

    try:
        debug_level = int(os.environ.get('STGIT_DEBUG_LEVEL', 0))
    except ValueError:
        out.error('Invalid STGIT_DEBUG_LEVEL environment variable')
        sys.exit(utils.STGIT_GENERAL_ERROR)

    try:
        (options, args) = parser.parse_args()
        directory.setup()
        config_setup()

        # Some commands don't (always) need an initialized series.
        if directory.needs_current_series:
            if hasattr(options, 'branch') and options.branch:
                command.crt_series = Series(options.branch)
            else:
                command.crt_series = Series()

        ret = command.func(parser, options, args)
    except (StgException, IOError, ParsingError, NoSectionError), err:
        directory.write_log(cmd)
        if debug_level > 0:
            traceback.print_exc(file=sys.stderr)
        out.error(str(err), title = '%s %s' % (prog, cmd))
        sys.exit(utils.STGIT_COMMAND_ERROR)
    except SystemExit:
        # Triggered by the option parser when it finds bad commandline
        # parameters.
        sys.exit(utils.STGIT_COMMAND_ERROR)
    except KeyboardInterrupt:
        sys.exit(utils.STGIT_GENERAL_ERROR)
    except:
        out.error('Unhandled exception:')
        traceback.print_exc(file=sys.stderr)
        sys.exit(utils.STGIT_BUG_ERROR)

    directory.write_log(cmd)
    sys.exit(ret or utils.STGIT_SUCCESS)

def main():
    try:
        _main()
    finally:
        run.finish_logging()
