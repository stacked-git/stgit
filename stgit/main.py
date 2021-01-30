"""Basic quilt-like functionality"""

import os
import sys
import traceback

import stgit.commands
from stgit import argparse, run, utils
from stgit.compat import environ_get, fsdecode_utf8
from stgit.config import config
from stgit.out import out
from stgit.pager import pager

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""


class CommandAlias:
    def __init__(self, name, command):
        self._command = command
        self.__name__ = name
        self.usage = ['<arguments>']
        self.help = 'Alias for "%s <arguments>".' % self._command
        self.options = []

    def func(self, args):
        cmd = self._command.split() + args
        p = run.Run(*cmd)
        p.discard_exitcode().run()
        return p.exitcode


def is_cmd_alias(cmd):
    return isinstance(cmd, CommandAlias)


def append_alias_commands(cmd_list):
    for (name, command) in config.getstartswith('stgit.alias.'):
        name = utils.strip_prefix('stgit.alias.', name)
        cmd_list.append((name, CommandAlias(name, command), 'Alias commands', command))


#
# The commands map
#
class Commands(dict):
    """Commands class. It performs on-demand module loading"""

    def canonical_cmd(self, key):
        """Return the canonical name for a possibly-shortened command name."""
        candidates = [cmd for cmd in self if cmd.startswith(key)]

        if not candidates:
            out.error(
                'Unknown command: %s' % key,
                'Try "%s help" for a list of supported commands' % prog,
            )
            sys.exit(utils.STGIT_GENERAL_ERROR)
        elif len(candidates) > 1:
            out.error(
                'Ambiguous command: %s' % key,
                'Candidates are: %s' % ', '.join(candidates),
            )
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
commands = Commands((cmd, mod) for cmd, mod, _, _ in cmd_list)


def print_help():
    print('usage: %s <command> [options]' % os.path.basename(sys.argv[0]))
    print()
    print('Generic commands:')
    print('  help        print the detailed command usage')
    print('  version     display version information')
    print('  copyright   display copyright information')
    print()
    stgit.commands.pretty_command_list(cmd_list, sys.stdout)


def _main():
    global prog

    sys.argv = list(map(fsdecode_utf8, sys.argv))

    prog = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print('usage: %s <command>' % prog, file=sys.stderr)
        print(
            '  Try "%s --help" for a list of supported commands' % prog, file=sys.stderr
        )
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
            sys.argv[0] += ' %s' % cmd
            command = commands[cmd]
            parser = argparse.make_option_parser(command)
            if is_cmd_alias(command):
                parser.remove_option('-h')
            pager(parser.format_help().encode())
        else:
            print_help()
        sys.exit(utils.STGIT_SUCCESS)
    if cmd in ['-v', '--version', 'version']:
        from stgit.version import get_version

        print('Stacked GIT %s' % get_version())
        os.system('git --version')
        print('Python version %s' % sys.version)
        sys.exit(utils.STGIT_SUCCESS)
    if cmd in ['copyright']:
        print(__copyright__)
        sys.exit(utils.STGIT_SUCCESS)

    # re-build the command line arguments
    cmd = commands.canonical_cmd(cmd)
    sys.argv[0] += ' %s' % cmd
    del sys.argv[1]

    command = commands[cmd]
    if is_cmd_alias(command):
        sys.exit(command.func(sys.argv[1:]))

    parser = argparse.make_option_parser(command)

    # These modules are only used from this point onwards and do not
    # need to be imported earlier
    try:
        from configparser import NoSectionError, ParsingError
    except ImportError:
        from ConfigParser import NoSectionError, ParsingError
    from stgit.config import config_setup
    from stgit.exception import StgException
    from stgit.lib.git import MergeConflictException

    try:
        debug_level = int(environ_get('STGIT_DEBUG_LEVEL', 0))
    except ValueError:
        out.error('Invalid STGIT_DEBUG_LEVEL environment variable')
        sys.exit(utils.STGIT_GENERAL_ERROR)

    try:
        (options, args) = parser.parse_args()
        command.directory.setup()
        config_setup()
        ret = command.func(parser, options, args)
    except MergeConflictException as err:
        if debug_level > 1:
            traceback.print_exc(file=sys.stderr)
        for conflict in err.conflicts:
            out.err(conflict)
        sys.exit(utils.STGIT_CONFLICT)
    except (StgException, IOError, ParsingError, NoSectionError) as err:
        if debug_level > 0:
            traceback.print_exc(file=sys.stderr)
        out.error(str(err), title='%s %s' % (prog, cmd))
        sys.exit(utils.STGIT_COMMAND_ERROR)
    except SystemExit:
        # Triggered by the option parser when it finds bad commandline
        # parameters.
        sys.exit(utils.STGIT_COMMAND_ERROR)
    except KeyboardInterrupt:
        sys.exit(utils.STGIT_GENERAL_ERROR)
    except BaseException:
        out.error('Unhandled exception:')
        traceback.print_exc(file=sys.stderr)
        sys.exit(utils.STGIT_BUG_ERROR)

    sys.exit(ret or utils.STGIT_SUCCESS)


def main():
    try:
        _main()
    finally:
        run.finish_logging()
