"""Basic quilt-like functionality"""

import os
import sys
import traceback

import stgit.commands
from stgit import argparse, get_version, run, utils
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


class Commands(dict):
    """Commands class. It performs on-demand module loading"""

    def canonical_cmd(self, key):
        """Return the canonical name for a possibly-shortened command name."""
        candidates = [cmd for cmd in self if cmd.startswith(key)]

        if not candidates:
            out.error(
                'Unknown command: %s' % key,
                'Try "stg help" for a list of supported commands',
            )
            raise KeyError(key)
        elif len(candidates) == 1:
            return candidates[0]
        elif key in candidates:
            return key
        else:
            out.error(
                'Ambiguous command: %s' % key,
                'Candidates are: %s' % ', '.join(candidates),
            )
            raise KeyError(key)

    def __getitem__(self, key):
        cmd_mod = self.get(key) or self.get(self.canonical_cmd(key))
        if is_cmd_alias(cmd_mod):
            return cmd_mod
        else:
            return stgit.commands.get_command(cmd_mod)


cmd_list = stgit.commands.get_commands()
append_alias_commands(cmd_list)
commands = Commands((cmd, mod) for cmd, mod, _, _ in cmd_list)


def print_help(prog):
    print('usage: %s <command> [options]' % prog)
    print()
    print('Generic commands:')
    print('  help        print the detailed command usage')
    print('  version     display version information')
    print('  copyright   display copyright information')
    print()
    stgit.commands.pretty_command_list(cmd_list, sys.stdout)


def _main(argv):
    prog = argv[0] = 'stg'

    if len(argv) < 2:
        print('usage: %s <command>' % prog, file=sys.stderr)
        print(
            '  Try "%s --help" for a list of supported commands' % prog, file=sys.stderr
        )
        return utils.STGIT_GENERAL_ERROR

    cmd = argv[1]

    if cmd in ['-h', '--help']:
        if len(argv) >= 3:
            try:
                cmd = commands.canonical_cmd(argv[2])
            except KeyError:
                return utils.STGIT_GENERAL_ERROR

            argv[2] = '--help'
        else:
            print_help(prog)
            return utils.STGIT_SUCCESS
    if cmd == 'help':
        if len(argv) == 3 and not argv[2] in ['-h', '--help']:
            try:
                cmd = commands.canonical_cmd(argv[2])
            except KeyError:
                return utils.STGIT_GENERAL_ERROR
            argv[0] += ' %s' % cmd
            command = commands[cmd]
            parser = argparse.make_option_parser(command)
            if is_cmd_alias(command):
                parser.remove_option('-h')
            pager(parser.format_help().encode())
        else:
            print_help(prog)
        return utils.STGIT_SUCCESS
    if cmd in ['-v', '--version', 'version']:
        print('Stacked Git %s' % get_version())
        os.system('git --version')
        os.system('python --version')
        return utils.STGIT_SUCCESS
    if cmd in ['copyright']:
        print(__copyright__)
        return utils.STGIT_SUCCESS

    # re-build the command line arguments
    try:
        cmd = commands.canonical_cmd(cmd)
    except KeyError:
        return utils.STGIT_GENERAL_ERROR
    argv[0] += ' %s' % cmd
    del argv[1]

    command = commands[cmd]
    if is_cmd_alias(command):
        return command.func(argv[1:])

    parser = argparse.make_option_parser(command)

    # These modules are only used from this point onwards and do not
    # need to be imported earlier
    from configparser import NoSectionError, ParsingError

    from stgit.config import config_setup
    from stgit.exception import StgException
    from stgit.lib.git import MergeConflictException

    try:
        debug_level = int(environ_get('STGIT_DEBUG_LEVEL', 0))
    except ValueError:
        out.error('Invalid STGIT_DEBUG_LEVEL environment variable')
        return utils.STGIT_GENERAL_ERROR

    try:
        (options, args) = parser.parse_args(argv[1:])
        command.directory.setup()
        config_setup()
        return command.func(parser, options, args)
    except MergeConflictException as err:
        if debug_level > 1:
            traceback.print_exc(file=sys.stderr)
        for conflict in err.conflicts:
            out.err(conflict)
        return utils.STGIT_CONFLICT
    except (StgException, IOError, ParsingError, NoSectionError) as err:
        if debug_level > 0:
            traceback.print_exc(file=sys.stderr)
        out.error(str(err), title='%s %s' % (prog, cmd))
        return utils.STGIT_COMMAND_ERROR
    except SystemExit:
        # Triggered by the option parser when it finds bad commandline
        # parameters.
        return utils.STGIT_COMMAND_ERROR
    except KeyboardInterrupt:
        return utils.STGIT_GENERAL_ERROR
    except BaseException:
        out.error('Unhandled exception:')
        traceback.print_exc(file=sys.stderr)
        return utils.STGIT_BUG_ERROR


def main(argv=None):
    if argv is None:
        argv = list(map(fsdecode_utf8, sys.argv))
    try:
        return _main(argv)
    finally:
        run.finish_logging()
