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

import sys, os
from optparse import OptionParser, make_option

from stgit.utils import *
from stgit import stack, git
from stgit.version import version
from stgit.config import config
from stgit.commands.common import *

# The commands
import stgit.commands.add
import stgit.commands.applied
import stgit.commands.delete
import stgit.commands.diff
import stgit.commands.export
import stgit.commands.files
import stgit.commands.init
import stgit.commands.new
import stgit.commands.pop
import stgit.commands.push
import stgit.commands.refresh
import stgit.commands.resolved
import stgit.commands.rm
import stgit.commands.series
import stgit.commands.status
import stgit.commands.top
import stgit.commands.unapplied


#
# The commands map
#
commands = {
    'add':      stgit.commands.add,
    'applied':  stgit.commands.applied,
    'delete':   stgit.commands.delete,
    'diff':     stgit.commands.diff,
    'export':   stgit.commands.export,
    'files':    stgit.commands.files,
    'init':     stgit.commands.init,
    'new':      stgit.commands.new,
    'pop':      stgit.commands.pop,
    'push':     stgit.commands.push,
    'refresh':  stgit.commands.refresh,
    'resolved': stgit.commands.resolved,
    'rm':       stgit.commands.rm,
    'series':   stgit.commands.series,
    'status':   stgit.commands.status,
    'top':      stgit.commands.top,
    'unapplied':stgit.commands.unapplied,
    }

def print_help():
    print 'usage: %s <command> [options]' % os.path.basename(sys.argv[0])
    print
    print 'commands:'
    print '  help        print this message'

    cmds = commands.keys()
    cmds.sort()
    for cmd in cmds:
        print '  ' + cmd + ' ' * (12 - len(cmd)) + commands[cmd].help

#
# The main function (command dispatcher)
#
def main():
    """The main function
    """
    prog = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print >> sys.stderr, 'Unknown command'
        print >> sys.stderr, \
              '  Try "%s help" for a list of supported commands' % prog
        sys.exit(1)

    cmd = sys.argv[1]

    if cmd in ['-h', '--help', 'help']:
        print_help()
        sys.exit(0)
    if cmd in ['-v', '--version']:
        print '%s %s' % (prog, version)
        sys.exit(0)
    if not cmd in commands:
        print >> sys.stderr, 'Unknown command: %s' % cmd
        print >> sys.stderr, '  Try "%s help" for a list of supported commands' \
              % prog
        sys.exit(1)

    # re-build the command line arguments
    sys.argv[0] += ' %s' % cmd
    del(sys.argv[1])

    command = commands[cmd]
    parser = OptionParser(usage = command.usage,
                          option_list = command.options)
    options, args = parser.parse_args()
    try:
        # the lines below are a simple way to avoid an exception when
        # stgit is run outside an initialised tree
        stgit.commands.common.crt_series = stack.Series()
        setattr(command, 'crt_series', stgit.commands.common.crt_series)

        command.func(parser, options, args)
    except (IOError, CmdException, stack.StackException, git.GitException), \
               err:
        print >> sys.stderr, '%s %s: %s' % (prog, cmd, err)
        sys.exit(2)

    sys.exit(0)
