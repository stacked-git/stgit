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
from stgit import stack, git, gitmergeonefile
from stgit.version import version
from stgit.config import config
from stgit.commands.common import *

# The commands
import stgit.commands.add
import stgit.commands.applied
import stgit.commands.branch
import stgit.commands.delete
import stgit.commands.diff
import stgit.commands.clean
import stgit.commands.clone
import stgit.commands.commit
import stgit.commands.export
import stgit.commands.files
import stgit.commands.fold
import stgit.commands.goto
import stgit.commands.id
import stgit.commands.imprt
import stgit.commands.init
import stgit.commands.log
import stgit.commands.mail
import stgit.commands.new
import stgit.commands.patches
import stgit.commands.pick
import stgit.commands.pop
import stgit.commands.pull
import stgit.commands.push
import stgit.commands.refresh
import stgit.commands.rename
import stgit.commands.resolved
import stgit.commands.rm
import stgit.commands.series
import stgit.commands.show
import stgit.commands.status
import stgit.commands.top
import stgit.commands.unapplied
import stgit.commands.uncommit


#
# The commands map
#
commands = {
    'add':      stgit.commands.add,
    'applied':  stgit.commands.applied,
    'branch':   stgit.commands.branch,
    'delete':   stgit.commands.delete,
    'diff':     stgit.commands.diff,
    'clean':    stgit.commands.clean,
    'clone':    stgit.commands.clone,
    'commit':   stgit.commands.commit,
    'export':   stgit.commands.export,
    'files':    stgit.commands.files,
    'fold':     stgit.commands.fold,
    'goto':     stgit.commands.goto,
    'id':       stgit.commands.id,
    'import':   stgit.commands.imprt,
    'init':     stgit.commands.init,
    'log':      stgit.commands.log,
    'mail':     stgit.commands.mail,
    'new':      stgit.commands.new,
    'patches':  stgit.commands.patches,
    'pick':     stgit.commands.pick,
    'pop':      stgit.commands.pop,
    'pull':     stgit.commands.pull,
    'push':     stgit.commands.push,
    'refresh':  stgit.commands.refresh,
    'rename':   stgit.commands.rename,
    'resolved': stgit.commands.resolved,
    'rm':       stgit.commands.rm,
    'series':   stgit.commands.series,
    'show':     stgit.commands.show,
    'status':   stgit.commands.status,
    'top':      stgit.commands.top,
    'unapplied':stgit.commands.unapplied,
    'uncommit': stgit.commands.uncommit,
    }

# classification: repository, stack, patch, working copy
repocommands = (
    'branch',
    'clone',
    'id',
    'pull'
    )
stackcommands = (
    'applied',
    'clean',
    'commit',
    'goto',
    'init',
    'pop',
    'push',
    'series',
    'top',
    'unapplied',
    'uncommit'
    )
patchcommands = (
    'delete',
    'export',
    'files',
    'fold',
    'import',
    'log',
    'mail',
    'new',
    'pick',
    'refresh',
    'rename',
    'show'
    )
wccommands = (
    'add',
    'diff',
    'patches',
    'resolved',
    'rm',
    'status'
    )

def _print_helpstring(cmd):
    print '  ' + cmd + ' ' * (12 - len(cmd)) + commands[cmd].help
    
def print_help():
    print 'usage: %s <command> [options]' % os.path.basename(sys.argv[0])
    print
    print 'Generic commands:'
    print '  help        print the detailed command usage'
    print '  version     display version information'
    print '  copyright   display copyright information'
    # unclassified commands if any
    cmds = commands.keys()
    cmds.sort()
    for cmd in cmds:
        if not cmd in repocommands and not cmd in stackcommands \
               and not cmd in patchcommands and not cmd in wccommands:
            _print_helpstring(cmd)
    print

    print 'Repository commands:'
    for cmd in repocommands:
        _print_helpstring(cmd)
    print
    
    print 'Stack commands:'
    for cmd in stackcommands:
        _print_helpstring(cmd)
    print

    print 'Patch commands:'
    for cmd in patchcommands:
        _print_helpstring(cmd)
    print

    print 'Working-copy commands:'
    for cmd in wccommands:
        _print_helpstring(cmd)

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
              '  Try "%s --help" for a list of supported commands' % prog
        sys.exit(1)

    cmd = sys.argv[1]

    if cmd in ['-h', '--help']:
        if len(sys.argv) == 3 and sys.argv[2] in commands:
            cmd = sys.argv[2]
            sys.argv[2] = '--help'
        else:
            print_help()
            sys.exit(0)
    if cmd == 'help':
        if len(sys.argv) == 3 and not sys.argv[2] in ['-h', '--help']:
            cmd = sys.argv[2]
            if not cmd in commands:
                print >> sys.stderr, '%s help: "%s" command unknown' \
                      % (prog, cmd)
                sys.exit(1)

            sys.argv[0] += ' %s' % cmd
            command = commands[cmd]
            parser = OptionParser(usage = command.usage,
                                  option_list = command.options)
            parser.print_help()
        else:
            print 'usage: %s help <command>' % prog

        sys.exit(0)
    if cmd in ['-v', '--version', 'version']:
        print 'Stacked GIT %s' % version
        os.system('git --version')
        print 'Python version %s' % sys.version
        sys.exit(0)
    if cmd in ['copyright']:
        print __copyright__
        sys.exit(0)
    if not cmd in commands:
        print >> sys.stderr, 'Unknown command: %s' % cmd
        print >> sys.stderr, '  Try "%s help" for a list of supported ' \
              'commands' % prog
        sys.exit(1)

    # re-build the command line arguments
    sys.argv[0] += ' %s' % cmd
    del(sys.argv[1])

    command = commands[cmd]
    usage = command.usage.split('\n')[0].strip()
    parser = OptionParser(usage = usage, option_list = command.options)
    options, args = parser.parse_args()
    try:
        # 'clone' doesn't expect an already initialised GIT tree. A Series
        # object will be created after the GIT tree is cloned
        if cmd != 'clone':
            if hasattr(options, 'branch') and options.branch:
                command.crt_series = stack.Series(options.branch)
            else:
                command.crt_series = stack.Series()
            stgit.commands.common.crt_series = command.crt_series

        command.func(parser, options, args)
    except (IOError, CmdException, stack.StackException, git.GitException,
            gitmergeonefile.GitMergeException), err:
        print >> sys.stderr, '%s %s: %s' % (prog, cmd, err)
        sys.exit(2)
    except KeyboardInterrupt:
        sys.exit(1)

    sys.exit(0)
