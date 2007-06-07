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
from optparse import OptionParser

import stgit.commands
from stgit.utils import out

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
            sys.exit(1)
        elif len(candidates) > 1:
            out.error('Ambiguous command: %s' % key,
                      'Candidates are: %s' % ', '.join(candidates))
            sys.exit(1)

        return candidates[0]
        
    def __getitem__(self, key):
        """Return the command python module name based.
        """
        global prog

        cmd_mod = self.get(key) or self.get(self.canonical_cmd(key))
            
        __import__('stgit.commands.' + cmd_mod)
        return getattr(stgit.commands, cmd_mod)

commands = Commands({
    'add':              'add',
    'applied':          'applied',
    'assimilate':       'assimilate',
    'branch':           'branch',
    'delete':           'delete',
    'diff':             'diff',
    'clean':            'clean',
    'clone':            'clone',
    'commit':           'commit',
    'cp':		'copy',
    'export':           'export',
    'files':            'files',
    'float':            'float',
    'fold':             'fold',
    'goto':             'goto',
    'hide':             'hide',
    'id':               'id',
    'import':           'imprt',
    'init':             'init',
    'log':              'log',
    'mail':             'mail',
    'new':              'new',
    'patches':          'patches',
    'pick':             'pick',
    'pop':              'pop',
    'pull':             'pull',
    'push':             'push',
    'rebase':           'rebase',
    'refresh':          'refresh',
    'rename':           'rename',
    'resolved':         'resolved',
    'rm':               'rm',
    'series':           'series',
    'show':             'show',
    'sink':             'sink',
    'status':           'status',
    'sync':             'sync',
    'top':              'top',
    'unapplied':        'unapplied',
    'uncommit':         'uncommit',
    'unhide':           'unhide'
    })

# classification: repository, stack, patch, working copy
repocommands = (
    'clone',
    'id',
    )
stackcommands = (
    'applied',
    'assimilate',
    'branch',
    'clean',
    'commit',
    'float',
    'goto',
    'hide',
    'init',
    'patches',
    'pop',
    'pull',
    'push',
    'rebase',
    'series',
    'sink',
    'top',
    'unapplied',
    'uncommit',
    'unhide',
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
    'show',
    'sync',
    )
wccommands = (
    'add',
    'cp',
    'diff',
    'resolved',
    'rm',
    'status',
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
    global prog

    prog = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print >> sys.stderr, 'usage: %s <command>' % prog
        print >> sys.stderr, \
              '  Try "%s --help" for a list of supported commands' % prog
        sys.exit(1)

    cmd = sys.argv[1]

    if cmd in ['-h', '--help']:
        if len(sys.argv) >= 3:
            cmd = commands.canonical_cmd(sys.argv[2])
            sys.argv[2] = '--help'
        else:
            print_help()
            sys.exit(0)
    if cmd == 'help':
        if len(sys.argv) == 3 and not sys.argv[2] in ['-h', '--help']:
            cmd = commands.canonical_cmd(sys.argv[2])
            if not cmd in commands:
                out.error('%s help: "%s" command unknown' % (prog, cmd))
                sys.exit(1)

            sys.argv[0] += ' %s' % cmd
            command = commands[cmd]
            parser = OptionParser(usage = command.usage,
                                  option_list = command.options)
            from pydoc import pager
            pager(parser.format_help())
        else:
            print_help()
        sys.exit(0)
    if cmd in ['-v', '--version', 'version']:
        from stgit.version import version
        print 'Stacked GIT %s' % version
        os.system('git --version')
        print 'Python version %s' % sys.version
        sys.exit(0)
    if cmd in ['copyright']:
        print __copyright__
        sys.exit(0)

    # re-build the command line arguments
    cmd = commands.canonical_cmd(cmd)
    sys.argv[0] += ' %s' % cmd
    del(sys.argv[1])

    command = commands[cmd]
    usage = command.usage.split('\n')[0].strip()
    parser = OptionParser(usage = usage, option_list = command.options)
    options, args = parser.parse_args()

    # These modules are only used from this point onwards and do not
    # need to be imported earlier
    from stgit.config import config_setup
    from ConfigParser import ParsingError, NoSectionError
    from stgit.stack import Series, StackException
    from stgit.git import GitException
    from stgit.commands.common import CmdException
    from stgit.gitmergeonefile import GitMergeException
    from stgit.utils import EditorException

    try:
        debug_level = int(os.environ['STGIT_DEBUG_LEVEL'])
    except KeyError:
        debug_level = 0
    except ValueError:
        out.error('Invalid STGIT_DEBUG_LEVEL environment variable')
        sys.exit(1)

    try:
        config_setup()

        # 'clone' doesn't expect an already initialised GIT tree. A Series
        # object will be created after the GIT tree is cloned
        if cmd != 'clone':
            if hasattr(options, 'branch') and options.branch:
                command.crt_series = Series(options.branch)
            else:
                command.crt_series = Series()
            stgit.commands.common.crt_series = command.crt_series

        command.func(parser, options, args)
    except (IOError, ParsingError, NoSectionError, CmdException,
            StackException, GitException, GitMergeException,
            EditorException), err:
        print >> sys.stderr, '%s %s: %s' % (prog, cmd, err)
        if debug_level:
            raise
        else:
            sys.exit(2)
    except KeyboardInterrupt:
        sys.exit(1)

    sys.exit(0)
