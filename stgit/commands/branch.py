"""Branch command
"""

__copyright__ = """
Copyright (C) 2005, Chuck Lever <cel@netapp.com>

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

import sys, os, time
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git, basedir


help = 'manage development branches'
usage = """%prog [options] branch-name [commit-id]

Create, clone, switch between, rename, or delete development branches
within a git repository.  By default, a single branch called 'master'
is always created in a new repository.  This subcommand allows you to
manage several patch series in the same repository via GIT branches.

When displaying the branches, the names can be prefixed with
's' (StGIT managed) or 'p' (protected).

If not given any options, switch to the named branch."""

options = [make_option('-c', '--create',
                       help = 'create a new development branch',
                       action = 'store_true'),
           make_option('--clone',
                       help = 'clone the contents of the current branch',
                       action = 'store_true'),
           make_option('--convert',
                       help = 'switch between old and new format branches',
                       action = 'store_true'),
           make_option('--delete',
                       help = 'delete an existing development branch',
                       action = 'store_true'),
           make_option('--force',
                       help = 'force a delete when the series is not empty',
                       action = 'store_true'),
           make_option('-l', '--list',
                       help = 'list branches contained in this repository',
                       action = 'store_true'),
           make_option('-p', '--protect',
                       help = 'prevent "stg pull" from modifying this branch',
                       action = 'store_true'),
           make_option('-r', '--rename',
                       help = 'rename an existing development branch',
                       action = 'store_true'),
           make_option('-u', '--unprotect',
                       help = 'allow "stg pull" to modify this branch',
                       action = 'store_true')]


def __is_current_branch(branch_name):
    return crt_series.get_branch() == branch_name

def __print_branch(branch_name, length):
    initialized = ' '
    current = ' '
    protected = ' '

    branch = stack.Series(branch_name)

    if branch.is_initialised():
        initialized = 's'
    if __is_current_branch(branch_name):
        current = '>'
    if branch.get_protected():
        protected = 'p'
    print current + ' ' + initialized + protected + '\t' + \
          branch_name.ljust(length) + '  | ' + branch.get_description()

def __delete_branch(doomed_name, force = False):
    doomed = stack.Series(doomed_name)

    if doomed.get_protected():
        raise CmdException, 'This branch is protected. Delete is not permitted'

    print 'Deleting branch "%s"...' % doomed_name,
    sys.stdout.flush()

    if __is_current_branch(doomed_name):
        check_local_changes()
        check_conflicts()
        check_head_top_equal()

        if doomed_name != 'master':
            git.switch_branch('master')

    doomed.delete(force)

    if doomed_name != 'master':
        git.delete_branch(doomed_name)

    print 'done'

def func(parser, options, args):

    if options.create:

        if len(args) == 0 or len(args) > 2:
            parser.error('incorrect number of arguments')

        check_local_changes()
        check_conflicts()
        check_head_top_equal()

        tree_id = None
        if len(args) == 2:
            tree_id = git_id(args[1])

        git.create_branch(args[0], tree_id)
        stack.Series(args[0]).init()

        print 'Branch "%s" created.' % args[0]
        return

    elif options.clone:

        if len(args) == 0:
            clone = crt_series.get_branch() + \
                    time.strftime('-%C%y%m%d-%H%M%S')
        elif len(args) == 1:
            clone = args[0]
        else:
            parser.error('incorrect number of arguments')

        check_local_changes()
        check_conflicts()
        check_head_top_equal()

        print 'Cloning current branch to "%s"...' % clone,
        sys.stdout.flush()
        crt_series.clone(clone)
        print 'done'

        return

    elif options.convert:

        if len(args) != 0:
            parser.error('incorrect number of arguments')

        crt_series.convert()
        return

    elif options.delete:

        if len(args) != 1:
            parser.error('incorrect number of arguments')
        __delete_branch(args[0], options.force)
        return

    elif options.list:

        if len(args) != 0:
            parser.error('incorrect number of arguments')

        branches = os.listdir(os.path.join(basedir.get(), 'refs', 'heads'))
        branches.sort()
        max_len = max([len(i) for i in branches])

        print 'Available branches:'
        for i in branches:
            __print_branch(i, max_len)
        return

    elif options.protect:

        if len(args) == 0:
            branch_name = crt_series.get_branch()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGIT' \
                  % branch_name

        print 'Protecting branch "%s"...' % branch_name,
        sys.stdout.flush()
        branch.protect()
        print 'done'

        return

    elif options.rename:

        if len(args) != 2:
            parser.error('incorrect number of arguments')

        if __is_current_branch(args[0]):
            raise CmdException, 'Renaming the current branch is not supported'

        stack.Series(args[0]).rename(args[1])

        print 'Renamed branch "%s" as "%s".' % (args[0], args[1])

        return

    elif options.unprotect:

        if len(args) == 0:
            branch_name = crt_series.get_branch()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGIT' \
                  % branch_name

        print 'Unprotecting branch "%s"...' % branch_name,
        sys.stdout.flush()
        branch.unprotect()
        print 'done'

        return

    elif len(args) == 1:

        if __is_current_branch(args[0]):
            raise CmdException, 'Branch "%s" is already the current branch' \
                  % args[0]

        check_local_changes()
        check_conflicts()
        check_head_top_equal()

        print 'Switching to branch "%s"...' % args[0],
        sys.stdout.flush()

        git.switch_branch(args[0])

        print 'done'
        return

    # default action: print the current branch
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    print crt_series.get_branch()
