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

import sys, os
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'manage development branches'
usage = """%prog [options] branch-name [commit-id]

Create, list, switch between, rename, or delete development branches
within a git repository.  By default, a single branch called 'master'
is always created in a new repository.  This subcommand allows you to
manage several patch series in the same repository.

When displaying the branches, the names can be prefixed with
's' (StGIT managed) or 'p' (protected)."""

options = [make_option('-c', '--create',
                       help = 'create a new development branch',
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


def is_current_branch(branch_name):
    return git.get_head_file() == branch_name

def print_branch(branch_name):
    initialized = ' '
    current = ' '
    protected = ' '

    branch = stack.Series(branch_name)

    if branch.is_initialised():
        initialized = 's'
    if is_current_branch(branch_name):
        current = '>'
    if branch.get_protected():
        protected = 'p'
    print '%s %s%s\t%s\t%s' % (current, initialized, protected, branch_name, \
                               branch.get_description())

def delete_branch(doomed_name, force = False):
    doomed = stack.Series(doomed_name)

    if doomed.get_protected():
        raise CmdException, 'This branch is protected. Delete is not permitted'

    if is_current_branch(doomed_name) and doomed_name != 'master':
        git.switch_branch('master')

    doomed.delete(force)

    if doomed_name != 'master':
        git.delete_branch(doomed_name)

    print 'Branch "%s" has been deleted.' % doomed_name

def func(parser, options, args):

    if options.create:

        if len(args) == 0 or len(args) > 2:
            parser.error('incorrect number of arguments')
        tree_id = None
        if len(args) == 2:
            tree_id = args[1]

        git.create_branch(args[0], tree_id)
        stack.Series(args[0]).init()

        print 'Branch "%s" created.' % args[0]
        return

    elif options.delete:

        if len(args) != 1:
            parser.error('incorrect number of arguments')
        delete_branch(args[0], options.force)
        return

    elif options.list:

        if len(args) != 0:
            parser.error('incorrect number of arguments')

        branches = os.listdir(os.path.join(git.base_dir, 'refs', 'heads'))
        branches.sort()

        print 'Available branches:'
        for i in branches:
            print_branch(i)
        return

    elif options.protect:

        if len(args) == 0:
            branch_name = git.get_head_file()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGit' % branch_name

        print 'Protecting branch "%s"...' % branch_name,
        sys.stdout.flush()
        branch.protect()
        print 'done'

        return

    elif options.rename:

        if len(args) != 2:
            parser.error('incorrect number of arguments')

        stack.Series(args[0]).rename(args[1])

        print 'Renamed branch "%s" as "%s".' % (args[0], args[1])

        return

    elif options.unprotect:

        if len(args) == 0:
            branch_name = git.get_head_file()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGit' % branch_name

        print 'Unprotecting branch "%s"...' % branch_name,
        sys.stdout.flush()
        branch.unprotect()
        print 'done'

        return

    elif len(args) == 1:

        if args[0] == git.get_head_file():
            raise CmdException, 'Branch "%s" is already the current branch' % args[0]

        print 'Switching to branch "%s"...' % args[0],
        sys.stdout.flush()

        git.switch_branch(args[0])

        print 'done'
        return

    # default action: print the current branch
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    print git.get_head_file()
