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

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'pull the changes from the remote repository'
usage = """%prog [options] [<repository>] [<refspec>]

Pull the latest changes from the given repository (defaulting to
'origin'). This command works by popping all the patches from the
stack, pulling the changes in the parent repository, setting the base
of the stack to the latest parent HEAD and pushing the patches back
(unless '--nopush' is specified). The 'push' operation can fail if
there are conflicts. They need to be resolved and the patch pushed
again.

Check the 'git pull' documentation for the <repository> and <refspec>
format."""

options = [make_option('-n', '--nopush',
                       help = 'do not push the patches back after pulling',
                       action = 'store_true')]

def func(parser, options, args):
    """Pull the changes from a remote repository
    """
    if len(args) > 2:
        parser.error('incorrect number of arguments')

    repository = 'origin'
    refspec = None
    if len(args) >= 1:
        repository = args[0]
    if len(args) == 2:
        refspec = args[1]

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected. Pulls are not permitted'

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    # pop all patches
    applied = crt_series.get_applied()
    if len(applied) > 0:
        print 'Popping all patches...',
        sys.stdout.flush()
        crt_series.pop_patch(applied[0])
        print 'done'

    # pull the remote changes
    print 'Pulling from "%s"...' % repository
    git.pull(repository, refspec)
    print 'done'

    # push the patches back
    if options.nopush:
        applied = []
    for p in applied:
        print 'Pushing patch "%s"...' % p,
        sys.stdout.flush()
        crt_series.push_patch(p)
        if crt_series.empty_patch(p):
            print 'done (empty patch)'
        else:
            print 'done'

    print_crt_patch()
