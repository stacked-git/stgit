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


help = 'move the stack base to another point in history'
usage = """%prog [options] <new-base-id>

Pop all patches from current stack, move the stack base to the given
<new-base-id> and push the patches back."""

options = [make_option('-n', '--nopush',
                       help = 'do not push the patches back after rebasing',
                       action = 'store_true'),
           make_option('-m', '--merged',
                       help = 'check for patches merged upstream',
                       action = 'store_true')]

def func(parser, options, args):
    """Rebase the current stack
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected. Rebase is not permitted'

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    # pop all patches
    applied = crt_series.get_applied()
    if len(applied) > 0:
        print 'Popping all applied patches...',
        sys.stdout.flush()
        crt_series.pop_patch(applied[0])
        print 'done'

    print 'Rebasing to "%s"...' % args[0]
    git.reset(tree_id = git_id(args[0]))

    # push the patches back
    if not options.nopush:
        push_patches(applied, options.merged)

    print_crt_patch()
