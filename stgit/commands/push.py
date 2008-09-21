
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
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import stack, git

help = 'Push one or more patches onto the stack'
kind = 'stack'
usage = ['[options] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
Push one or more patches (defaulting to the first unapplied one) onto
the stack. The 'push' operation allows patch reordering by commuting
them with the three-way merge algorithm. If there are conflicts while
pushing a patch, those conflicts are written to the work tree, and the
command halts. Conflicts raised during the push operation have to be
fixed and the 'resolved' command run (alternatively, you may undo the
conflicting push with 'stg undo').

The command also notifies when the patch becomes empty (fully merged
upstream) or is modified (three-way merged) by the 'push' operation."""

options = [
    opt('-a', '--all', action = 'store_true',
        short = 'Push all the unapplied patches'),
    opt('-n', '--number', type = 'int',
        short = 'Push the specified number of patches'),
    opt('--reverse', action = 'store_true',
        short = 'Push the patches in reverse order'),
    opt('-m', '--merged', action = 'store_true',
        short = 'Check for patches merged upstream')]

directory = DirectoryGotoToplevel(log = True)

def func(parser, options, args):
    """Pushes the given patch or all onto the series
    """

    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

    unapplied = crt_series.get_unapplied()
    if not unapplied:
        raise CmdException, 'No more patches to push'

    if options.number:
        patches = unapplied[:options.number]
    elif options.all:
        patches = unapplied
    elif len(args) == 0:
        patches = [unapplied[0]]
    else:
        patches = parse_patches(args, unapplied)

    if patches == []:
        raise CmdException, 'No patches to push'

    if options.reverse:
        patches.reverse()

    push_patches(crt_series, patches, options.merged)

    print_crt_patch(crt_series)
