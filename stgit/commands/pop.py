
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


help = 'pop one or more patches from the stack'
usage = """%prog [options] [<patch1>] [<patch2>] [<patch3>..<patch4>]

Pop the topmost patch or a range of patches from the stack. The
command fails if there are conflicts or local changes (and --keep was
not specified).

A series of pop and push operations are performed so that only the
patches passed on the command line are popped from the stack. Some of
the push operations may fail because of conflicts (push --undo would
revert the last push operation)."""

options = [make_option('-a', '--all',
                       help = 'pop all the applied patches',
                       action = 'store_true'),
           make_option('-n', '--number', type = 'int',
                       help = 'pop the specified number of patches'),
           make_option('-k', '--keep',
                       help = 'keep the local changes',
                       action = 'store_true')]


def func(parser, options, args):
    """Pop the topmost patch from the stack
    """
    check_conflicts()
    check_head_top_equal()

    if not options.keep:
        check_local_changes()

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'

    if options.all:
        patches = applied
    elif options.number:
        # reverse it twice to also work with negative or bigger than
        # the length numbers
        patches = applied[::-1][:options.number][::-1]
    elif len(args) == 0:
        patches = [applied[-1]]
    else:
        patches = parse_patches(args, applied, ordered = True)

    if not patches:
        raise CmdException, 'No patches to pop'

    # pop to the most distant popped patch
    topop = applied[applied.index(patches[0]):]
    # push those not in the popped range
    topush = [p for p in topop if p not in patches]

    if options.keep and topush:
        raise CmdException, 'Cannot pop arbitrary patches with --keep'

    topop.reverse()
    pop_patches(topop, options.keep)
    if topush:
        push_patches(topush)

    print_crt_patch()
