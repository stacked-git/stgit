
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
from stgit import stack, git

help = 'Pop one or more patches from the stack'
kind = 'stack'
usage = ['[options] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
Pop the topmost patch or a range of patches from the stack. The
command fails if there are conflicts or local changes (and --keep was
not specified).

A series of pop and push operations are performed so that only the
patches passed on the command line are popped from the stack. Some of
the push operations may fail because of conflicts (push --undo would
revert the last push operation)."""

options = [
    opt('-a', '--all', action = 'store_true',
        short = 'Pop all the applied patches'),
    opt('-n', '--number', type = 'int',
        short = 'Pop the specified number of patches'),
    opt('-k', '--keep', action = 'store_true',
        short = 'Keep the local changes')]

directory = DirectoryGotoToplevel()

def func(parser, options, args):
    """Pop the topmost patch from the stack
    """
    check_conflicts()
    check_head_top_equal(crt_series)

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
    pop_patches(crt_series, topop, options.keep)
    if topush:
        push_patches(crt_series, topush)

    print_crt_patch(crt_series)
