__copyright__ = """
Copyright (C) 2006, Robin Rosenberg <robin.rosenberg@dewire.com>
Modified by Catalin Marinas

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

help = 'push patches to the top, even if applied'
usage = """%prog [<patches> | -s [<series>] ]

Push a patch or a range of patches to the top even if applied. The
necessary pop and push operations will be performed to accomplish
this. The '--series' option can be used to rearrange the (top) patches
as specified by the given series file (or the standard input)."""

options = [make_option('-s', '--series',
                       help = 'rearrange according to a series file',
                       action = 'store_true')]

def func(parser, options, args):
    """Pops and pushed to make the named patch the topmost patch
    """
    args_nr = len(args)
    if (options.series and args_nr > 1) \
           or (not options.series and args_nr == 0):
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    unapplied = crt_series.get_unapplied()
    applied = crt_series.get_applied()
    all = unapplied + applied

    if options.series:
        if args_nr:
            f = file(args[0])
        else:
            f = sys.stdin

        patches = []
        for line in f:
            patch = re.sub('#.*$', '', line).strip()
            if patch:
                patches.append(patch)
    else:
        patches = parse_patches(args, all)

    # working with "topush" patches in reverse order might be a bit
    # more efficient for large series but the main reason is for the
    # "topop != topush" comparison to work
    patches.reverse()

    topush = []
    topop = []

    for p in patches:
        while p in applied:
            top = applied.pop()
            if not top in patches:
                topush.append(top)
            topop.append(top)
    topush = patches + topush

    # check whether the operation is really needed
    if topop != topush:
        if topop:
            pop_patches(topop)
        if topush:
            topush.reverse()
            push_patches(topush)
