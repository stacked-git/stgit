
__copyright__ = """
Copyright (C) 2007, Yann Dirson <ydirson@altern.org>

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

help = 'Send patches deeper down the stack'
usage = ['[-t <target patch>] [-n] [<patches>]']
description = """
This is the opposite operation of stglink:float[]: move the specified
patches down the stack.  It is for example useful to group stable
patches near the bottom of the stack, where they are less likely to be
impacted by the push of another patch, and from where they can be more
easily committed or pushed.

If no patch is specified on command-line, the current patch gets sunk.
By default patches are sunk to the bottom of the stack, but the '--to'
option allows to place them under any applied patch.

Sinking internally involves popping all patches (or all patches
including <target patch>), then pushing the patches to sink, and then
(unless '--nopush' is also given) pushing back into place the
formerly-applied patches."""

options = [
    opt('-n', '--nopush', action = 'store_true',
        short = 'Do not push the patches back after sinking', long = """
        Do not push back on the stack the formerly-applied patches.
        Only the patches to sink are pushed."""),
    opt('-t', '--to', metavar = 'TARGET',
        short = 'Sink patches below the TARGET patch', long = """
        Specify a target patch to place the patches below, instead of
        sinking them to the bottom of the stack.""")]

directory = DirectoryGotoToplevel()

def func(parser, options, args):
    """Sink patches down the stack.
    """

    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

    oldapplied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()
    all = oldapplied + unapplied

    if options.to and not options.to in oldapplied:
        raise CmdException('Cannot sink below %s, since it is not applied'
                           % options.to)

    if len(args) > 0:
        patches = parse_patches(args, all)
    else:
        current = crt_series.get_current()
        if not current:
            raise CmdException('No patch applied')
        patches = [current]

    before_patches = after_patches = []

    # pop necessary patches
    if oldapplied:
        if options.to:
            pop_idx = oldapplied.index(options.to)
        else:
            pop_idx = 0
        after_patches = [p for p in oldapplied[pop_idx:] if p not in patches]

        # find the deepest patch to pop
        sink_applied = [p for p in oldapplied if p in patches]
        if sink_applied:
            sinked_idx = oldapplied.index(sink_applied[0])
            if sinked_idx < pop_idx:
                # this is the case where sink brings patches forward
                before_patches = [p for p in oldapplied[sinked_idx:pop_idx]
                                  if p not in patches]
                pop_idx = sinked_idx

        crt_series.pop_patch(oldapplied[pop_idx])

    push_patches(crt_series, before_patches)
    push_patches(crt_series, patches)
    if not options.nopush:
        push_patches(crt_series, after_patches)
