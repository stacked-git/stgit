
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
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'send patches deeper down the stack'
usage = """%prog [-t <target patch>] [-n] [<patches>]

Pop all patches (or all patches including <target patch>), then
push the specified <patches> (the current patch by default), and
then push back into place the formerly-applied patches (unless -n
is also given)."""

options = [make_option('-n', '--nopush',
                       help = 'do not push the patches back after sinking',
                       action = 'store_true'),
           make_option('-t', '--to', metavar = 'TARGET',
                       help = 'sink patches below TARGET patch')]

def func(parser, options, args):
    """Sink patches down the stack.
    """

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    oldapplied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()
    all = unapplied + oldapplied

    if len(args) > 0:
        patches = parse_patches(args, all)
    else:
        patches = [ crt_series.get_current() ]

    crt_series.pop_patch(options.to or oldapplied[0])
    push_patches(patches)

    if not options.nopush:
        newapplied = crt_series.get_applied()
        def not_reapplied_yet(p):
            return not p in newapplied
        push_patches(filter(not_reapplied_yet, oldapplied))
