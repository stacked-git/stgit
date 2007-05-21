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


help = 'delete the empty patches in the series'
usage = """%prog [options]

Delete the empty patches in the whole series or only those applied or
unapplied. A patch is considered empty if the two commit objects
representing its boundaries refer to the same tree object."""

options = [make_option('-a', '--applied',
                       help = 'delete the empty applied patches',
                       action = 'store_true'),
           make_option('-u', '--unapplied',
                       help = 'delete the empty unapplied patches',
                       action = 'store_true')]


def __delete_empty(patches, applied):
    """Delete the empty patches
    """
    for p in patches:
        if crt_series.empty_patch(p):
            out.start('Deleting patch "%s"' % p)
            if applied and crt_series.patch_applied(p):
                crt_series.pop_patch(p)
            crt_series.delete_patch(p)
            out.done()
        elif applied and crt_series.patch_unapplied(p):
            crt_series.push_patch(p)

def func(parser, options, args):
    """Delete the empty patches in the series
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    if not (options.applied or options.unapplied):
        options.applied = options.unapplied = True

    if options.applied:
        applied = crt_series.get_applied()
        __delete_empty(applied, True)

    if options.unapplied:
        unapplied = crt_series.get_unapplied()
        __delete_empty(unapplied, False)

    print_crt_patch()
