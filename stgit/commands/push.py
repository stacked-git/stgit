
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


help = 'push one or more patches onto of the stack'
usage = """%prog [options] [<patch1>] [<patch2>] [<patch3>..<patch4>]

Push one or more patches (defaulting to the first unapplied one) onto
the stack. The 'push' operation allows patch reordering by commuting
them with the three-way merge algorithm. If the result of the 'push'
operation is not acceptable or if there are too many conflicts, the
'--undo' option can be used to revert the last pushed patch. Conflicts
raised during the push operation have to be fixed and the 'resolved'
command run.

The command also notifies when the patch becomes empty (fully merged
upstream) or is modified (three-way merged) by the 'push' operation."""

options = [make_option('-a', '--all',
                       help = 'push all the unapplied patches',
                       action = 'store_true'),
           make_option('-n', '--number', type = 'int',
                       help = 'push the specified number of patches'),
           make_option('--reverse',
                       help = 'push the patches in reverse order',
                       action = 'store_true'),
           make_option('-m', '--merged',
                       help = 'check for patches merged upstream',
                       action = 'store_true'),
           make_option('--undo',
                       help = 'undo the last patch pushing',
                       action = 'store_true')]


def func(parser, options, args):
    """Pushes the given patch or all onto the series
    """

    # If --undo is passed, do the work and exit
    if options.undo:
        patch = crt_series.get_current()
        if not patch:
            raise CmdException, 'No patch to undo'

        out.start('Undoing push of "%s"' % patch)
        resolved_all()
        if crt_series.undo_push():
            out.done()
        else:
            out.done('patch unchanged')
        print_crt_patch()

        return

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

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

    push_patches(patches, options.merged)

    print_crt_patch()
