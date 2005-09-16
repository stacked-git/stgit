
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


help = 'push a patch on top of the series'
usage = """%prog [options] [<name>]

Push a patch (defaulting to the first unapplied one) or range of
patches to the stack. The 'push' operation allows patch reordering by
commuting them with the three-way merge algorithm. If the result of
the 'push' operation is not acceptable or if there are too many
conflicts, the '--undo' option can be used to revert the patch and the
tree to the state before the operation. Conflicts raised during the
push operation have to be fixed and the 'resolved' command run.

The 'push' command also notifies when the patch becomes empty after
the merge operation (i.e. it was fully merged upstream)."""

options = [make_option('-a', '--all',
                       help = 'push all the unapplied patches',
                       action = 'store_true'),
           make_option('-n', '--number', type = 'int',
                       help = 'push the specified number of patches'),
           make_option('-t', '--to', metavar = 'PATCH1[:PATCH2]',
                       help = 'push all patches to PATCH1 or between '
                       'PATCH1 and PATCH2'),
           make_option('--reverse',
                       help = 'push the patches in reverse order',
                       action = 'store_true'),
           make_option('--undo',
                       help = 'undo the last push operation',
                       action = 'store_true')]


def func(parser, options, args):
    """Pushes the given patch or all onto the series
    """
    # If --undo is passed, do the work and exit
    if options.undo:
        patch = crt_series.get_current()
        if not patch:
            raise CmdException, 'No patch to undo'

        print 'Undoing the "%s" push...' % patch,
        sys.stdout.flush()
        resolved_all()
        if crt_series.undo_push():
            print 'done'
        else:
            print 'done (patch unchanged)'
        print_crt_patch()

        return

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    unapplied = crt_series.get_unapplied()
    if not unapplied:
        raise CmdException, 'No more patches to push'

    if options.to:
        boundaries = options.to.split(':')
        if len(boundaries) == 1:
            if boundaries[0] not in unapplied:
                raise CmdException, 'Patch "%s" not unapplied' % boundaries[0]
            patches = unapplied[:unapplied.index(boundaries[0])+1]
        elif len(boundaries) == 2:
            if boundaries[0] not in unapplied:
                raise CmdException, 'Patch "%s" not unapplied' % boundaries[0]
            if boundaries[1] not in unapplied:
                raise CmdException, 'Patch "%s" not unapplied' % boundaries[1]
            lb = unapplied.index(boundaries[0])
            hb = unapplied.index(boundaries[1])
            if lb > hb:
                raise CmdException, 'Patch "%s" after "%s"' \
                      % (boundaries[0], boundaries[1])
            patches = unapplied[lb:hb+1]
        else:
            raise CmdException, 'incorrect parameters to "--to"'
    elif options.number:
        patches = unapplied[:options.number]
    elif options.all:
        patches = unapplied
    elif len(args) == 0:
        patches = [unapplied[0]]
    elif len(args) == 1:
        patches = [args[0]]
    else:
        parser.error('incorrect number of arguments')

    if patches == []:
        raise CmdException, 'No patches to push'

    if options.reverse:
        patches.reverse()

    print 'Trying fast-forward...'

    forwarded = crt_series.forward_patches(patches)
    if forwarded > 1:
        print 'Fast-forwarded patches "%s" - "%s"' % (patches[0],
                                                      patches[forwarded - 1])
    elif forwarded == 1:
        print 'Fast-forwarded patch "%s"' % patches[0]
    else:
        print 'Fast-forwarding failed, using normal pushing'

    for p in patches[forwarded:]:
        if p not in unapplied:
            raise CmdException, 'Patch "%s" not unapplied' % p

        print 'Pushing patch "%s"...' % p,
        sys.stdout.flush()

        crt_series.push_patch(p)

        if crt_series.empty_patch(p):
            print 'done (empty patch)'
        else:
            print 'done'
    print_crt_patch()
