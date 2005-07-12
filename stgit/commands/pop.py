
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


help = 'pop the top of the series'
usage = '%prog [options]'

options = [make_option('-a', '--all',
                       help = 'pop all the applied patches',
                       action = 'store_true'),
           make_option('-n', '--number', type = 'int',
                       help = 'pop the specified number of patches'),
           make_option('-t', '--to', metavar = 'PATCH',
                       help = 'pop all patches up to PATCH')]


def func(parser, options, args):
    """Pop the topmost patch from the stack
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'
    applied.reverse()

    if options.to:
        if options.to not in applied:
            raise CmdException, 'Patch "%s" not applied' % options.to
        patches = applied[:applied.index(options.to)]
    elif options.number:
        patches = applied[:options.number]
    elif options.all:
        patches = applied
    else:
        patches = [applied[0]]

    if patches == []:
        raise CmdException, 'No patches to pop'

    # pop everything to the given patch
    p = patches[-1]
    if len(patches) == 1:
        print 'Popping patch "%s"...' % p,
    else:
        print 'Popping "%s" - "%s" patches...' % (patches[0], p),
    sys.stdout.flush()

    crt_series.pop_patch(p)

    print 'done'
    print_crt_patch()
