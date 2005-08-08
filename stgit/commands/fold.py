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


help = 'integrate a GNU diff patch into the current patch'
usage = """%prog [options] [<file>]

Apply the given GNU diff file (or the standard input) onto the top of
the current patch. With the '--threeway' option, the patch is applied
onto the bottom of the current patch and a three-way merged is
performed with the current top."""

options = [make_option('-t', '--threeway',
                       help = 'perform a three-way merge with the current patch',
                       action = 'store_true'),
           make_option('-n', '--norefresh',
                       help = 'do not refresh the current patch',
                       action = 'store_true')]


def func(parser, options, args):
    """Integrate a GNU diff patch into the current patch
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    if len(args) == 1:
        filename = args[0]
    else:
        filename = None

    current = crt_series.get_current()
    if not current:
        raise CmdException, 'No patches applied'

    if filename:
        print 'Folding patch "%s"...' % filename,
    else:
        print 'Folding patch from stdin...',
    sys.stdout.flush()

    if options.threeway:
        crt_patch = crt_series.get_patch(current)
        bottom = crt_patch.get_bottom()
        top = crt_patch.get_top()

        git.switch(bottom)
        git.apply_patch(filename)
        fold_head = crt_series.refresh_patch(commit_only = True)
        git.switch(top)

        git.merge(bottom, top, fold_head)
    else:
        git.apply_patch(filename)

    # no merge conflicts at this point, exception would have been raised
    modified = git.local_changes()

    if not options.norefresh and modified:
        crt_series.refresh_patch()

    if modified:
        print 'done'
    else:
        print 'done (unchanged)'
