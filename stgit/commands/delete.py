
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


help = 'remove the topmost or any unapplied patch'
usage = """%prog [options] <patch>

Delete the patch passed as argument. The patch to be deleted can only
be part of the unapplied list or be the topmost one, in the latter
case the command also popping it from the stack. Note that the
'delete' operation is irreversible."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]

def func(parser, options, args):
    """Deletes a patch
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    if args[0] == crt_series.get_current():
        check_local_changes()
        check_conflicts()
        check_head_top_equal()

    crt_series.delete_patch(args[0])
    print 'Patch "%s" successfully deleted' % args[0]

    if not options.branch:
        print_crt_patch()
