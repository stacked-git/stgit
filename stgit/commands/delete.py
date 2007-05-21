
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


help = 'delete patches'
usage = """%prog [options] <patch1> [<patch2>] [<patch3>..<patch4>]

Delete the patches passed as arguments. If an applied patch is to be
deleted, all other patches applied on top of it must be deleted too,
and they must be explicitly specified, since this command will not try
to delete a patch unless you explicitly ask it to. If any applied
patches are deleted, they are popped from the stack.

Note that the 'delete' operation is irreversible."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]

def func(parser, options, args):
    """Deletes one or more patches.
    """
    applied_patches = crt_series.get_applied()
    unapplied_patches = crt_series.get_unapplied()
    all_patches = applied_patches + unapplied_patches

    if args:
        patches = parse_patches(args, all_patches, len(applied_patches))
    else:
        parser.error('No patches specified')

    applied = []

    # find the applied patches to be deleted. We can only delete
    # consecutive patches in the applied range
    for patch in applied_patches[::-1]:
        if patch in patches:
            applied.append(patch)
            patches.remove(patch)
        else:
            break

    # any applied patches to be deleted but not in consecutive order?
    for patch in patches:
        if patch in applied_patches:
            raise CmdException, 'Cannot delete the applied patch "%s"' % patch

    if applied and not options.branch:
        check_local_changes()
        check_conflicts()
        check_head_top_equal()

    # delete the patches
    for patch in applied + patches:
        crt_series.delete_patch(patch)
        out.info('Patch "%s" successfully deleted' % patch)

    if not options.branch:
        print_crt_patch()
