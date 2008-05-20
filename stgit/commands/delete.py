
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

from optparse import make_option

from stgit.commands import common
from stgit.lib import transaction

help = 'delete patches'
usage = """%prog [options] <patch1> [<patch2>] [<patch3>..<patch4>]

Delete the patches passed as arguments.

Note that the 'delete' operation is irreversible."""

directory = common.DirectoryHasRepositoryLib()
options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]

def func(parser, options, args):
    """Delete one or more patches."""
    if options.branch:
        stack = directory.repository.get_stack(options.branch)
        iw = None # can't use index/workdir to manipulate another branch
    else:
        stack = directory.repository.current_stack
        iw = stack.repository.default_iw
    if args:
        patches = set(common.parse_patches(args, list(stack.patchorder.all)))
    else:
        parser.error('No patches specified')
    def allow_conflicts(trans):
        # Allow conflicts if the topmost patch stays the same.
        if stack.patchorder.applied:
            return (trans.applied
                    and trans.applied[-1] == stack.patchorder.applied[-1])
        else:
            return not trans.applied
    trans = transaction.StackTransaction(stack, 'delete',
                                         allow_conflicts = allow_conflicts)
    try:
        to_push = trans.delete_patches(lambda pn: pn in patches)
        for pn in to_push:
            trans.push_patch(pn, iw)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)
