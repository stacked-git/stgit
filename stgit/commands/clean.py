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

from stgit.argparse import opt
from stgit.out import *
from stgit.commands import common
from stgit.lib import transaction

help = 'Delete the empty patches in the series'
kind = 'stack'
usage = ['']
description = """
Delete the empty patches in the whole series or only those applied or
unapplied. A patch is considered empty if the two commit objects
representing its boundaries refer to the same tree object."""

args = []
options = [
    opt('-a', '--applied', action = 'store_true',
        short = 'Delete the empty applied patches'),
    opt('-u', '--unapplied', action = 'store_true',
        short = 'Delete the empty unapplied patches')]

directory = common.DirectoryHasRepositoryLib()

def _clean(stack, clean_applied, clean_unapplied):
    trans = transaction.StackTransaction(stack, 'clean', allow_conflicts = True)
    def del_patch(pn):
        if pn in stack.patchorder.applied:
            if pn == stack.patchorder.applied[-1]:
                # We're about to clean away the topmost patch. Don't
                # do that if we have conflicts, since that means the
                # patch is only empty because the conflicts have made
                # us dump its contents into the index and worktree.
                if stack.repository.default_index.conflicts():
                    return False
            return clean_applied and trans.patches[pn].data.is_nochange()
        elif pn in stack.patchorder.unapplied:
            return clean_unapplied and trans.patches[pn].data.is_nochange()
    for pn in trans.delete_patches(del_patch):
        trans.push_patch(pn)
    trans.run()

def func(parser, options, args):
    """Delete the empty patches in the series
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    if not (options.applied or options.unapplied):
        options.applied = options.unapplied = True

    _clean(directory.repository.current_stack,
           options.applied, options.unapplied)
