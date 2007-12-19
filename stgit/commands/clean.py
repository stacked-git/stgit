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
from stgit.out import *
from stgit.commands import common
from stgit.lib import transaction

help = 'delete the empty patches in the series'
usage = """%prog [options]

Delete the empty patches in the whole series or only those applied or
unapplied. A patch is considered empty if the two commit objects
representing its boundaries refer to the same tree object."""

directory = common.DirectoryHasRepositoryLib()
options = [make_option('-a', '--applied',
                       help = 'delete the empty applied patches',
                       action = 'store_true'),
           make_option('-u', '--unapplied',
                       help = 'delete the empty unapplied patches',
                       action = 'store_true')]


def _clean(stack, clean_applied, clean_unapplied):
    def deleting(pn):
        out.info('Deleting empty patch %s' % pn)
    trans = transaction.StackTransaction(stack, 'clean')
    if clean_unapplied:
        trans.unapplied = []
        for pn in stack.patchorder.unapplied:
            p = stack.patches.get(pn)
            if p.is_empty():
                trans.patches[pn] = None
                deleting(pn)
            else:
                trans.unapplied.append(pn)
    if clean_applied:
        trans.applied = []
        parent = stack.base
        for pn in stack.patchorder.applied:
            p = stack.patches.get(pn)
            if p.is_empty():
                trans.patches[pn] = None
                deleting(pn)
            else:
                if parent != p.commit.data.parent:
                    parent = trans.patches[pn] = stack.repository.commit(
                        p.commit.data.set_parent(parent))
                else:
                    parent = p.commit
                trans.applied.append(pn)
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
