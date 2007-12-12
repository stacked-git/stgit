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

from stgit.commands import common
from stgit.lib import transaction
from stgit.out import *

help = 'permanently store the applied patches into stack base'
usage = """%prog [options]

Merge the applied patches into the base of the current stack and
remove them from the series while advancing the base.

Use this command only if you want to permanently store the applied
patches and no longer manage them with StGIT."""

directory = common.DirectoryHasRepositoryLib()
options = []


def func(parser, options, args):
    """Merge the applied patches into the base of the current stack
       and remove them from the series while advancing the base
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    stack = directory.repository.current_stack
    patches = stack.patchorder.applied
    if not patches:
        raise CmdException('No patches to commit')
    out.start('Committing %d patches' % len(patches))
    trans = transaction.StackTransaction(stack, 'stg commit')
    for pn in patches:
        trans.patches[pn] = None
    trans.applied = []
    trans.base = stack.head
    trans.run()
    out.done()
