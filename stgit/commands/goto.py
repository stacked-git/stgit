__copyright__ = """
Copyright (C) 2006, Catalin Marinas <catalin.marinas@gmail.com>

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

help = 'Push or pop patches to the given one'
kind = 'stack'
usage = ['<patch-name>']
description = """
Push/pop patches to/from the stack until the one given on the command
line becomes current. There is no '--undo' option for 'goto'. Use the
'push --undo' command for this."""

options = []

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    if len(args) != 1:
        parser.error('incorrect number of arguments')
    patch = args[0]

    stack = directory.repository.current_stack
    iw = stack.repository.default_iw
    trans = transaction.StackTransaction(stack, 'goto')
    if patch in trans.applied:
        to_pop = set(trans.applied[trans.applied.index(patch)+1:])
        assert not trans.pop_patches(lambda pn: pn in to_pop)
    elif patch in trans.unapplied:
        try:
            for pn in trans.unapplied[:trans.unapplied.index(patch)+1]:
                trans.push_patch(pn, iw)
        except transaction.TransactionHalted:
            pass
    elif patch in trans.hidden:
        raise common.CmdException('Cannot goto a hidden patch')
    else:
        raise common.CmdException('Patch "%s" does not exist' % patch)
    return trans.run(iw)
