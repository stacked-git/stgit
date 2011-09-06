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
from stgit import argparse
from stgit.argparse import opt

help = 'Push or pop patches to the given one'
kind = 'stack'
usage = ['[options] [--] <patch-name>']
description = """
Push/pop patches to/from the stack until the one given on the command
line becomes current."""

args = [argparse.other_applied_patches, argparse.unapplied_patches]
options = argparse.keep_option() + argparse.merged_option()

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    if len(args) != 1:
        parser.error('incorrect number of arguments')
    patch = args[0]

    stack = directory.repository.current_stack
    iw = stack.repository.default_iw
    clean_iw = (not options.keep and iw) or None
    trans = transaction.StackTransaction(stack, 'goto',
                                         check_clean_iw = clean_iw)

    if not patch in trans.all_patches:
        candidate = common.get_patch_from_list(patch, trans.all_patches)
        if candidate is None:
            raise common.CmdException('Patch "%s" does not exist' % patch)
        patch = candidate

    if patch in trans.applied:
        to_pop = set(trans.applied[trans.applied.index(patch)+1:])
        assert not trans.pop_patches(lambda pn: pn in to_pop)
    elif patch in trans.unapplied:
        try:
            to_push = trans.unapplied[:trans.unapplied.index(patch)+1]
            if options.merged:
                merged = set(trans.check_merged(to_push))
            else:
                merged = set()
            for pn in to_push:
                trans.push_patch(pn, iw, allow_interactive = True,
                                 already_merged = pn in merged)
        except transaction.TransactionHalted:
            pass
    else:
        raise common.CmdException('Cannot goto a hidden patch')
    return trans.run(iw)
