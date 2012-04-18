
__copyright__ = """
Copyright (C) 2007, Yann Dirson <ydirson@altern.org>

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
from stgit.commands import common
from stgit.lib import transaction
from stgit import argparse

help = 'Send patches deeper down the stack'
kind = 'stack'
usage = ['[-t <target patch>] [-n] [--] [<patches>]']
description = """
This is the opposite operation of linkstg:float[]: move the specified
patches down the stack.  It is for example useful to group stable
patches near the bottom of the stack, where they are less likely to be
impacted by the push of another patch, and from where they can be more
easily committed or pushed.

If no patch is specified on command-line, the current patch gets sunk.
By default patches are sunk to the bottom of the stack, but the '--to'
option allows one to place them under any applied patch.

Sinking internally involves popping all patches (or all patches
including <target patch>), then pushing the patches to sink, and then
(unless '--nopush' is also given) pushing back into place the
formerly-applied patches."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches)]
options = [
    opt('-n', '--nopush', action = 'store_true',
        short = 'Do not push the patches back after sinking', long = """
        Do not push back on the stack the formerly-applied patches.
        Only the patches to sink are pushed."""),
    opt('-t', '--to', metavar = 'TARGET', args = [argparse.applied_patches],
        short = 'Sink patches below the TARGET patch', long = """
        Specify a target patch to place the patches below, instead of
        sinking them to the bottom of the stack.""")
    ] + argparse.keep_option()

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Sink patches down the stack.
    """
    stack = directory.repository.current_stack

    if options.to and not options.to in stack.patchorder.applied:
        raise common.CmdException('Cannot sink below %s since it is not applied'
                                  % options.to)

    if len(args) > 0:
        patches = common.parse_patches(args, stack.patchorder.all)
    else:
        # current patch
        patches = list(stack.patchorder.applied[-1:])

    if not patches:
        raise common.CmdException('No patches to sink')
    if options.to and options.to in patches:
        raise common.CmdException('Cannot have a sinked patch as target')

    applied = [p for p in stack.patchorder.applied if p not in patches]
    if options.to:
        insert_idx = applied.index(options.to)
    else:
        insert_idx = 0
    applied = applied[:insert_idx] + patches + applied[insert_idx:]
    unapplied = [p for p in stack.patchorder.unapplied if p not in patches]

    iw = stack.repository.default_iw
    clean_iw = (not options.keep and iw) or None
    trans = transaction.StackTransaction(stack, 'sink',
                                         check_clean_iw = clean_iw)

    try:
        trans.reorder_patches(applied, unapplied, iw = iw,
                              allow_interactive = True)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)
