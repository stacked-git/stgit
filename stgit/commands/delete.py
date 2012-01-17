
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
from stgit.commands import common
from stgit.lib import transaction
from stgit import argparse

help = 'Delete patches'
kind = 'patch'
usage = ['[options] [--] <patch1> [<patch2>] [<patch3>..<patch4>]']
description = """
Delete the patches passed as arguments."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches)]
options = [
    opt('--spill', action = 'store_true',
        short = 'Spill patch contents to worktree and index', long = """
        Delete the patches, but do not touch the index and worktree.
        This only works with applied patches at the top of the stack.
        The effect is to "spill" the patch contents into the index and
        worktree. This can be useful e.g. if you want to split a patch
        into several smaller pieces."""),
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch'),
    opt('-t', '--top', action = 'store_true',
        short = 'Delete top patch'),]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Delete one or more patches."""
    stack = directory.repository.get_stack(options.branch)
    if options.branch:
        iw = None # can't use index/workdir to manipulate another branch
    else:
        iw = stack.repository.default_iw
    if args and options.top:
        parser.error('Either --top or patches must be specified')
    elif args:
        patches = set(common.parse_patches(args, list(stack.patchorder.all),
                                           len(stack.patchorder.applied)))
    elif options.top:
        applied = stack.patchorder.applied
        if applied:
            patches = set([applied[-1]])
        else:
            raise common.CmdException, 'No patches applied'
    else:
        parser.error('No patches specified')

    if options.spill:
        if set(stack.patchorder.applied[-len(patches):]) != patches:
            parser.error('Can only spill topmost applied patches')
        iw = None # don't touch index+worktree

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
