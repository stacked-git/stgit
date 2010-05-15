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
from stgit.out import *
from stgit import argparse

help = 'Permanently store the applied patches into the stack base'
kind = 'stack'
usage = ['',
         '[--] <patchnames>',
         '-n NUM',
         '--all']
description = """
Merge one or more patches into the base of the current stack and
remove them from the series while advancing the base. This is the
opposite of 'stg uncommit'. Use this command if you no longer want to
manage a patch with StGIT.

By default, the bottommost patch is committed. If patch names are
given, the stack is rearranged so that those patches are at the
bottom, and then they are committed.

The -n/--number option specifies the number of applied patches to
commit (counting from the bottom of the stack). If -a/--all is given,
all applied patches are committed."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches)]
options = [
    opt('-n', '--number', type = 'int',
        short = 'Commit the specified number of patches'),
    opt('-a', '--all', action = 'store_true',
        short = 'Commit all applied patches')]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Commit a number of patches."""
    stack = directory.repository.current_stack
    args = common.parse_patches(args, list(stack.patchorder.all_visible))
    if len([x for x in [args, options.number != None, options.all] if x]) > 1:
        parser.error('too many options')
    if args:
        patches = [pn for pn in stack.patchorder.all_visible if pn in args]
        bad = set(args) - set(patches)
        if bad:
            raise common.CmdException('Nonexistent or hidden patch names: %s'
                                      % ', '.join(sorted(bad)))
    elif options.number != None:
        if options.number <= len(stack.patchorder.applied):
            patches = stack.patchorder.applied[:options.number]
        else:
            raise common.CmdException('There are not that many applied patches')
    elif options.all:
        patches = stack.patchorder.applied
    else:
        patches = stack.patchorder.applied[:1]
    if not patches:
        raise common.CmdException('No patches to commit')

    iw = stack.repository.default_iw
    def allow_conflicts(trans):
        # As long as the topmost patch stays where it is, it's OK to
        # run "stg commit" with conflicts in the index.
        return len(trans.applied) >= 1
    trans = transaction.StackTransaction(stack, 'commit',
                                         allow_conflicts = allow_conflicts)
    try:
        common_prefix = 0
        for i in xrange(min(len(stack.patchorder.applied), len(patches))):
            if stack.patchorder.applied[i] == patches[i]:
                common_prefix += 1
            else:
                break
        if common_prefix < len(patches):
            to_push = [pn for pn in stack.patchorder.applied[common_prefix:]
                       if pn not in patches[common_prefix:]]
            # this pops all the applied patches from common_prefix
            trans.pop_patches(lambda pn: pn in to_push)
            for pn in patches[common_prefix:]:
                trans.push_patch(pn, iw)
        else:
            to_push = []
        new_base = trans.patches[patches[-1]]
        for pn in patches:
            trans.patches[pn] = None
        trans.applied = [pn for pn in trans.applied if pn not in patches]
        trans.base = new_base
        out.info('Committed %d patch%s' % (len(patches),
                                           ['es', ''][len(patches) == 1]))
        for pn in to_push:
            trans.push_patch(pn, iw)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)
