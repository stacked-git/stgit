# -*- coding: utf-8 -*-

__copyright__ = """
Copyright (C) 2008, Karl Hasselström <kha@treskal.com>

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
from stgit.lib import git, log, transaction
from stgit.out import out

help = 'Reset the patch stack to an earlier state'
kind = 'stack'
usage = ['<state> [<patchnames>]']
description = """
Reset the patch stack to an earlier state. The state is specified with
a commit from a stack log; for a branch foo, StGit stores the stack
log in foo.stgit^. So to undo the last N StGit commands (or rather,
the last N log entries; there is not an exact one-to-one
relationship), you would say

  stg reset foo.stgit^~N

or, if you are not sure how many steps to undo, you can view the log
with "git log" or gitk

  gitk foo.stgit^

and then reset to any sha1 you see in the log.

If one or more patch names are given, reset only those patches, and
leave the rest alone."""

options = []

directory = common.DirectoryHasRepositoryLib()

def reset_stack(stack, iw, state, only_patches):
    only_patches = set(only_patches)
    def mask(s):
        if only_patches:
            return s & only_patches
        else:
            return s
    patches_to_reset = mask(set(state.applied + state.unapplied + state.hidden))
    existing_patches = set(stack.patchorder.all)
    to_delete = mask(existing_patches - patches_to_reset)
    trans = transaction.StackTransaction(stack, 'reset')

    # If we have to change the stack base, we need to pop all patches
    # first.
    if not only_patches and trans.base != state.base:
        trans.pop_patches(lambda pn: True)
        out.info('Setting stack base to %s' % state.base.sha1)
        trans.base = state.base

    # In one go, do all the popping we have to in order to pop the
    # patches we're going to delete or modify.
    def mod(pn):
        if only_patches and not pn in only_patches:
            return False
        if pn in to_delete:
            return True
        if stack.patches.get(pn).commit != state.patches.get(pn, None):
            return True
        return False
    trans.pop_patches(mod)

    # Delete and modify/create patches. We've previously popped all
    # patches that we touch in this step.
    trans.delete_patches(lambda pn: pn in to_delete)
    for pn in patches_to_reset:
        if pn in existing_patches:
            if trans.patches[pn] == state.patches[pn]:
                continue
            else:
                out.info('Resetting %s' % pn)
        else:
            if pn in state.hidden:
                trans.hidden.append(pn)
            else:
                trans.unapplied.append(pn)
            out.info('Resurrecting %s' % pn)
        trans.patches[pn] = state.patches[pn]

    # Push/pop patches as necessary.
    try:
        if only_patches:
            # Push all the patches that we've popped, if they still
            # exist.
            pushable = set(trans.unapplied)
            for pn in stack.patchorder.applied:
                if pn in pushable:
                    trans.push_patch(pn, iw)
        else:
            # Recreate the exact order specified by the goal state.
            trans.reorder_patches(state.applied, state.unapplied,
                                  state.hidden, iw)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)

def func(parser, options, args):
    stack = directory.repository.current_stack
    if len(args) >= 1:
        ref, patches = args[0], args[1:]
        state = log.get_log_entry(stack.repository, ref)
    else:
        raise common.CmdException('Wrong number of arguments')
    return reset_stack(stack, stack.repository.default_iw, state, patches)
