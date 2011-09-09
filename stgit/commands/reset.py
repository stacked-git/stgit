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

from stgit.argparse import opt
from stgit.commands import common
from stgit.lib import git, log, transaction
from stgit.out import out
from stgit import argparse, utils

help = 'Reset the patch stack to an earlier state'
kind = 'stack'
usage = ['[options] [--] [<state> [<patchnames>]]']
description = """
Reset the patch stack to an earlier state. If no state is specified,
reset only the changes in the worktree.

The state is specified with a commit id from a stack log; "stg log" lets
you view this log, and "stg reset" lets you reset to any state you see
in the log. If one or more patch names are given, reset only those
patches, and leave the rest alone."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches,
                             argparse.hidden_patches)]
options = [
    opt('--hard', action = 'store_true',
        short = 'Discard changes in your index/worktree')]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    stack = directory.repository.current_stack
    iw = stack.repository.default_iw
    if len(args) >= 1:
        ref, patches = args[0], args[1:]
        state = log.get_log_entry(stack.repository, ref,
                                  stack.repository.rev_parse(ref))
    elif options.hard:
        iw.checkout_hard(stack.head.data.tree)
        return utils.STGIT_SUCCESS
    else:
        raise common.CmdException('Wrong options or number of arguments')

    trans = transaction.StackTransaction(stack, 'reset',
                                         discard_changes = options.hard,
                                         allow_bad_head = True)
    try:
        if patches:
            log.reset_stack_partially(trans, iw, state, patches)
        else:
            log.reset_stack(trans, iw, state)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw, allow_bad_head = not patches)
