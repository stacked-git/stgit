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

help = 'Reset the patch stack to an earlier state'
kind = 'stack'
usage = ['[options] <state> [<patchnames>]']
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

options = [
    opt('--hard', action = 'store_true',
        short = 'Discard changes in your index/worktree')]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    stack = directory.repository.current_stack
    if len(args) >= 1:
        ref, patches = args[0], args[1:]
        state = log.get_log_entry(stack.repository, ref,
                                  stack.repository.rev_parse(ref))
    else:
        raise common.CmdException('Wrong number of arguments')
    trans = transaction.StackTransaction(stack, 'reset',
                                         discard_changes = options.hard)
    try:
        log.reset_stack(trans, stack.repository.default_iw, state, patches)
    except transaction.TransactionHalted:
        pass
    return trans.run(stack.repository.default_iw)
