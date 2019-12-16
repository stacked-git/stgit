# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    git_commit,
    post_rebase,
    prepare_rebase,
    rebase,
)

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

help = 'Move the stack base to another point in history'
kind = 'stack'
usage = ['[options] [--] <new-base-id>']
description = """
Pop all patches from current stack, move the stack base to the given
<new-base-id> and push the patches back.

If you experience merge conflicts, resolve the problem and continue
the rebase by executing the following sequence:

        $ git add --update
        $ stg refresh
        $ stg goto top-patch

Or if you want to skip that patch:

        $ stg undo --hard
        $ stg push next-patch..top-patch"""

args = ['commit']
options = [
    opt(
        '-n',
        '--nopush',
        action='store_true',
        short='Do not push the patches back after rebasing',
    ),
    opt(
        '-m',
        '--merged',
        action='store_true',
        short='Check for patches merged upstream',
    ),
]

directory = DirectoryGotoTopLevel()


def func(parser, options, args):
    """Rebase the current stack
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    repository = directory.repository
    stack = repository.get_stack()
    iw = repository.default_iw

    if stack.protected:
        raise CmdException('This branch is protected. Rebase is not permitted')

    target = git_commit(args[0], repository)

    applied = stack.patchorder.applied

    retval = prepare_rebase(stack, 'rebase')
    if retval:
        return retval

    rebase(stack, iw, target)
    if not options.nopush:
        return post_rebase(
            stack, applied, 'rebase', check_merged=options.merged
        )
