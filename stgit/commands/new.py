# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit import argparse, utils
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    run_commit_msg_hook,
    update_commit_data,
)
from stgit.lib.git import CommitData, Person
from stgit.lib.transaction import StackTransaction

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

help = 'Create a new, empty patch'
kind = 'patch'
usage = ['[options] [--] [<name>]']
description = """
Create a new, empty patch on the current stack. The new patch is
created on top of the currently applied patches, and is made the new
top of the stack. Uncommitted changes in the work tree are not
included in the patch -- that is handled by linkstg:refresh[].

The given name must be unique in the stack, and may only contain
alphanumeric characters, dashes and underscores. If no name is given,
one is generated from the first line of the patch's commit message.

An editor will be launched to edit the commit message to be used for
the patch, unless the '--message' flag already specified one. The
'patchdescr.tmpl' template file (if available) is used to pre-fill the
editor."""

args = []
options = (
    argparse.author_options()
    + argparse.message_options(save_template=True)
    + argparse.sign_options()
    + argparse.hook_options()
)

directory = DirectoryHasRepository()


def func(parser, options, args):
    """Create a new patch."""
    stack = directory.repository.current_stack
    if stack.repository.default_index.conflicts():
        raise CmdException(
            'Cannot create a new patch -- resolve conflicts first'
        )

    # Choose a name for the new patch -- or None, which means make one
    # up later when we've gotten hold of the commit message.
    if len(args) == 0:
        name = None
    elif len(args) == 1:
        name = args[0]
        if stack.patches.exists(name):
            raise CmdException('%s: patch already exists' % name)
        elif not stack.patches.is_name_valid(name):
            raise CmdException('Invalid patch name: "%s"' % name)
    else:
        parser.error('incorrect number of arguments')

    cd = CommitData(
        tree=stack.head.data.tree,
        parents=[stack.head],
        message='',
        author=Person.author(),
        committer=Person.committer(),
    )
    cd = update_commit_data(cd, options)

    if options.save_template:
        options.save_template(cd.message.encode('utf-8'))
        return utils.STGIT_SUCCESS

    if not options.no_verify:
        cd = run_commit_msg_hook(stack.repository, cd)

    if name is None:
        name = utils.make_patch_name(cd.message, stack.patches.exists)
        assert stack.patches.is_name_valid(name)

    # Write the new patch.
    stack.repository.default_iw
    trans = StackTransaction(stack, 'new: %s' % name)
    trans.patches[name] = stack.repository.commit(cd)
    trans.applied.append(name)
    return trans.run()
