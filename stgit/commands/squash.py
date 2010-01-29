# -*- coding: utf-8 -*-

__copyright__ = """
Copyright (C) 2007, Karl Hasselström <kha@treskal.com>

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
from stgit.out import *
from stgit import argparse, utils
from stgit.commands import common
from stgit.lib import git, transaction

help = 'Squash two or more patches into one'
kind = 'stack'
usage = ['[options] [--] <patches>']
description = """
Squash two or more patches, creating one big patch that contains all
their changes. In more detail:

  1. Pop all the given patches, plus any other patches on top of them.

  2. Push the given patches in the order they were given on the
     command line.

  3. Squash the given patches into one big patch.

  4. Allow the user to edit the commit message of the new patch
     interactively.

  5. Push the other patches that were popped in step (1).

Conflicts can occur whenever we push a patch; that is, in step (2) and
(5). If there are conflicts, the command will stop so that you can
resolve them."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches)]
options = [opt('-n', '--name', short = 'Name of squashed patch')
           ] + argparse.message_options(save_template = True)

directory = common.DirectoryHasRepositoryLib()

class SaveTemplateDone(Exception):
    pass

def _squash_patches(trans, patches, msg, save_template):
    cd = trans.patches[patches[0]].data
    cd = git.CommitData(tree = cd.tree, parents = cd.parents)
    for pn in patches[1:]:
        c = trans.patches[pn]
        tree = trans.stack.repository.simple_merge(
            base = c.data.parent.data.tree,
            ours = cd.tree, theirs = c.data.tree)
        if not tree:
            return None
        cd = cd.set_tree(tree)
    if msg == None:
        msg = utils.append_comment(
            trans.patches[patches[0]].data.message,
            '\n\n'.join('%s\n\n%s' % (pn.ljust(70, '-'),
                                      trans.patches[pn].data.message)
                        for pn in patches[1:]))
        if save_template:
            save_template(msg)
            raise SaveTemplateDone()
        else:
            msg = utils.edit_string(msg, '.stgit-squash.txt')
    msg = utils.strip_comment(msg).strip()
    cd = cd.set_message(msg)

    return cd

def _squash(stack, iw, name, msg, save_template, patches):

    # If a name was supplied on the command line, make sure it's OK.
    def bad_name(pn):
        return pn not in patches and stack.patches.exists(pn)
    def get_name(cd):
        return name or utils.make_patch_name(cd.message, bad_name)
    if name and bad_name(name):
        raise common.CmdException('Patch name "%s" already taken')

    def make_squashed_patch(trans, new_commit_data):
        name = get_name(new_commit_data)
        trans.patches[name] = stack.repository.commit(new_commit_data)
        trans.unapplied.insert(0, name)

    trans = transaction.StackTransaction(stack, 'squash',
                                         allow_conflicts = True)
    push_new_patch = bool(set(patches) & set(trans.applied))
    try:
        new_commit_data = _squash_patches(trans, patches, msg, save_template)
        if new_commit_data:
            # We were able to construct the squashed commit
            # automatically. So just delete its constituent patches.
            to_push = trans.delete_patches(lambda pn: pn in patches)
        else:
            # Automatic construction failed. So push the patches
            # consecutively, so that a second construction attempt is
            # guaranteed to work.
            to_push = trans.pop_patches(lambda pn: pn in patches)
            for pn in patches:
                trans.push_patch(pn, iw)
            new_commit_data = _squash_patches(trans, patches, msg,
                                                save_template)
            assert not trans.delete_patches(lambda pn: pn in patches)
        make_squashed_patch(trans, new_commit_data)

        # Push the new patch if necessary, and any unrelated patches we've
        # had to pop out of the way.
        if push_new_patch:
            trans.push_patch(get_name(new_commit_data), iw)
        for pn in to_push:
            trans.push_patch(pn, iw)
    except SaveTemplateDone:
        trans.abort(iw)
        return
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)

def func(parser, options, args):
    stack = directory.repository.current_stack
    patches = common.parse_patches(args, list(stack.patchorder.all))
    if len(patches) < 2:
        raise common.CmdException('Need at least two patches')
    return _squash(stack, stack.repository.default_iw, options.name,
                   options.message, options.save_template, patches)
