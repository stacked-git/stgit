from stgit import argparse, utils
from stgit.argparse import opt, patch_range
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    parse_patches,
    run_commit_msg_hook,
)
from stgit.lib.transaction import StackTransaction, TransactionHalted

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

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

args = [patch_range('applied_patches', 'unapplied_patches')]
options = (
    [opt('-n', '--name', short='Name of squashed patch')]
    + argparse.message_options(save_template=True)
    + argparse.hook_options()
)

directory = DirectoryHasRepository()


class SaveTemplateDone(Exception):
    pass


def _strip_comments(message):
    for line in message.splitlines():
        if not line.startswith('#'):
            yield line


def _squash_patches(trans, patches, msg, save_template, no_verify=False):
    cd = trans.patches[patches[0]].data
    for pn in patches[1:]:
        c = trans.patches[pn]
        tree = trans.stack.repository.simple_merge(
            base=c.data.parent.data.tree,
            ours=cd.tree,
            theirs=c.data.tree,
        )
        if not tree:
            return None
        cd = cd.set_tree(tree)
    if msg is None:
        msg = "# This is a combination of %s patches.\n" % len(patches)
        for num, pn in enumerate(patches, 1):
            msg += "# This is the commit message for patch #%s (%s):" % (
                num,
                pn,
            )
            msg += "\n%s\n" % trans.patches[pn].data.message_str
        msg += (
            "# Please enter the commit message for your patch. Lines starting\n"
            "# with '#' will be ignored."
        )

        if save_template:
            save_template(msg.encode(cd.encoding))
            raise SaveTemplateDone()
        else:
            msg = utils.edit_string(msg, '.stgit-squash.txt')
    msg = '\n'.join(_strip_comments(msg)).strip()
    cd = cd.set_message(msg)

    if not no_verify:
        cd = run_commit_msg_hook(trans.stack.repository, cd)

    return cd


def _squash(stack, iw, name, msg, save_template, patches, no_verify=False):
    # If a name was supplied on the command line, make sure it's OK.
    if name and name not in patches and name in stack.patches:
        raise CmdException('Patch name "%s" already taken' % name)

    def get_name(cd):
        return name or stack.patches.make_name(cd.message_str, allow=patches)

    def make_squashed_patch(trans, new_commit_data):
        name = get_name(new_commit_data)
        trans.patches[name] = stack.repository.commit(new_commit_data)
        trans.unapplied.insert(0, name)

    trans = StackTransaction(stack, 'squash', allow_conflicts=True)
    push_new_patch = bool(set(patches) & set(trans.applied))
    try:
        new_commit_data = _squash_patches(trans, patches, msg, save_template, no_verify)
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
            new_commit_data = _squash_patches(
                trans, patches, msg, save_template, no_verify
            )
            popped_extra = trans.delete_patches(lambda pn: pn in patches)
            assert not popped_extra
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
    except TransactionHalted:
        pass
    return trans.run(iw)


def func(parser, options, args):
    stack = directory.repository.current_stack
    patches = parse_patches(args, list(stack.patchorder.all))
    if len(patches) < 2:
        raise CmdException('Need at least two patches')
    if options.name and not stack.patches.is_name_valid(options.name):
        raise CmdException('Patch name "%s" is invalid' % options.name)
    return _squash(
        stack,
        stack.repository.default_iw,
        options.name,
        options.message,
        options.save_template,
        patches,
        options.no_verify,
    )
