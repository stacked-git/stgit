"""Patch editing command
"""

__copyright__ = """
Copyright (C) 2007, Catalin Marinas <catalin.marinas@gmail.com>

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
from stgit import argparse, git, utils
from stgit.commands import common
from stgit.lib import git as gitlib, transaction, edit
from stgit.out import *

help = 'Edit a patch description or diff'
kind = 'patch'
usage = ['[options] [--] [<patch>]']
description = """
Edit the description and author information of the given patch (or the
current patch if no patch name was given). With --diff, also edit the
diff.

The editor is invoked with the following contents:

  From: A U Thor <author@example.com>
  Date: creation date

  Patch description

If --diff was specified, the diff appears at the bottom, after a
separator:

  ---

  Diff text

Command-line options can be used to modify specific information
without invoking the editor. (With the --edit option, the editor is
invoked even if such command-line options are given.)

If the patch diff is edited but does not apply, no changes are made to
the patch at all. The edited patch is saved to a file which you can
feed to "stg edit --file", once you have made sure it does apply.

With --set-tree you set the git tree of the patch to the specified
TREE-ISH without changing the tree of any other patches. When used on
the top patch, the index and work tree will be updated to match the
tree.  This low-level option is primarily meant to be used by tools
built on top of StGit, such as the Emacs mode. See also the --set-tree
flag of stg push."""

args = [argparse.applied_patches, argparse.unapplied_patches,
        argparse.hidden_patches]
options = (
    [ opt('-d', '--diff', action = 'store_true',
          short = 'Edit the patch diff'),
      opt('-e', '--edit', action = 'store_true',
          short = 'Invoke interactive editor') ] +
    argparse.sign_options() +
    argparse.message_options(save_template = True) +
    argparse.author_options() + argparse.diff_opts_option() +
    [ opt('-t', '--set-tree', action = 'store',
          metavar = 'TREE-ISH',
          short = 'Set the git tree of the patch to TREE-ISH') ])

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Edit the given patch or the current one.
    """
    stack = directory.repository.current_stack

    if len(args) == 0:
        if not stack.patchorder.applied:
            raise common.CmdException(
                'Cannot edit top patch, because no patches are applied')
        patchname = stack.patchorder.applied[-1]
    elif len(args) == 1:
        [patchname] = args
        if not stack.patches.exists(patchname):
            raise common.CmdException('%s: no such patch' % patchname)
    else:
        parser.error('Cannot edit more than one patch')

    cd = orig_cd = stack.patches.get(patchname).commit.data

    if options.set_tree:
        cd = cd.set_tree(stack.repository.rev_parse(
                options.set_tree, discard_stderr = True, object_type = 'tree'))

    cd, failed_diff = edit.auto_edit_patch(
        stack.repository, cd, msg = options.message, contains_diff = True,
        author = options.author, committer = lambda p: p,
        sign_str = options.sign_str)

    if options.save_template:
        options.save_template(
            edit.patch_desc(stack.repository, cd,
                            options.diff, options.diff_flags, failed_diff))
        return utils.STGIT_SUCCESS

    if cd == orig_cd or options.edit:
        cd, failed_diff = edit.interactive_edit_patch(
            stack.repository, cd, options.diff, options.diff_flags, failed_diff)

    def failed():
        fn = '.stgit-failed.patch'
        f = file(fn, 'w')
        f.write(edit.patch_desc(stack.repository, cd,
                                options.diff, options.diff_flags, failed_diff))
        f.close()
        out.error('Edited patch did not apply.',
                  'It has been saved to "%s".' % fn)
        return utils.STGIT_COMMAND_ERROR

    # If we couldn't apply the patch, fail without even trying to
    # effect any of the changes.
    if failed_diff:
        return failed()

    # The patch applied, so now we have to rewrite the StGit patch
    # (and any patches on top of it).
    iw = stack.repository.default_iw
    trans = transaction.StackTransaction(stack, 'edit', allow_conflicts = True)
    if patchname in trans.applied:
        popped = trans.applied[trans.applied.index(patchname)+1:]
        assert not trans.pop_patches(lambda pn: pn in popped)
    else:
        popped = []
    trans.patches[patchname] = stack.repository.commit(cd)
    try:
        for pn in popped:
            if options.set_tree:
                trans.push_tree(pn)
            else:
                trans.push_patch(pn, iw, allow_interactive = True)
    except transaction.TransactionHalted:
        pass
    try:
        # Either a complete success, or a conflict during push. But in
        # either case, we've successfully effected the edits the user
        # asked us for.
        return trans.run(iw)
    except transaction.TransactionException:
        # Transaction aborted -- we couldn't check out files due to
        # dirty index/worktree. The edits were not carried out.
        return failed()
