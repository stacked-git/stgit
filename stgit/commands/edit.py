"""Patch editing command"""

from stgit import argparse, utils
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    run_commit_msg_hook,
)
from stgit.config import config
from stgit.lib import edit

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

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

args = ['applied_patches', 'unapplied_patches', 'hidden_patches']
options = [
    opt('-d', '--diff', action='store_true', short='Edit the patch diff'),
    opt('-e', '--edit', action='store_true', short='Invoke interactive editor'),
]
options.extend(argparse.sign_options())
options.extend(argparse.message_options(save_template=True))
options.extend(argparse.hook_options())
options.extend(argparse.author_options())
options.extend(argparse.diff_opts_option())
options.append(
    opt(
        '-t',
        '--set-tree',
        action='store',
        metavar='TREE-ISH',
        short='Set the git tree of the patch to TREE-ISH',
    )
)


directory = DirectoryHasRepository()


def func(parser, options, args):
    """Edit the given patch or the current one."""
    stack = directory.repository.current_stack

    if len(args) == 0:
        if not stack.patchorder.applied:
            raise CmdException('Cannot edit top patch because no patches are applied')
        patchname = stack.patchorder.applied[-1]
    elif len(args) == 1:
        [patchname] = args
        if patchname not in stack.patches:
            raise CmdException('%s: no such patch' % patchname)
    else:
        parser.error('Cannot edit more than one patch')

    cd = orig_cd = stack.patches[patchname].data

    if options.set_tree:
        cd = cd.set_tree(
            stack.repository.rev_parse(
                options.set_tree, discard_stderr=True, object_type='tree'
            )
        )

    cd = edit.auto_edit_patch(
        stack.repository,
        cd,
        msg=(
            None
            if options.message is None
            else options.message.encode(config.get('i18n.commitencoding'))
        ),
        author=options.author,
        sign_str=options.sign_str,
    )

    if options.save_template:
        options.save_template(
            edit.get_patch_description(
                stack.repository, cd, patchname, options.diff, options.diff_flags
            )
        )
        return utils.STGIT_SUCCESS

    use_editor = cd == orig_cd or options.edit or options.diff
    if use_editor:
        cd, new_patchname, failed_diff = edit.interactive_edit_patch(
            stack.repository, cd, patchname, options.diff, options.diff_flags
        )
        # If we couldn't apply the patch, fail without even trying to
        # affect any of the changes.
        if failed_diff is not None:
            return utils.STGIT_COMMAND_ERROR
    else:
        new_patchname = None

    if not options.no_verify and (use_editor or cd.message != orig_cd.message):
        try:
            cd = run_commit_msg_hook(stack.repository, cd, use_editor)
        except Exception:
            if options.diff:
                patch_desc = edit.get_patch_description(
                    stack.repository, cd, patchname, options.diff, options.diff_flags
                )
                edit.note_patch_application_failure(
                    patch_desc, 'The commit-msg hook failed.'
                )
            raise

    retval, _ = edit.perform_edit(
        stack,
        cd,
        patchname,
        new_patchname,
        options.diff,
        options.diff_flags,
        options.set_tree,
    )
    return retval
