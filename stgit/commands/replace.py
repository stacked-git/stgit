"""Patch editing command"""

import io
import os
import sys

from stgit import argparse, utils
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    apply_transaction_edits,
    run_commit_msg_hook,
    split_descr_diff,
)
from stgit.lib import edit
from stgit.out import out

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

help = 'Replace patch description or diff'
kind = 'patch'
usage = ['[options] [--] [<file>]']
description = """
"""

args = ['files']
options = (
    [
        opt('-e', '--edit', action='store_true', short='Invoke interactive editor'),
        opt(
            '--import-description',
            action='store_true',
            short='Import description from the diff, if there is one.',
        ),
        opt(
            '-p',
            '--patch',
            args=['other_applied_patches', 'unapplied_patches', 'hidden_patches'],
            short='Replace PATCH instead of the top patch',
        ),
    ]
    + argparse.hook_options()
    + argparse.sign_options()
    + argparse.author_options()
    + argparse.diff_opts_option()
)

directory = DirectoryHasRepository()


def __read_patch(args):
    filename = None
    if len(args) == 1:
        filename = args[0]

    if filename:
        if os.path.exists(filename):
            out.start('Replacing with patch "%s"' % filename)
            with io.open(filename, 'rb') as f:
                diff = f.read()
        else:
            raise CmdException('No such file: %s' % filename)
    else:
        out.start('Replacing with patch from stdin')
        diff = sys.stdin.buffer.read()
    return diff


def __get_patchname(stack, given_patch):
    """Get the name of the patch we are to refresh."""
    if given_patch:
        patch_name = given_patch
        if not stack.patches.exists(patch_name):
            raise CmdException('%s: no such patch' % patch_name)
        return patch_name
    else:
        if not stack.patchorder.applied:
            raise CmdException(
                'Cannot refresh top patch because no patches are applied'
            )
        return stack.patchorder.applied[-1]


def __replace_diff(
    stack, cd, diff, diff_flags, author=None, sign_str=None, import_description=False
):
    # GOAL: Parse the message and changes out of the difftext.
    #  Modify changes of CD.
    #  (if requested) Overwrite the message of CD with one from the difftext.

    (diff_descr, diff_data) = split_descr_diff(diff)

    # Check if the diff contains description:
    if import_description and len(diff_descr) > 0:
        msg = diff
    else:
        msg = edit.patch_desc(
            repo=stack.repository,
            cd=cd,
            append_diff=True,
            diff_flags=diff_flags,
            replacement_diff=diff_data,
        )

    return edit.auto_edit_patch(
        repo=stack.repository,
        cd=cd,
        msg=msg,
        author=author,
        sign_str=sign_str,
        contains_diff=True,
    )


def func(parser, options, args):
    """Edit the given patch or the current one."""
    stack = directory.repository.current_stack

    diff = __read_patch(args)
    patchname = __get_patchname(stack, options.patch)
    replace_description = options.import_description

    cd = orig_cd = stack.patches.get(patchname).commit.data
    cd = __replace_diff(
        stack=stack,
        cd=cd,
        diff=diff,
        diff_flags=options.diff_flags,
        author=options.author,
        sign_str=options.sign_str,
        import_description=replace_description,
    )

    use_editor = cd == orig_cd or options.edit
    if use_editor:
        cd, failed_diff = edit.interactive_edit_patch(
            repo=stack.repository, cd=cd, edit_diff=True, diff_flags=options.diff_flags
        )
    else:
        failed_diff = None

    def failed(reason='Edited patch did not apply.'):
        fn = '.stgit-failed.patch'
        with io.open(fn, 'wb') as f:
            f.write(
                edit.patch_desc(
                    repo=stack.repository,
                    cd=cd,
                    append_diff=True,
                    diff_flags=options.diff_flags,
                    replacement_diff=failed_diff,
                )
            )
        out.error(reason, 'The patch has been saved to "%s".' % fn)
        return utils.STGIT_COMMAND_ERROR

    # If we couldn't apply the patch, fail without even trying to
    # effect any of the changes.
    if failed_diff:
        return failed()

    if not options.no_verify and (use_editor or cd.message != orig_cd.message):
        try:
            cd = run_commit_msg_hook(stack.repository, cd, use_editor)
        except Exception:
            if options.diff:
                failed('The commit-msg hook failed.')
            raise

    # Refresh the committer information
    cd = cd.set_committer(None)

    return apply_transaction_edits(
        stack=stack,
        iw=stack.repository.default_iw,
        cd=cd,
        log_msg="replace",
        patchname=patchname,
        on_fail_cb=failed,
    )
