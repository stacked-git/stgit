# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit import argparse
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepositoryLib,
    color_diff_flags,
)
from stgit.out import out
from stgit.pager import pager

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

help = 'Show the applied patches modifying a file'
kind = 'stack'
usage = ['[options] [--] [<files or dirs>]']
description = """
Show the applied patches modifying the given files. Without arguments,
it shows the patches affected by the local tree modifications. The
'--diff' option also lists the patch log and the diff for the given
files."""

args = ['known_files']
options = [
    opt(
        '-d',
        '--diff',
        action='store_true',
        short='Show the diff for the given files',
    ),
    opt(
        '-b',
        '--branch',
        args=['stg_branches'],
        short='Use BRANCH instead of the default branch',
    ),
] + argparse.diff_opts_option()

directory = DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Show the patches modifying a file."""
    repository = directory.repository
    stack = repository.get_stack(options.branch)

    if not stack.patchorder.applied:
        raise CmdException('No patches applied')

    iw = repository.default_iw

    if not args:
        files = iw.changed_files(stack.head.data.tree)
    else:
        files = iw.ls_files(stack.head.data.tree, args)

    if not files:
        raise CmdException('No files specified or no local changes')

    directory.cd_to_topdir()

    # Find set of revisions that modify the selected files.
    revs = set(
        repository.run(
            [
                'git',
                'rev-list',
                '--stdin',
                stack.base.sha1 + '..' + stack.top.sha1,
            ]
        )
        .raw_input('--\n' + '\n'.join(files))
        .output_lines()
    )

    diff_lines = []
    for name in stack.patchorder.applied:
        patch = stack.patches.get(name)
        if patch.commit.sha1 not in revs:
            continue
        if options.diff:
            diff_lines.extend(
                [
                    b'-' * 79,
                    patch.name.encode('utf-8'),
                    b'-' * 79,
                    patch.commit.data.message.encode('utf-8'),
                    b'---',
                    b'',
                    repository.diff_tree(
                        patch.commit.data.parent.data.tree,
                        patch.commit.data.tree,
                        pathlimits=files,
                        diff_opts=options.diff_flags + color_diff_flags(),
                    )
                ]
            )
        else:
            out.stdout(patch.name)

    if options.diff:
        pager(b'\n'.join(diff_lines))
