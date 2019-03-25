# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.argparse import diff_opts_option, opt, patch_range
from stgit.commands.common import (
    DirectoryHasRepositoryLib,
    color_diff_flags,
    git_commit,
)
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

help = 'Show the tree diff'
kind = 'wc'
usage = ['[options] [--] [<files or dirs>]']
description = """
Show the diff (default) or diffstat between the current working copy
or a tree-ish object and another tree-ish object (defaulting to HEAD).
File names can also be given to restrict the diff output. The
tree-ish object has the format accepted by the linkstg:id[] command."""

args = ['known_files', 'dirty_files']
options = [
    opt(
        '-r',
        '--range',
        metavar='rev1[..[rev2]]',
        dest='revs',
        args=[
            patch_range(
                'applied_patches',
                'unapplied_patches',
                'hidden_patches',
            )
        ],
        short='Show the diff between revisions',
    ),
    opt(
        '-s',
        '--stat',
        action='store_true',
        short='Show the stat instead of the diff',
    ),
] + diff_opts_option()

directory = DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Show the tree diff"""
    repository = directory.repository

    if options.revs:
        rev_list = options.revs.split('..')
        if len(rev_list) not in [1, 2] or not rev_list[0]:
            parser.error('incorrect parameters to -r')
        elif len(rev_list) == 1:
            rev1 = git_commit(rev_list[0], repository)
            rev2 = None
        else:
            rev1 = git_commit(rev_list[0], repository)
            if rev_list[1]:
                rev2 = git_commit(rev_list[1], repository)
            else:
                rev2 = None
    else:
        rev1 = repository.rev_parse('HEAD')
        rev2 = None

    iw = repository.default_iw

    files = iw.ls_files(rev1.data.tree, args)

    diff_opts = color_diff_flags()
    diff_opts.extend(options.diff_flags)

    if rev1 and rev2:
        diff = repository.diff_tree(
            rev1.data.tree,
            rev2.data.tree,
            diff_opts=diff_opts,
            pathlimits=files,
            stat=options.stat,
            binary=False,
        )
    else:
        diff = iw.diff(
            rev1.data.tree,
            diff_opts=diff_opts,
            pathlimits=files,
            stat=options.stat,
            binary=False,
        )

    pager(diff)
