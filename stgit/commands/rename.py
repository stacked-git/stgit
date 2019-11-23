# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.argparse import opt
from stgit.commands.common import CmdException, DirectoryHasRepositoryLib
from stgit.lib.log import log_entry
from stgit.out import out

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

help = 'Rename a patch'
kind = 'patch'
usage = ['[options] [--] [oldpatch] <newpatch>']
description = """
Rename <oldpatch> into <newpatch> in a series. If <oldpatch> is not
given, the top-most patch will be renamed."""

args = ['applied_patches', 'unapplied_patches', 'hidden_patches']
options = [
    opt(
        '-b',
        '--branch',
        args=['stg_branches'],
        short='use BRANCH instead of the default one',
    )
]

directory = DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Rename a patch in the series"""
    stack = directory.repository.get_stack(options.branch)

    if len(args) == 2:
        old, new = args
    elif len(args) == 1:
        if not stack.patchorder.applied:
            raise CmdException("No applied top patch to rename exists.")
        old = stack.patchorder.applied[-1]
        new = args[0]
    else:
        parser.error('incorrect number of arguments')

    out.start('Renaming patch "%s" to "%s"' % (old, new))
    stack.rename_patch(old, new)
    log_entry(stack, 'rename %s to %s' % (old, new))
    out.done()
