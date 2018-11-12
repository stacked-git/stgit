# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from stgit import argparse
from stgit.argparse import opt
from stgit.commands import common
from stgit.lib import transaction

__copyright__ = """
Copyright (C) 2009, Catalin Marinas <catalin.marinas@gmail.com>

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

help = 'Unhide a hidden patch'
kind = 'stack'
usage = ['[options] [--] <patch-range>']
description = """
Unhide a hidden range of patches so that they are shown in the plain
'stg series' command output."""

args = [argparse.patch_range(argparse.hidden_patches)]
options = [
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch')]

directory = common.DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Unhide a range of patch in the series."""
    stack = directory.repository.current_stack
    trans = transaction.StackTransaction(stack, 'unhide')

    if not args:
        parser.error('No patches specified')

    patches = common.parse_patches(args, trans.all_patches)
    for p in patches:
        if p not in trans.hidden:
            raise common.CmdException('Patch "%s" not hidden' % p)

    applied = list(trans.applied)
    unapplied = trans.unapplied + patches
    hidden = [p for p in trans.hidden if p not in set(patches)]

    trans.reorder_patches(applied, unapplied, hidden)
    return trans.run()
