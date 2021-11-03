from stgit.argparse import opt, patch_range
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    check_head_top_equal,
    parse_patches,
)
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

args = [patch_range('hidden_patches')]
options = [
    opt(
        '-b',
        '--branch',
        args=['stg_branches'],
        short='Use BRANCH instead of the default branch',
    )
]

directory = DirectoryHasRepository()


def func(parser, options, args):
    """Unhide a range of patch in the series."""
    stack = directory.repository.current_stack
    check_head_top_equal(stack)
    trans = transaction.StackTransaction(stack)

    if not args:
        parser.error('No patches specified')

    patches = parse_patches(args, trans.all_patches)
    for p in patches:
        if p not in trans.hidden:
            raise CmdException('Patch "%s" not hidden' % p)

    applied = list(trans.applied)
    unapplied = trans.unapplied + patches
    hidden = [p for p in trans.hidden if p not in set(patches)]

    trans.reorder_patches(applied, unapplied, hidden)
    return trans.execute('unhide')
