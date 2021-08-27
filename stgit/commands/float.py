import io
import re
import sys

from stgit.argparse import keep_option, opt, patch_range
from stgit.commands.common import CmdException, DirectoryHasRepository, parse_patches
from stgit.lib import transaction

__copyright__ = """
Copyright (C) 2006, Robin Rosenberg <robin.rosenberg@dewire.com>
Modified by Catalin Marinas

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

help = 'Push patches to the top, even if applied'
kind = 'stack'
usage = ['[--] <patches>', '-s <series>']
description = """
Float a patch or range of patches to be the top-most applied patches.
The patches to be floated may currently be either applied or unapplied.
The necessary pop and push operations will be performed to float the
named patches.  Patches not specified will remain applied or unapplied
as they were prior to the float operation."""

args = [patch_range('applied_patches', 'unapplied_patches')]
options = [
    opt(
        '--noapply',
        action='store_true',
        short='Reorder patches by floating without applying.',
    ),
    opt(
        '-s',
        '--series',
        metavar='FILE',
        short='Rearrange according to the series FILE',
    ),
]
options.extend(keep_option())

directory = DirectoryHasRepository()


def func(parser, options, args):
    """Reorder patches to make the named patch the topmost one."""
    if options.series and args:
        parser.error('<patches> cannot be used with --series')
    elif not options.series and not args:
        parser.error('incorrect number of arguments')

    stack = directory.repository.current_stack

    if options.series:
        if options.series == '-':
            f = io.open(sys.stdin.fileno())
        else:
            f = io.open(options.series)

        patches = []
        for line in f:
            patch = re.sub('#.*$', '', line).strip()
            if patch:
                patches.append(patch)
        patches = parse_patches(patches, stack.patchorder.all)
    else:
        patches = parse_patches(args, stack.patchorder.all)

    if not patches:
        raise CmdException('No patches to float')

    iw = stack.repository.default_iw
    if options.keep or (
        options.noapply and not any(pn in stack.patchorder.applied for pn in patches)
    ):
        clean_iw = None
    else:
        clean_iw = iw
    trans = transaction.StackTransaction(stack, 'float', check_clean_iw=clean_iw)

    if options.noapply:
        applied = [p for p in trans.applied if p not in patches]
        unapplied = patches + [p for p in trans.unapplied if p not in patches]
    else:
        applied = [p for p in trans.applied if p not in patches] + patches
        unapplied = [p for p in trans.unapplied if p not in patches]

    try:
        trans.reorder_patches(applied, unapplied, iw=iw)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)
