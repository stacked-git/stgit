# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import io
import re
import sys

from stgit.argparse import keep_option, opt, patch_range
from stgit.commands import common
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
usage = ['[--] <patches>',
         '-s <series>']
description = """
Push a patch or a range of patches to the top even if applied. The
necessary pop and push operations will be performed to accomplish
this. The '--series' option can be used to rearrange the (top) patches
as specified by the given series file (or the standard input)."""

args = [patch_range('applied_patches', 'unapplied_patches')]
options = [
    opt(
        '-s',
        '--series',
        metavar='FILE',
        short='Rearrange according to the series FILE',
    )
] + keep_option()

directory = common.DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Reorder patches to make the named patch the topmost one.
    """
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
    else:
        patches = common.parse_patches(args, stack.patchorder.all)

    if not patches:
        raise common.CmdException('No patches to float')

    applied = [
        p for p in stack.patchorder.applied if p not in patches
    ] + patches
    unapplied = [p for p in stack.patchorder.unapplied if p not in patches]

    iw = stack.repository.default_iw
    clean_iw = (not options.keep and iw) or None
    trans = transaction.StackTransaction(
        stack, 'float', check_clean_iw=clean_iw
    )

    try:
        trans.reorder_patches(applied, unapplied, iw=iw)
    except transaction.TransactionHalted:
        pass
    return trans.run(iw)
