# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import io
import os
import sys

from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepositoryLib,
    apply_patch,
    check_conflicts,
    check_head_top_equal,
    check_local_changes,
    git_commit,
)
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

help = 'Integrate a GNU diff patch into the current patch'
kind = 'patch'
usage = ['[options] [--] [<file>]']
description = """
Apply the given GNU diff file (or the standard input) onto the top of
the current patch. With the '--threeway' option, the patch is applied
onto the bottom of the current patch and a three-way merge is
performed with the current top. With the --base option, the patch is
applied onto the specified base and a three-way merged is performed
with the current top."""

args = ['files']
options = [
    opt(
        '-t',
        '--threeway',
        action='store_true',
        short='Perform a three-way merge with the current patch',
    ),
    opt(
        '-b',
        '--base',
        args=['commit'],
        short='Use BASE instead of HEAD when applying the patch',
    ),
    opt(
        '-p',
        '--strip',
        type='int',
        metavar='N',
        short='Remove N leading slashes from diff paths (default 1)',
    ),
    opt(
        '--reject',
        action='store_true',
        short='Leave the rejected hunks in corresponding *.rej files',
    ),
]

directory = DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Integrate a GNU diff patch into the current patch
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    repository = directory.repository
    stack = repository.get_stack()

    check_local_changes(repository)
    check_conflicts(repository.default_iw)
    check_head_top_equal(stack)

    if len(args) == 1:
        filename = args[0]
    else:
        filename = None

    applied = stack.patchorder.applied
    if not applied:
        raise CmdException('No patches applied')

    current = applied[-1]

    if filename:
        if os.path.exists(filename):
            out.start('Folding patch "%s"' % filename)
            with io.open(filename, 'rb') as f:
                diff = f.read()
        else:
            raise CmdException('No such file: %s' % filename)
    else:
        out.start('Folding patch from stdin')
        if hasattr(sys.stdin, 'buffer'):
            diff = sys.stdin.buffer.read()
        else:
            diff = sys.stdin.read()

    if options.threeway:
        top_patch = stack.patches.get(current)
        apply_patch(
            stack,
            diff,
            base=top_patch.commit.data.parent,
            strip=options.strip,
            reject=options.reject,
        )
    elif options.base:
        apply_patch(
            stack,
            diff,
            base=git_commit(options.base, repository),
            reject=options.reject,
            strip=options.strip,
        )
    else:
        apply_patch(
            stack,
            diff,
            strip=options.strip,
            reject=options.reject,
        )

    out.done()
