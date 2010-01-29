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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

import sys, os
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import argparse, stack, git

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

args = [argparse.files]
options = [
    opt('-t', '--threeway', action = 'store_true',
        short = 'Perform a three-way merge with the current patch'),
    opt('-b', '--base', args = [argparse.commit],
        short = 'Use BASE instead of HEAD when applying the patch'),
    opt('-p', '--strip', type = 'int', metavar = 'N',
        short = 'Remove N leading slashes from diff paths (default 1)'),
    opt('--reject', action = 'store_true',
        short = 'Leave the rejected hunks in corresponding *.rej files')]

directory = DirectoryHasRepository(log = True)

def func(parser, options, args):
    """Integrate a GNU diff patch into the current patch
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

    if len(args) == 1:
        filename = args[0]
    else:
        filename = None

    current = crt_series.get_current()
    if not current:
        raise CmdException, 'No patches applied'

    if filename:
        if os.path.exists(filename):
            out.start('Folding patch "%s"' % filename)
        else:
            raise CmdException, 'No such file: %s' % filename
    else:
        out.start('Folding patch from stdin')

    if options.threeway:
        crt_patch = crt_series.get_patch(current)
        bottom = crt_patch.get_bottom()
        git.apply_patch(filename = filename, base = bottom,
                        strip = options.strip, reject = options.reject)
    elif options.base:
        git.apply_patch(filename = filename, reject = options.reject,
                        strip = options.strip,
                        base = git_id(crt_series, options.base))
    else:
        git.apply_patch(filename = filename, strip = options.strip,
                        reject = options.reject)

    out.done()
