
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
from stgit.lib import git as gitlib

help = 'Show the files modified by a patch (or the current patch)'
kind = 'patch'
usage = ['[options] [--] [[<branch>:]<patch>]']
description = """
List the files modified by the given patch (defaulting to the current
one). Passing the '--stat' option shows the diff statistics for the
given patch. Note that this command doesn't show the files modified in
the working tree and not yet included in the patch by a 'refresh'
command. Use the 'diff' or 'status' commands for these files."""

args = [argparse.applied_patches, argparse.unapplied_patches,
        argparse.hidden_patches]
options = [
    opt('-s', '--stat', action = 'store_true',
        short = 'Show the diffstat'),
    opt('--bare', action = 'store_true',
        short = 'Bare file names (useful for scripting)'),
    ] + argparse.diff_opts_option()

directory = DirectoryHasRepository(log = False)

def func(parser, options, args):
    """Show the files modified by a patch (or the current patch)
    """
    if len(args) == 0:
        patch = 'HEAD'
    elif len(args) == 1:
        patch = args[0]
    else:
        parser.error('incorrect number of arguments')

    rev1 = git_id(crt_series, '%s^' % patch)
    rev2 = git_id(crt_series, '%s' % patch)

    if options.stat:
        output = gitlib.diffstat(git.diff(rev1 = rev1, rev2 = rev2,
                                          diff_flags = options.diff_flags))
    elif options.bare:
        output = git.barefiles(rev1, rev2)
    else:
        output = git.files(rev1, rev2, diff_flags = options.diff_flags)
    if output:
        if not output.endswith('\n'):
            output += '\n'
        out.stdout_raw(output)
