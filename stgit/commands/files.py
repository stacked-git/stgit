
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
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'show the files modified by a patch (or the current patch)'
usage = """%prog [options] [<patch>]

List the files modified by the given patch (defaulting to the current
one). Passing the '--stat' option shows the diff statistics for the
given patch. Note that this command doesn't show the files modified in
the working tree and not yet included in the patch by a 'refresh'
command. Use the 'diff' or 'status' commands for these files."""

options = [make_option('-s', '--stat',
                       help = 'show the diff stat',
                       action = 'store_true'),
           make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-O', '--diff-opts',
                       help = 'options to pass to git-diff'),
           make_option('--bare',
                       help = 'bare file names (useful for scripting)',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the files modified by a patch (or the current patch)
    """
    if len(args) == 0:
        patch = ''
    elif len(args) == 1:
        patch = args[0]
    else:
        parser.error('incorrect number of arguments')

    rev1 = git_id('%s//bottom' % patch)
    rev2 = git_id('%s//top' % patch)

    if options.stat:
        out.stdout_raw(git.diffstat(rev1 = rev1, rev2 = rev2) + '\n')
    elif options.bare:
        out.stdout_raw(git.barefiles(rev1, rev2) + '\n')
    else:
        if options.diff_opts:
            diff_flags = options.diff_opts.split()
        else:
            diff_flags = []

        out.stdout_raw(git.files(rev1, rev2, diff_flags = diff_flags) + '\n')
