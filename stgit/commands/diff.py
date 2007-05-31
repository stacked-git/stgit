
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
from pydoc import pager

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'show the tree diff'
usage = """%prog [options] [<files...>]

Show the diff (default) or diffstat between the current working copy
or a tree-ish object and another tree-ish object. File names can also
be given to restrict the diff output. The tree-ish object can be a
standard git commit, tag or tree. In addition to these, the command
also supports 'base', representing the bottom of the current stack,
and '[patch][//[bottom | top]]' for the patch boundaries (defaulting to
the current one):

rev = '([patch][//[bottom | top]]) | <tree-ish> | base'

If neither bottom nor top are given but a '//' is present, the command
shows the specified patch (defaulting to the current one)."""

options = [make_option('-r', '--range',
                       metavar = 'rev1[..[rev2]]', dest = 'revs',
                       help = 'show the diff between revisions'),
           make_option('-O', '--diff-opts',
                       help = 'options to pass to git-diff'),
           make_option('-s', '--stat',
                       help = 'show the stat instead of the diff',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the tree diff
    """
    if options.revs:
        rev_list = options.revs.split('..')
        rev_list_len = len(rev_list)
        if rev_list_len == 1:
            rev = rev_list[0]
            if rev.endswith('/'):
                # the whole patch
                rev = strip_suffix('/', rev)
                if rev.endswith('/'):
                    rev = strip_suffix('/', rev)
                rev1 = rev + '//bottom'
                rev2 = rev + '//top'
            else:
                rev1 = rev_list[0]
                rev2 = None
        elif rev_list_len == 2:
            rev1 = rev_list[0]
            rev2 = rev_list[1]
        else:
            parser.error('incorrect parameters to -r')
    else:
        rev1 = 'HEAD'
        rev2 = None

    if options.diff_opts:
        diff_flags = options.diff_opts.split()
    else:
        diff_flags = []

    if options.stat:
        out.stdout_raw(git.diffstat(args, git_id(rev1), git_id(rev2)) + '\n')
    else:
        diff_str = git.diff(args, git_id(rev1), git_id(rev2),
                            diff_flags = diff_flags )
        if diff_str:
            pager(diff_str)
