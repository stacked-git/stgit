
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


help = 'show the tree diff'
usage = """%prog [options] [<files...>]

The revision format is '([patch]/[bottom | top]) | <tree-ish>'"""

options = [make_option('-r', metavar = 'rev1[:[rev2]]', dest = 'revs',
                       help = 'show the diff between revisions'),
           make_option('-s', '--stat',
                       help = 'show the stat instead of the diff',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the tree diff
    """
    if options.revs:
        rev_list = options.revs.split(':')
        rev_list_len = len(rev_list)
        if rev_list_len == 1:
            if rev_list[0][-1] == '/':
                # the whole patch
                rev1 = rev_list[0] + 'bottom'
                rev2 = rev_list[0] + 'top'
            else:
                rev1 = rev_list[0]
                rev2 = None
        elif rev_list_len == 2:
            rev1 = rev_list[0]
            rev2 = rev_list[1]
            if rev2 == '':
                rev2 = 'HEAD'
        else:
            parser.error('incorrect parameters to -r')
    else:
        rev1 = 'HEAD'
        rev2 = None

    if options.stat:
        print git.diffstat(args, git_id(rev1), git_id(rev2))
    else:
        git.diff(args, git_id(rev1), git_id(rev2))
