
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


help = 'print the applied patches'
usage = """%prog [options]

List the patches from the series which were already pushed onto the
stack.  They are listed in the order in which they were pushed, the
last one being the current (topmost) patch."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-c', '--count',
                       help = 'print the number of applied patches',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the applied patches
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    applied = crt_series.get_applied()

    if options.count:
        out.stdout(len(applied))
    else:
        for p in applied:
            out.stdout(p)
