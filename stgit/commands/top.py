
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


help = 'print the name of the top patch'
usage = """%prog [options]

Print the name of the current (topmost) patch."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]


def func(parser, options, args):
    """Show the name of the topmost patch
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    name = crt_series.get_current()
    if name:
        out.stdout(name)
    else:
        raise CmdException, 'No patches applied'
