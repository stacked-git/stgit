
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


help = 'remove files from the repository'
usage = """%prog [options] <files...>

Remove given files from the repository. The command doesn't remove the
working copy of the file."""

options = [make_option('-f', '--force',
                       help = 'force removing even if the file exists',
                       action = 'store_true')]


def func(parser, options, args):
    """Remove files from the repository
    """
    if len(args) < 1:
        parser.error('incorrect number of arguments')

    if not crt_series.get_current():
        raise CmdException, 'No patches applied'

    git.rm(args, options.force)
