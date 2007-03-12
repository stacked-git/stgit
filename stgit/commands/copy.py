
__copyright__ = """
Copyright (C) 2007, Yann Dirson <ydirson@altern.org>

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


help = 'copy files inside the repository'
usage = """%prog [options] [<file/dir> <newname> | <files/dirs...> <dir>]

Copy of the files and dirs passed as arguments under another name or
location inside the same repository."""

options = []

def func(parser, options, args):
    """Copy files inside the repository
    """
    if len(args) < 1:
        parser.error('incorrect number of arguments')

    if not crt_series.get_current():
        raise CmdException, 'No patches applied'

    git.copy(args[0:-1], args[-1])
