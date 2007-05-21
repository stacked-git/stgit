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


help = 'print the GIT hash value of a StGIT reference'
usage = """%prog [options] [id]

Print the hash value of a GIT id (defaulting to HEAD). In addition to
the standard GIT id's like heads and tags, this command also accepts
'base[@<branch>]' and '[<patch>[@<branch>]][//[bottom | top]]'. If no
'top' or 'bottom' are passed and <patch> is a valid patch name, 'top'
will be used by default."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]


def func(parser, options, args):
    """Show the applied patches
    """
    if len(args) == 0:
        id_str = 'HEAD'
    elif len(args) == 1:
        id_str = args[0]
    else:
        parser.error('incorrect number of arguments')

    out.stdout(git_id(id_str))
