
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


help = 'initialise the tree for use with StGIT'
usage = """%prog [options]

Initialise a GIT repository to be used with StGIT. Note that the
repository must be already initialised with git-init-db and the
.git/HEAD link must point to a valid file in refs/heads/. For people
switching between multiple branches in the same repository, the 'init'
command has to be run on all the individual branches intended to be
used with StGIT."""

options = []


def func(parser, options, args):
    """Performs the repository initialisation
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    crt_series.init()
