
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


help = 'show the tree status'
usage = '%prog [options] [<files...>]'

options = [make_option('-m', '--modified',
                       help = 'show modified files only',
                       action = 'store_true'),
           make_option('-n', '--new',
                       help = 'show new files only',
                       action = 'store_true'),
           make_option('-d', '--deleted',
                       help = 'show deleted files only',
                       action = 'store_true'),
           make_option('-c', '--conflict',
                       help = 'show conflict files only',
                       action = 'store_true'),
           make_option('-u', '--unknown',
                       help = 'show unknown files only',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the tree status
    """
    git.status(args, options.modified, options.new, options.deleted,
               options.conflict, options.unknown)
