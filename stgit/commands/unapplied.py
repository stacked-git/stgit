
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

from optparse import make_option
from stgit.out import *
from stgit.commands import common


help = 'print the unapplied patches'
usage = """%prog [options]

List the patches from the series which are not pushed onto the stack.
They are listed in the reverse order in which they were popped."""

directory = common.DirectoryHasRepositoryLib()
options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default branch'),
           make_option('-c', '--count',
                       help = 'print the number of unapplied patches',
                       action = 'store_true')]


def func(parser, options, args):
    """Show the unapplied patches
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    if options.branch:
        s = directory.repository.get_stack(options.branch)
    else:
        s = directory.repository.current_stack

    if options.count:
        out.stdout(len(s.patchorder.unapplied))
    else:
        for pn in s.patchorder.unapplied:
            out.stdout(pn)
