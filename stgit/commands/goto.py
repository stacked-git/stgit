__copyright__ = """
Copyright (C) 2006, Catalin Marinas <catalin.marinas@gmail.com>

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


help = 'push or pop patches to the given one'
usage = """%prog [options] <name>

Push/pop patches to/from the stack until the one given on the command
line becomes current. This is a shortcut for the 'push --to' or 'pop
--to' commands. There is no '--undo' option for 'goto'. Use the 'push'
command for this."""

options = [make_option('-k', '--keep',
                       help = 'keep the local changes when popping patches',
                       action = 'store_true')]


def func(parser, options, args):
    """Pushes the given patch or all onto the series
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    check_conflicts()
    check_head_top_equal()

    if not options.keep:
        check_local_changes()

    applied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()
    patch = args[0]

    if patch in applied:
        applied.reverse()
        patches = applied[:applied.index(patch)]
        pop_patches(patches, options.keep)
    elif patch in unapplied:
        if options.keep:
            raise CmdException, 'Cannot use --keep with patch pushing'
        patches = unapplied[:unapplied.index(patch)+1]
        push_patches(patches)
    else:
        raise CmdException, 'Patch "%s" does not exist' % patch

    print_crt_patch()
