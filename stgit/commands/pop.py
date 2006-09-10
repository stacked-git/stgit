
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


help = 'pop one or more patches from the stack'
usage = """%prog [options] [<patch>]

Pop the topmost patch or a range of patches starting with the topmost
one from the stack. The command fails if there are local changes or
conflicts. If a patch name is given as argument, the command will pop
all the patches up to the given one."""

options = [make_option('-a', '--all',
                       help = 'pop all the applied patches',
                       action = 'store_true'),
           make_option('-n', '--number', type = 'int',
                       help = 'pop the specified number of patches'),
           make_option('--keep',
                       help = 'keep the current working directory',
                       action = 'store_true')]


def func(parser, options, args):
    """Pop the topmost patch from the stack
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    if not options.keep:
        check_local_changes()
        check_conflicts()
        check_head_top_equal()

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'
    # the popping is done in reverse order
    applied.reverse()

    if options.all:
        patches = applied
    elif options.number:
        patches = applied[:options.number]
    elif len(args) == 1:
        upto_patch = args[0]
        if upto_patch not in applied:
            if upto_patch in crt_series.get_unapplied():
                raise CmdException, 'Patch "%s" is not currently applied' \
                      % upto_patch
            else:
                raise CmdException, 'Patch "%s" does not exist' % upto_patch
        patches = applied[:applied.index(upto_patch)]
    else:
        patches = [applied[0]]

    if patches == []:
        raise CmdException, 'No patches to pop'

    pop_patches(patches, options.keep)

    print_crt_patch()
