
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


help = 'print the patch series'
usage = """%prog [options]

Show all the patches in the series. The applied patches are prefixed
with a '+' and the unapplied ones with a '-'. The current patch is
prefixed with a '>'. Empty patches are prefixed with a '0'."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-c', '--count',
                       help = 'print the number of patches in the series',
                       action = 'store_true'),
           make_option('-d', '--description',
                       help = 'show a short description for each patch',
                       action = 'store_true'),
           make_option('-e', '--empty',
                       help = 'check whether patches are empty '
                       '(much slower)',
                       action = 'store_true'),
           make_option('-s', '--short',
                       help = 'list just the patches around the topmost patch',
                       action = 'store_true'),
           make_option('-g', '--graphical',
                       help = 'run gitk instead of printing',
                       action = 'store_true')]


def __get_description(patch):
    """Extract and return a patch's short description
    """
    p = crt_series.get_patch(patch)
    descr = (p.get_description() or '').strip()
    descr_lines = descr.split('\n')
    return descr_lines[0].rstrip()

def __print_patch(patch, prefix, empty_prefix, length, options):
    if options.empty and crt_series.empty_patch(patch):
        prefix = empty_prefix
    if options.description:
        print prefix + patch.ljust(length) + '  | ' + __get_description(patch)
    else:
        print prefix + patch

def func(parser, options, args):
    """Show the patch series
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    applied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()

    if options.count:
        print len(applied) + len(unapplied)
        return

    if options.short:
        if len(applied) > 5:
            applied = applied[-6:]
        if len(unapplied) > 5:
            unapplied = unapplied[:5]

    patches = applied + unapplied
    if not patches:
        return

    if options.graphical:
        if applied:
            gitk_args = ' %s^..%s' % (git_id(applied[0]), git_id(applied[-1]))
        else:
            gitk_args = ''

        for p in unapplied:
            patch_id = git_id(p)
            gitk_args += ' %s^..%s' % (patch_id, patch_id)

        if os.system('gitk%s' % gitk_args) != 0:
            raise CmdException, 'gitk execution failed'
    else:
        max_len = 0
        if len(patches) > 0:
            max_len = max([len(i) for i in patches])

        if len(applied) > 0:
            for p in applied [0:-1]:
                __print_patch(p, '+ ', '0 ', max_len, options)

            __print_patch(applied[-1], '> ', '0>', max_len, options)

        for p in unapplied:
            __print_patch(p, '- ', '0 ', max_len, options)
