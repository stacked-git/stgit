
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

import stgit.commands.common
from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'print the patch series'
usage = """%prog [options] [<patch-range>]

Show all the patches in the series or just those in the given
range. The applied patches are prefixed with a '+' and the unapplied
ones with a '-'. The current patch is prefixed with a '>'. Empty
patches are prefixed with a '0'."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-m', '--missing', metavar = 'BRANCH',
                       help = 'show patches in BRANCH missing in current'),
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
           make_option('--showbranch',
                       help = 'append the branch name to the listed patches',
                       action = 'store_true'),
           make_option('--noprefix',
                       help = 'do not show the patch status prefix',
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

def __print_patch(patch, branch_str, prefix, empty_prefix, length, options):
    if options.noprefix:
        prefix = ''
    elif options.empty and crt_series.empty_patch(patch):
        prefix = empty_prefix

    patch_str = patch + branch_str

    if options.description:
        print prefix + patch_str.ljust(length) + '  | ' \
              + __get_description(patch)
    else:
        print prefix + patch_str

def func(parser, options, args):
    """Show the patch series
    """
    global crt_series

    if options.missing:
        # switch the series, the one specified with --missing should
        # become the current
        cmp_series = crt_series
        crt_series = stack.Series(options.missing)
        stgit.commands.common.crt_series = crt_series

        cmp_patches = cmp_series.get_applied() + cmp_series.get_unapplied()
    else:
        cmp_patches = []

    applied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()

    # the filtering range covers the whole series
    if args:
        show_patches = parse_patches(args, applied + unapplied, len(applied))
    else:
        show_patches = applied + unapplied

    # filter the patches
    applied = [p for p in applied
               if p in show_patches and p not in cmp_patches]
    unapplied = [p for p in unapplied
                 if p in show_patches and p not in cmp_patches]

    if options.short:
        if len(applied) > 5:
            applied = applied[-6:]
        if len(unapplied) > 5:
            unapplied = unapplied[:5]

    patches = applied + unapplied

    if options.count:
        print len(patches)
        return

    if not patches:
        return

    if options.showbranch:
        branch_str = '@' + crt_series.get_branch()
    else:
        branch_str = ''

    if options.graphical:
        if options.missing:
            raise CmdException, '--graphical not supported with --missing'

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
            max_len = max([len(i + branch_str) for i in patches])

        if len(applied) > 0:
            for p in applied [0:-1]:
                __print_patch(p, branch_str, '+ ', '0 ', max_len, options)

            __print_patch(applied[-1], branch_str, '> ', '0>', max_len,
                          options)

        for p in unapplied:
            __print_patch(p, branch_str, '- ', '0 ', max_len, options)
