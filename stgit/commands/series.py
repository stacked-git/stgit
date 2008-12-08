
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

from stgit.argparse import opt
from stgit.commands import common
from stgit.commands.common import parse_patches
from stgit.out import out
from stgit.config import config
from stgit import argparse

help = 'Print the patch series'
kind = 'stack'
usage = ['[options] [<patch-range>]']
description = """
Show all the patches in the series or just those in the given
range. The applied patches are prefixed with a '+', the unapplied ones
with a '-' and the hidden ones with a '!'. The current patch is
prefixed with a '>'. Empty patches are prefixed with a '0'."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches,
                             argparse.hidden_patches)]
options = [
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch'),
    opt('-a', '--all', action = 'store_true',
        short = 'Show all patches, including the hidden ones'),
    opt('-A', '--applied', action = 'store_true',
        short = 'Show the applied patches only'),
    opt('-U', '--unapplied', action = 'store_true',
        short = 'Show the unapplied patches only'),
    opt('-H', '--hidden', action = 'store_true',
        short = 'Show the hidden patches only'),
    opt('-m', '--missing', metavar = 'BRANCH',  args = [argparse.stg_branches],
        short = 'Show patches in BRANCH missing in current'),
    opt('-c', '--count', action = 'store_true',
        short = 'Print the number of patches in the series'),
    opt('-d', '--description', action = 'store_true',
        short = 'Show a short description for each patch'),
    opt('--author', action = 'store_true',
        short = 'Show the author name for each patch'),
    opt('-e', '--empty', action = 'store_true',
        short = 'Check whether patches are empty'),
    opt('--showbranch', action = 'store_true',
        short = 'Append the branch name to the listed patches'),
    opt('--noprefix', action = 'store_true',
        short = 'Do not show the patch status prefix'),
    opt('-s', '--short', action = 'store_true',
        short = 'List just the patches around the topmost patch')]

directory = common.DirectoryHasRepositoryLib()

def __get_description(stack, patch):
    """Extract and return a patch's short description
    """
    cd = stack.patches.get(patch).commit.data
    descr = cd.message.strip()
    descr_lines = descr.split('\n')
    return descr_lines[0].rstrip()

def __get_author(stack, patch):
    """Extract and return a patch's short description
    """
    cd = stack.patches.get(patch).commit.data
    return cd.author.name

def __print_patch(stack, patch, branch_str, prefix, length, options):
    """Print a patch name, description and various markers.
    """
    if options.noprefix:
        prefix = ''
    elif options.empty:
        if stack.patches.get(patch).is_empty():
            prefix = '0' + prefix
        else:
            prefix = ' ' + prefix

    patch_str = branch_str + patch

    if options.description or options.author:
        patch_str = patch_str.ljust(length)

    if options.description:
        out.stdout(prefix + patch_str + ' # ' + __get_description(stack, patch))
    elif options.author:
        out.stdout(prefix + patch_str + ' # ' + __get_author(stack, patch))
    else:
        out.stdout(prefix + patch_str)

def func(parser, options, args):
    """Show the patch series
    """
    if options.all and options.short:
        raise common.CmdException, 'combining --all and --short is meaningless'

    stack = directory.repository.get_stack(options.branch)
    if options.missing:
        cmp_stack = stack
        stack = directory.repository.get_stack(options.missing)

    # current series patches
    applied = unapplied = hidden = ()
    if options.applied or options.unapplied or options.hidden:
        if options.all:
            raise common.CmdException, \
                '--all cannot be used with --applied/unapplied/hidden'
        if options.applied:
            applied = stack.patchorder.applied
        if options.unapplied:
            unapplied = stack.patchorder.unapplied
        if options.hidden:
            hidden = stack.patchorder.hidden
    elif options.all:
        applied = stack.patchorder.applied
        unapplied = stack.patchorder.unapplied
        hidden = stack.patchorder.hidden
    else:
        applied = stack.patchorder.applied
        unapplied = stack.patchorder.unapplied

    if options.missing:
        cmp_patches = cmp_stack.patchorder.all
    else:
        cmp_patches = ()

    # the filtering range covers the whole series
    if args:
        show_patches = parse_patches(args, applied + unapplied + hidden,
                                     len(applied))
    else:
        show_patches = applied + unapplied + hidden

    # missing filtering
    show_patches = [p for p in show_patches if p not in cmp_patches]

    # filter the patches
    applied = [p for p in applied if p in show_patches]
    unapplied = [p for p in unapplied if p in show_patches]
    hidden = [p for p in hidden if p in show_patches]

    if options.short:
        nr = int(config.get('stgit.shortnr'))
        if len(applied) > nr:
            applied = applied[-(nr+1):]
        n = len(unapplied)
        if n > nr:
            unapplied = unapplied[:nr]
        elif n < nr:
            hidden = hidden[:nr-n]

    patches = applied + unapplied + hidden

    if options.count:
        out.stdout(len(patches))
        return

    if not patches:
        return

    if options.showbranch:
        branch_str = stack.name + ':'
    else:
        branch_str = ''

    max_len = 0
    if len(patches) > 0:
        max_len = max([len(i + branch_str) for i in patches])

    if applied:
        for p in applied[:-1]:
            __print_patch(stack, p, branch_str, '+ ', max_len, options)
        __print_patch(stack, applied[-1], branch_str, '> ', max_len,
                      options)

    for p in unapplied:
        __print_patch(stack, p, branch_str, '- ', max_len, options)

    for p in hidden:
        __print_patch(stack, p, branch_str, '! ', max_len, options)
