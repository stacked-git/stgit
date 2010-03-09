
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

from stgit.commands import common
from stgit.lib import transaction
from stgit import argparse
from stgit.argparse import opt

help = 'Pop one or more patches from the stack'
kind = 'stack'
usage = ['[options] [--] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
Pop the topmost patch or a range of patches from the stack. The
command fails if there are conflicts or local changes (and --keep was
not specified).

A series of pop and push operations are performed so that only the
patches passed on the command line are popped from the stack. Some of
the push operations may fail because of conflicts ("stg undo" would
revert the last push operation)."""

args = [argparse.patch_range(argparse.applied_patches)]
options = [
    opt('-a', '--all', action = 'store_true',
        short = 'Pop all the applied patches'),
    opt('-n', '--number', type = 'int',
        short = 'Pop the specified number of patches', long = '''
        Pop the specified number of patches.

        With a negative number, pop all but that many patches.'''),
    ] + argparse.keep_option()

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Pop the given patches or the topmost one from the stack."""
    stack = directory.repository.current_stack
    iw = stack.repository.default_iw
    clean_iw = (not options.keep and iw) or None
    trans = transaction.StackTransaction(stack, 'pop',
                                         check_clean_iw = clean_iw)

    if options.number == 0:
        # explicitly allow this without any warning/error message
        return

    if not trans.applied:
        raise common.CmdException('No patches applied')

    if options.all:
        patches = trans.applied
    elif options.number is not None:
        # reverse it twice to also work with negative or bigger than
        # the length numbers
        patches = trans.applied[::-1][:options.number][::-1]
    elif not args:
        patches = [trans.applied[-1]]
    else:
        patches = common.parse_patches(args, trans.applied, ordered = True)

    if not patches:
        raise common.CmdException('No patches to pop')

    applied = [p for p in trans.applied if not p in set(patches)]
    unapplied = patches + trans.unapplied
    try:
        trans.reorder_patches(applied, unapplied, iw = iw,
                              allow_interactive = True)
    except transaction.TransactionException:
        pass
    return trans.run(iw)
