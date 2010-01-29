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
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit import argparse, stack, git

help = 'Move the stack base to another point in history'
kind = 'stack'
usage = ['[options] [--] <new-base-id>']
description = """
Pop all patches from current stack, move the stack base to the given
<new-base-id> and push the patches back.

If you experience merge conflicts, resolve the problem and continue
the rebase by executing the following sequence:

        $ git add --update
        $ stg refresh
        $ stg goto top-patch

Or if you want to skip that patch:

        $ stg undo --hard
        $ stg push next-patch..top-patch"""

args = [argparse.commit]
options = [
    opt('-n', '--nopush', action = 'store_true',
        short = 'Do not push the patches back after rebasing'),
    opt('-m', '--merged', action = 'store_true',
        short = 'Check for patches merged upstream')]

directory = DirectoryGotoToplevel(log = True)

def func(parser, options, args):
    """Rebase the current stack
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected. Rebase is not permitted'

    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

    # ensure an exception is raised before popping on non-existent target
    if git_id(crt_series, args[0]) == None:
        raise GitException, 'Unknown revision: %s' % args[0]
        
    applied = prepare_rebase(crt_series)
    rebase(crt_series, args[0])
    post_rebase(crt_series, applied, options.nopush, options.merged)

    print_crt_patch(crt_series)
