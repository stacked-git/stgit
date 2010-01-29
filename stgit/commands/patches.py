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
from pydoc import pager
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import argparse, stack, git

help = 'Show the applied patches modifying a file'
kind = 'stack'
usage = ['[options] [--] [<files or dirs>]']
description = """
Show the applied patches modifying the given files. Without arguments,
it shows the patches affected by the local tree modifications. The
'--diff' option also lists the patch log and the diff for the given
files."""

args = [argparse.known_files]
options = [
    opt('-d', '--diff', action = 'store_true',
        short = 'Show the diff for the given files'),
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch')]

directory = DirectoryHasRepository(log = False)

diff_tmpl = \
          '-------------------------------------------------------------------------------\n' \
          '%s\n' \
          '-------------------------------------------------------------------------------\n' \
          '%s' \
          '---\n\n' \
          '%s'

def func(parser, options, args):
    """Show the patches modifying a file
    """
    if not args:
        files = [path for (stat,path) in git.tree_status(verbose = True)]
        # git.tree_status returns absolute paths
    else:
        files = git.ls_files(args)
    directory.cd_to_topdir()

    if not files:
        raise CmdException, 'No files specified or no local changes'

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'

    revs = git.modifying_revs(files, crt_series.get_base(),
                              crt_series.get_head())
    revs.reverse()

    # build the patch/revision mapping
    rev_patch = dict()
    for name in applied:
        patch = crt_series.get_patch(name)
        rev_patch[patch.get_top()] = patch

    # print the patch names
    diff_output = ''
    for rev in revs:
        if rev in rev_patch:
            patch = rev_patch[rev]
            if options.diff:
                diff_output += diff_tmpl \
                               % (patch.get_name(), patch.get_description(),
                                  git.diff(files, patch.get_bottom(),
                                           patch.get_top()))
            else:
                out.stdout(patch.get_name())

    if options.diff:
        pager(diff_output)
