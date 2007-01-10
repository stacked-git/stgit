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
from pydoc import pager

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'show the patches modifying a file'
usage = """%prog [options] <file> [<file>...]

Show the applied patches modifying the given files. The '--diff'
option also lists the patch log and the diff for the given files."""

options = [make_option('-d', '--diff',
                       help = 'show the diff for the given files',
                       action = 'store_true')]

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
    if len(args) < 1:
        parser.error('incorrect number of arguments')

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'

    revs = git.modifying_revs(args, git_id('base'))
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
                                  git.diff(args, patch.get_bottom(),
                                           patch.get_top()))
            else:
                print patch.get_name()

    if options.diff:
        pager(diff_output)
