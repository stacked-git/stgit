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

help = 'permanently store the applied patches into stack base'
usage = """%prog [options]

Merge the applied patches into the base of the current stack and
remove them from the series while advancing the base.

Use this command only if you want to permanently store the applied
patches and no longer manage them with StGIT."""

options = []


def func(parser, options, args):
    """Merge the applied patches into the base of the current stack
       and remove them from the series while advancing the base
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    applied = crt_series.get_applied()
    if not applied:
        raise CmdException, 'No patches applied'

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected.  Commit is not permitted'

    crt_head = git.get_head()

    out.start('Committing %d patches' % len(applied))

    crt_series.pop_patch(applied[0])
    git.switch(crt_head)

    for patch in applied:
        crt_series.delete_patch(patch)

    out.done()
