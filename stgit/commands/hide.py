__copyright__ = """
Copyright (C) 2007, Catalin Marinas <catalin.marinas@gmail.com>

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


help = 'hide a patch in the series'
usage = """%prog [options] [<patch-range>]

Hide a range of patches or the current one so that they are no longer
shown in the plain 'series' command output."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]

def func(parser, options, args):
    """Hide a range of patch in the series
    """
    if not args:
        patches = [crt_series.get_current()]
    else:
        applied = crt_series.get_applied()
        unapplied = crt_series.get_unapplied()
        patches = parse_patches(args, applied + unapplied, len(applied))

    patches = [p for p in patches if p not in crt_series.get_hidden()]

    for patch in patches:
        crt_series.hide_patch(patch)
        out.info('Patch "%s" hidden' % patch)
