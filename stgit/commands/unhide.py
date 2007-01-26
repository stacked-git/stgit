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


help = 'unhide a hidden patch in the series'
usage = """%prog [options] [<patch-range>]

Unhide a hidden range of patches or the current one if hidden so that
they are shown in the plain 'series' command output."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one')]

def func(parser, options, args):
    """Unhide a range of patches in the series
    """
    if not args:
        patches = [crt_series.get_current()]
    else:
        applied = crt_series.get_applied()
        unapplied = crt_series.get_unapplied()
        patches = parse_patches(args, applied + unapplied, len(applied))

    patches = [p for p in patches if p in crt_series.get_hidden()]

    for patch in patches:
        crt_series.unhide_patch(patch)
        print 'Patch "%s" unhidden' % patch
