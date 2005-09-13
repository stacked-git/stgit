
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
                       help = 'use BRANCH instead of the default one')]


def func(parser, options, args):
    """Show the patch series
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    applied = crt_series.get_applied()
    if len(applied) > 0:
        for p in applied [0:-1]:
            if crt_series.empty_patch(p):
                print '0', p
            else:
                print '+', p
        p = applied[-1]

        if crt_series.empty_patch(p):
            print '0>%s' % p
        else:
            print '> %s' % p

    for p in crt_series.get_unapplied():
        if crt_series.empty_patch(p):
            print '0', p
        else:
            print '-', p
