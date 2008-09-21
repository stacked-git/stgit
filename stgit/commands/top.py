
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
from stgit.out import out
from stgit import argparse

help = 'Print the name of the top patch'
kind = 'stack'
usage = ['']
description = """
Print the name of the current (topmost) patch."""

args = []
options = [
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch')]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Show the name of the topmost patch
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    stack = directory.repository.get_stack(options.branch)
    applied = stack.patchorder.applied

    if applied:
        out.stdout(applied[-1])
    else:
        raise common.CmdException, 'No patches applied'
