from stgit.commands.common import DirectoryHasRepository
from stgit.lib.stack import Stack

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

help = 'Initialise the current branch for use with StGIT'
kind = 'stack'
usage = ['']
description = """
Initialise the current git branch to be used as an StGIT stack. The
branch (and the git repository it is in) must already exist and
contain at least one commit."""

args = []
options = []

directory = DirectoryHasRepository()


def func(parser, options, args):
    """Performs the repository initialisation"""
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    Stack.initialise(directory.repository)
