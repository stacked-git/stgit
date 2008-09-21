
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
from stgit import argparse, stack, git, basedir
from stgit.config import config, file_extensions
from stgit.gitmergeonefile import interactive_merge

help = 'Mark a file conflict as solved'
kind = 'wc'
usage = ['[options] [<files...>]']
description = """
Mark a merge conflict as resolved. The conflicts can be seen with the
'status' command, the corresponding files being prefixed with a
'C'."""

args = [argparse.conflicting_files]
options = [
    opt('-a', '--all', action = 'store_true',
        short = 'Mark all conflicts as solved'),
    opt('-r', '--reset', metavar = '(ancestor|current|patched)',
        args = [argparse.strings('ancestor', 'current', 'patched')],
        short = 'Reset the file(s) to the given state'),
    opt('-i', '--interactive', action = 'store_true',
        short = 'Run the interactive merging tool')]

directory = DirectoryHasRepository(needs_current_series = False, log = False)

def func(parser, options, args):
    """Mark the conflict as resolved
    """
    args = git.ls_files(args)
    directory.cd_to_topdir()

    if options.reset \
           and options.reset not in file_extensions():
        raise CmdException, 'Unknown reset state: %s' % options.reset

    if options.all and not options.interactive:
        resolved_all(options.reset)
        return

    conflicts = git.get_conflicts()

    if len(args) != 0:
        files = args
    elif options.all:
        files = conflicts
    else:
        parser.error('incorrect number of arguments')

    if not conflicts:
        raise CmdException, 'No more conflicts'

    # check for arguments validity
    if not options.all:
        for filename in files:
            if not filename in conflicts:
                raise CmdException, 'No conflicts for "%s"' % filename

    # resolved
    if options.interactive:
        for filename in files:
            interactive_merge(filename)
            git.resolved([filename])
    else:
        git.resolved(files, options.reset)
