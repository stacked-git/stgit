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
from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git

help = 'Make a local clone of a remote repository'
usage = ['<repository> <dir>']
description = """
Clone a git repository into the local directory <dir> (using
stglink:clone[]) and initialise the local branch "master".

This operation is for example suitable to start working using the
"tracking branch" workflow (see link:stg[1]). Other means to setup
an StGit stack are stglink:init[] and the '--create' and '--clone'
commands of stglink:branch[].

The target directory <dir> will be created by this command, and must
not already exist."""

options = []

directory = DirectoryAnywhere(needs_current_series = False)

def func(parser, options, args):
    """Clone the <repository> into the local <dir> and initialises the
    stack
    """
    if len(args) != 2:
        parser.error('incorrect number of arguments')

    repository = args[0]
    local_dir = args[1]

    if os.path.exists(local_dir):
        raise CmdException, '"%s" exists. Remove it first' % local_dir

    print 'Cloning "%s" into "%s"...' % (repository, local_dir)

    git.clone(repository, local_dir)
    os.chdir(local_dir)
    git.checkout(tree_id = 'HEAD')

    # be sure to forget any cached value for .git, since we're going
    # to work on a brand new repository
    basedir.clear_cache()
    stack.Series().init()

    print 'done'
