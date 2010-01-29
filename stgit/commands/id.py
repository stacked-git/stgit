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

from stgit.out import out
from stgit.commands import common
from stgit.lib import stack
from stgit import argparse

help = 'Print the git hash value of a StGit reference'
kind = 'repo'
usage = ['[options] [--] [<id>]']
description = """
Print the SHA1 value of a Git id (defaulting to HEAD). In addition to the
standard Git id's like heads and tags, this command also accepts
'[<branch>:]<patch>' for the id of a patch, '[<branch>:]\{base\}' for the base
of the stack and '[<branch>:]\{public\}' for the public branch corresponding
to the stack (see the 'publish' command for details). If no branch is
specified, it defaults to the current one. The bottom of a patch is accessible
with the '[<branch>:]<patch>^' format."""

args = [argparse.applied_patches, argparse.unapplied_patches,
        argparse.hidden_patches]
options = []

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Show the applied patches
    """
    if len(args) == 0:
        id_str = 'HEAD'
    elif len(args) == 1:
        id_str = args[0]
    else:
        parser.error('incorrect number of arguments')

    out.stdout(common.git_commit(id_str, directory.repository).sha1)
