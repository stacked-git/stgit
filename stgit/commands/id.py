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

from optparse import OptionParser, make_option

from stgit.out import out
from stgit.commands import common
from stgit.lib import stack

help = 'print the GIT hash value of a StGIT reference'
usage = """%prog [options] [id]

Print the SHA1 value of a Git id (defaulting to HEAD). In addition to
the standard Git id's like heads and tags, this command also accepts
'[<branch>:]<patch>' and '[<branch>:]{base}' showing the id of a patch
or the base of the stack. If no branch is specified, it defaults to the
current one. The bottom of a patch is accessible with the
'[<branch>:]<patch>^' format."""

directory = common.DirectoryHasRepositoryLib()
options = []

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
