
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
from stgit import stack, git, basedir
from stgit.config import file_extensions


help = 'mark a file conflict as solved'
usage = """%prog [options] [<files...>]

Mark a merge conflict as resolved. The conflicts can be seen with the
'status' command, the corresponding files being prefixed with a
'C'. This command also removes any <file>.{ancestor,current,patched}
files."""

options = [make_option('-a', '--all',
                       help = 'mark all conflicts as solved',
                       action = 'store_true'),
           make_option('-r', '--reset', metavar = '(ancestor|current|patched)',
                       help = 'reset the file(s) to the given state')]


def func(parser, options, args):
    """Mark the conflict as resolved
    """
    if options.reset \
           and options.reset not in file_extensions():
        raise CmdException, 'Unknown reset state: %s' % options.reset

    if options.all:
        resolved_all(options.reset)
        return

    if len(args) == 0:
        parser.error('incorrect number of arguments')

    conflicts = git.get_conflicts()
    if not conflicts:
        raise CmdException, 'No more conflicts'
    # check for arguments validity
    for filename in args:
        if not filename in conflicts:
            raise CmdException, 'No conflicts for "%s"' % filename
    # resolved
    for filename in args:
        resolved(filename, options.reset)
        del conflicts[conflicts.index(filename)]

    # save or remove the conflicts file
    if conflicts == []:
        os.remove(os.path.join(basedir.get(), 'conflicts'))
    else:
        f = file(os.path.join(basedir.get(), 'conflicts'), 'w+')
        f.writelines([line + '\n' for line in conflicts])
        f.close()
