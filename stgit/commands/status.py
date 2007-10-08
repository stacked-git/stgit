
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


help = 'show the tree status'
usage = """%prog [options] [<files or dirs>]

Show the status of the whole working copy or the given files. The
command also shows the files in the current directory which are not
under revision control. The files are prefixed as follows:

  M - locally modified
  N - newly added to the repository
  D - deleted from the repository
  C - conflict
  ? - unknown

A 'refresh' command clears the status of the modified, new and deleted
files."""

directory = DirectoryHasRepository(needs_current_series = False)
options = [make_option('-m', '--modified',
                       help = 'show modified files only',
                       action = 'store_true'),
           make_option('-n', '--new',
                       help = 'show new files only',
                       action = 'store_true'),
           make_option('-d', '--deleted',
                       help = 'show deleted files only',
                       action = 'store_true'),
           make_option('-c', '--conflict',
                       help = 'show conflict files only',
                       action = 'store_true'),
           make_option('-u', '--unknown',
                       help = 'show unknown files only',
                       action = 'store_true'),
           make_option('-x', '--noexclude',
                       help = 'do not exclude any files from listing',
                       action = 'store_true'),
           make_option('-O', '--diff-opts',
                       help = 'options to pass to git-diff'),
           make_option('--reset',
                       help = 'reset the current tree changes',
                       action = 'store_true')]


def status(files = None, modified = False, new = False, deleted = False,
           conflict = False, unknown = False, noexclude = False,
           diff_flags = []):
    """Show the tree status
    """
    cache_files = git.tree_status(files,
                                  unknown = (not files),
                                  noexclude = noexclude,
                                  diff_flags = diff_flags)
    filtered = (modified or new or deleted or conflict or unknown)

    if filtered:
        filestat = []
        if modified:
            filestat.append('M')
        if new:
            filestat.append('A')
            filestat.append('N')
        if deleted:
            filestat.append('D')
        if conflict:
            filestat.append('C')
        if unknown:
            filestat.append('?')
        cache_files = [x for x in cache_files if x[0] in filestat]

    output = []
    for st, fn in cache_files:
        if filtered:
            output.append(fn)
        else:
            output.append('%s %s' % (st, fn))
    for o in sorted(output):
        out.stdout(o)

def func(parser, options, args):
    """Show the tree status
    """
    args = git.ls_files(args)
    directory.cd_to_topdir()

    if options.reset:
        if args:
            for f in args:
                resolved(f)
            git.reset(args)
        else:
            resolved_all()
            git.reset()
    else:
        if options.diff_opts:
            diff_flags = options.diff_opts.split()
        else:
            diff_flags = []

        status(args, options.modified, options.new, options.deleted,
               options.conflict, options.unknown, options.noexclude,
               diff_flags = diff_flags)
