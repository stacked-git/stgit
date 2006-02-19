__copyright__ = """
Copyright (C) 2006, Karl Hasselström <kha@treskal.com>

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

help = 'turn regular git commits into StGIT patches'
usage = """%prog [options] <patchname1> [<patchname2> ... ]

Takes one or more git commits at the base of the current stack, and
turns them into StGIT patches. These new patches are alreay applied,
at the bottom of the stack. This is the exact opposite of 'stg
commit'.

You can either give one patch name for each commit you wish to
uncommit, or use the --number option and exactly one patch name; StGIT
will then create numbered patches with the given patch name as prefix.

Only commits with exactly one parent can be uncommitted; in other
words, you can't uncommmit a merge."""

options = [make_option('-n', '--number', type = 'int',
                       help = 'uncommit the specified number of commits')]

def func(parser, options, args):
    if len(args) == 0:
        parser.error('you must specify at least one patch name')
    if options.number:
        if len(args) != 1:
            parser.error('when using --number, specify exactly one patch name')
        patchnames = ['%s%d' % (args[0], i)
                      for i in xrange(options.number - 1, -1, -1)]
    else:
        patchnames = args

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected. Uncommit is not permitted'

    print 'Uncommitting %d patches...' % len(patchnames),
    sys.stdout.flush()

    for patchname in patchnames:
        base_file = crt_series.get_base_file()
        commit_id = read_string(base_file)
        commit = git.Commit(commit_id)
        try:
            parent, = commit.get_parents()
        except ValueError:
            raise CmdException, ('Commit %s does not have exactly one parent'
                                 % commit_id)
        author_name, author_email, author_date = name_email_date(
            commit.get_author())
        crt_series.new_patch(patchname,
                             can_edit = False, before_existing = True,
                             top = commit_id, bottom = parent,
                             message = commit.get_log(),
                             author_name = author_name,
                             author_email = author_email,
                             author_date = author_date)
        write_string(base_file, parent)

    print 'done'
