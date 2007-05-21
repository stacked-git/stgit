# -*- coding: utf-8 -*-

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

help = 'turn regular GIT commits into StGIT patches'
usage = """%prog [<patchnames>] | -n NUM [<prefix>]] | -t <committish>

Take one or more git commits at the base of the current stack and turn
them into StGIT patches. The new patches are created as applied patches
at the bottom of the stack. This is the exact opposite of 'stg commit'.

By default, the number of patches to uncommit is determined by the
number of patch names provided on the command line. First name is used
for the first patch to uncommit, i.e. for the newest patch.

The -n/--number option specifies the number of patches to uncommit. In
this case, at most one patch name may be specified. It is used as
prefix to which the patch number is appended. If no patch names are
provided on the command line, StGIT automatically generates them based
on the first line of the patch description.

The -t/--to option specifies that all commits up to and including the
given commit should be uncommitted.

Only commits with exactly one parent can be uncommitted; in other
words, you can't uncommit a merge."""

options = [make_option('-n', '--number', type = 'int',
                       help = 'uncommit the specified number of commits'),
           make_option('-t', '--to',
                       help = 'uncommit to the specified commit')]

def func(parser, options, args):
    """Uncommit a number of patches.
    """
    if options.to:
        if options.number:
            parser.error('cannot give both --to and --number')
        if len(args) != 0:
            parser.error('cannot specify patch name with --to')
        patch_nr = patchnames = None
        to_commit = git.rev_parse(options.to)
    elif options.number:
        if options.number <= 0:
            parser.error('invalid value passed to --number')

        patch_nr = options.number

        if len(args) == 0:
            patchnames = None
        elif len(args) == 1:
            # prefix specified
            patchnames = ['%s%d' % (args[0], i)
                          for i in xrange(patch_nr, 0, -1)]
        else:
            parser.error('when using --number, specify at most one patch name')
    elif len(args) == 0:
        patchnames = None
        patch_nr = 1
    else:
        patchnames = args
        patch_nr = len(patchnames)

    if crt_series.get_protected():
        raise CmdException, \
              'This branch is protected. Uncommit is not permitted'

    def get_commit(commit_id):
        commit = git.Commit(commit_id)
        try:
            parent, = commit.get_parents()
        except ValueError:
            raise CmdException('Commit %s does not have exactly one parent'
                               % commit_id)
        return (commit, commit_id, parent)

    commits = []
    next_commit = crt_series.get_base()
    if patch_nr:
        out.start('Uncommitting %d patches' % patch_nr)
        for i in xrange(patch_nr):
            commit, commit_id, parent = get_commit(next_commit)
            commits.append((commit, commit_id, parent))
            next_commit = parent
    else:
        out.start('Uncommitting to %s' % to_commit)
        while True:
            commit, commit_id, parent = get_commit(next_commit)
            commits.append((commit, commit_id, parent))
            if commit_id == to_commit:
                break
            next_commit = parent
        patch_nr = len(commits)

    for (commit, commit_id, parent), patchname in \
        zip(commits, patchnames or [None for i in xrange(len(commits))]):
        author_name, author_email, author_date = \
                     name_email_date(commit.get_author())
        crt_series.new_patch(patchname,
                             can_edit = False, before_existing = True,
                             top = commit_id, bottom = parent,
                             message = commit.get_log(),
                             author_name = author_name,
                             author_email = author_email,
                             author_date = author_date)

    out.done()
