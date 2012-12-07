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

from stgit.argparse import opt
from stgit.commands import common
from stgit.lib import transaction
from stgit.out import *
from stgit import argparse, utils

help = 'Turn regular git commits into StGit patches'
kind = 'stack'
usage = ['[--] <patch-name-1> [<patch-name-2> ...]',
         '-n NUM [--] [<prefix>]',
         '-t <committish> [-x]']
description = """
Take one or more git commits at the base of the current stack and turn
them into StGIT patches. The new patches are created as applied patches
at the bottom of the stack. This is the opposite of 'stg commit'.

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

args = []
options = [
    opt('-n', '--number', type = 'int',
        short = 'Uncommit the specified number of commits'),
    opt('-t', '--to', args = [argparse.commit],
        short = 'Uncommit to the specified commit'),
    opt('-x', '--exclusive', action = 'store_true',
        short = 'Exclude the commit specified by the --to option')]

directory = common.DirectoryHasRepositoryLib()

def func(parser, options, args):
    """Uncommit a number of patches.
    """
    stack = directory.repository.current_stack
    if options.to:
        if options.number:
            parser.error('cannot give both --to and --number')
        if len(args) != 0:
            parser.error('cannot specify patch name with --to')
        patch_nr = patchnames = None
        to_commit = stack.repository.rev_parse(options.to)
        # check whether the --to commit is on a different branch
        merge_bases = directory.repository.get_merge_bases(to_commit, stack.base)
        if not to_commit in merge_bases:
            to_commit = merge_bases[0]
            options.exclusive = True
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

    def check_and_append(c, n):
        next = n.data.parents;
        try:
            [next] = next
        except ValueError:
            out.done()
            raise common.CmdException(
                'Trying to uncommit %s, which does not have exactly one parent'
                % n.sha1)
        return c.append(n)

    commits = []
    next_commit = stack.base
    if patch_nr:
        out.start('Uncommitting %d patches' % patch_nr)
        for i in xrange(patch_nr):
            check_and_append(commits, next_commit)
            next_commit = next_commit.data.parent
    else:
        if options.exclusive:
            out.start('Uncommitting to %s (exclusive)' % to_commit.sha1)
        else:
            out.start('Uncommitting to %s' % to_commit.sha1)
        while True:
            if next_commit == to_commit:
                if not options.exclusive:
                    check_and_append(commits, next_commit)
                break
            check_and_append(commits, next_commit)
            next_commit = next_commit.data.parent
        patch_nr = len(commits)

    taken_names = set(stack.patchorder.all)
    if patchnames:
        for pn in patchnames:
            if pn in taken_names:
                raise common.CmdException('Patch name "%s" already taken' % pn)
            taken_names.add(pn)
    else:
        patchnames = []
        for c in reversed(commits):
            pn = utils.make_patch_name(c.data.message,
                                       lambda pn: pn in taken_names)
            patchnames.append(pn)
            taken_names.add(pn)
        patchnames.reverse()

    trans = transaction.StackTransaction(stack, 'uncommit',
                                         allow_conflicts = True,
                                         allow_bad_head = True)
    for commit, pn in zip(commits, patchnames):
        trans.patches[pn] = commit
    trans.applied = list(reversed(patchnames)) + trans.applied
    trans.run(set_head = False)
    out.done()
