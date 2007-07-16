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
from stgit.stack import Series


help = 'import a patch from a different branch or a commit object'
usage = """%prog [options] [<patch@branch>|<commit>]

Import a patch from a different branch or a commit object into the
current series. By default, the name of the imported patch is used as
the name of the current patch. It can be overridden with the '--name'
option. A commit object can be reverted with the '--reverse'
option. The log and author information are those of the commit object."""

options = [make_option('-n', '--name',
                       help = 'use NAME as the patch name'),
           make_option('-r', '--reverse',
                       help = 'reverse the commit object before importing',
                       action = 'store_true'),
           make_option('-p', '--parent', metavar = 'COMMITID',
                       help = 'use COMMITID as parent'),
           make_option('-x', '--expose',
                       help = 'append the imported commit id to the patch log',
                       action = 'store_true'),
           make_option('--fold',
                       help = 'fold the commit object into the current patch',
                       action = 'store_true'),
           make_option('--update',
                       help = 'like fold but only update the current patch files',
                       action = 'store_true'),
           make_option('--unapplied',
                       help = 'keep the patch unapplied',
                       action = 'store_true')]


def func(parser, options, args):
    """Import a commit object as a new patch
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    if not options.unapplied:
        check_local_changes()
        check_conflicts()
        check_head_top_equal()

    commit_str = args[0]
    commit_id = git_id(commit_str)
    commit = git.Commit(commit_id)

    if options.fold or options.update:
        if not crt_series.get_current():
            raise CmdException, 'No patches applied'
    else:
        patch_branch = commit_str.split('@')
        if options.name:
            patchname = options.name
        elif len(patch_branch) == 2:
            patchname = patch_branch[0]
        else:
            patchname = None

    if options.parent:
        parent = git_id(options.parent)
    else:
        parent = commit.get_parent()

    if not options.reverse:
        bottom = parent
        top = commit_id
    else:
        bottom = commit_id
        top = parent

    if options.fold:
        out.start('Folding commit %s' % commit_id)

        # try a direct git-apply first
        if not git.apply_diff(bottom, top):
            git.merge(bottom, git.get_head(), top, recursive = True)

        out.done()
    elif options.update:
        rev1 = git_id('//bottom')
        rev2 = git_id('//top')
        files = git.barefiles(rev1, rev2).split('\n')

        out.start('Updating with commit %s' % commit_id)

        if not git.apply_diff(bottom, top, files = files):
            raise CmdException, 'Patch updating failed'

        out.done()
    else:
        message = commit.get_log()
        if options.expose:
            message += '(imported from commit %s)\n' % commit.get_id_hash()
        author_name, author_email, author_date = \
                     name_email_date(commit.get_author())

        out.start('Importing commit %s' % commit_id)

        newpatch = crt_series.new_patch(patchname, message = message, can_edit = False,
                                        unapplied = True, bottom = bottom, top = top,
                                        author_name = author_name,
                                        author_email = author_email,
                                        author_date = author_date)
        # in case the patch name was automatically generated
        patchname = newpatch.get_name()

        # find a patchlog to fork from
        (refpatchname, refbranchname, refpatchid) = parse_rev(commit_str)
        if refpatchname and not refpatchid and \
               (not refpatchid or refpatchid == 'top'):
            # FIXME: should also support picking //top.old
            if refbranchname:
                # assume the refseries is OK, since we already resolved
                # commit_str to a git_id
                refseries = Series(refbranchname)
            else:
                refseries = crt_series
            patch = refseries.get_patch(refpatchname)
            if patch.get_log():
                out.info("Log was %s" % newpatch.get_log())
                out.info("Setting log to %s\n" %  patch.get_log())
                newpatch.set_log(patch.get_log())
                out.info("Log is now %s" % newpatch.get_log())
            else:
                out.info("No log for %s\n" % patchname)

        if not options.unapplied:
            modified = crt_series.push_patch(patchname)
        else:
            modified = False

        if crt_series.empty_patch(patchname):
            out.done('empty patch')
        elif modified:
            out.done('modified')
        else:
            out.done()

    print_crt_patch()
