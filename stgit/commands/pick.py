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
from stgit.out import *
from stgit import argparse, stack, git
from stgit.stack import Series

help = 'Import a patch from a different branch or a commit object'
kind = 'patch'
usage = ['[options] [--] ([<patch1>] [<patch2>] [<patch3>..<patch4>])|<commit>']
description = """
Import one or more patches from a different branch or a commit object
into the current series. By default, the name of the imported patch is
used as the name of the current patch. It can be overridden with the
'--name' option. A commit object can be reverted with the '--revert'
option. The log and author information are those of the commit
object."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches,
                             argparse.hidden_patches)]
options = [
    opt('-n', '--name',
        short = 'Use NAME as the patch name'),
    opt('-B', '--ref-branch', args = [argparse.stg_branches],
        short = 'Pick patches from BRANCH'),
    opt('-r', '--revert', action = 'store_true',
        short = 'Revert the given commit object'),
    opt('-p', '--parent', metavar = 'COMMITID', args = [argparse.commit],
        short = 'Use COMMITID as parent'),
    opt('-x', '--expose', action = 'store_true',
        short = 'Append the imported commit id to the patch log'),
    opt('--fold', action = 'store_true',
        short = 'Fold the commit object into the current patch'),
    opt('--update', action = 'store_true',
        short = 'Like fold but only update the current patch files'),
    opt('-f', '--file', action = 'append',
        short = 'Only fold the given file (can be used multiple times)'),
    opt('--unapplied', action = 'store_true',
        short = 'Keep the patch unapplied')]

directory = DirectoryGotoToplevel(log = True)

def __pick_commit(commit_id, patchname, options):
    """Pick a commit id.
    """
    commit = git.Commit(commit_id)

    if options.name:
        patchname = options.name
    elif patchname and options.revert:
        patchname = 'revert-' + patchname
    if patchname:
        patchname = find_patch_name(patchname, crt_series.patch_exists)

    if options.parent:
        parent = git_id(crt_series, options.parent)
    else:
        parent = commit.get_parent()

    if not options.revert:
        bottom = parent
        top = commit_id
    else:
        bottom = commit_id
        top = parent

    if options.fold:
        out.start('Folding commit %s' % commit_id)

        # try a direct git apply first
        if not git.apply_diff(bottom, top, files = options.file):
            if options.file:
                raise CmdException('Patch folding failed')
            else:
                git.merge_recursive(bottom, git.get_head(), top)

        out.done()
    elif options.update:
        rev1 = git_id(crt_series, 'HEAD^')
        rev2 = git_id(crt_series, 'HEAD')
        files = git.barefiles(rev1, rev2).split('\n')

        out.start('Updating with commit %s' % commit_id)

        if not git.apply_diff(bottom, top, files = files):
            raise CmdException, 'Patch updating failed'

        out.done()
    else:
        message = commit.get_log()
        if options.revert:
            if message:
                lines = message.splitlines()
                subject = lines[0]
                body = '\n'.join(lines[2:])
            else:
                subject = commit.get_id_hash()
                body = ''
            message = 'Revert "%s"\n\nThis reverts commit %s.\n\n%s\n' \
                    % (subject, commit.get_id_hash(), body)
        elif options.expose:
            message += '(imported from commit %s)\n' % commit.get_id_hash()
        author_name, author_email, author_date = \
                     name_email_date(commit.get_author())
        if options.revert:
            author_name = author_email = None

        out.start('Importing commit %s' % commit_id)

        newpatch = crt_series.new_patch(patchname, message = message, can_edit = False,
                                        unapplied = True, bottom = bottom, top = top,
                                        author_name = author_name,
                                        author_email = author_email,
                                        author_date = author_date)
        # in case the patch name was automatically generated
        patchname = newpatch.get_name()

        # find a patchlog to fork from
        refbranchname, refpatchname = parse_rev(patchname)
        if refpatchname:
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


def func(parser, options, args):
    """Import a commit object as a new patch
    """
    if not args:
        parser.error('incorrect number of arguments')

    if options.file and not options.fold:
        parser.error('--file can only be specified with --fold')

    if not options.unapplied:
        check_local_changes()
        check_conflicts()
        check_head_top_equal(crt_series)

    if options.ref_branch:
        remote_series = Series(options.ref_branch)
    else:
        remote_series = crt_series

    applied = remote_series.get_applied()
    unapplied = remote_series.get_unapplied()
    try:
        patches = parse_patches(args, applied + unapplied, len(applied))
        commit_id = None
    except CmdException:
        if len(args) > 1:
            raise
        # no patches found, try a commit id
        commit_id = git_id(remote_series, args[0])

    if not commit_id and len(patches) > 1:
        if options.name:
            raise CmdException, '--name can only be specified with one patch'
        if options.parent:
            raise CmdException, '--parent can only be specified with one patch'

    if options.update and not crt_series.get_current():
        raise CmdException, 'No patches applied'

    if commit_id:
        # Try to guess a patch name if the argument was <branch>:<patch>
        try:
            patchname = args[0].split(':')[1]
        except IndexError:
            patchname = None
        __pick_commit(commit_id, patchname, options)
    else:
        if options.unapplied:
            patches.reverse()
        for patch in patches:
            __pick_commit(git_id(remote_series, patch), patch, options)

    print_crt_patch(crt_series)
