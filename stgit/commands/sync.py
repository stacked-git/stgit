__copyright__ = """
Copyright (C) 2006, Catalin Marinas <catalin.marinas@gmail.com>

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
import stgit.commands.common
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import argparse, stack, git

help = 'Synchronise patches with a branch or a series'
kind = 'patch'
usage = ['[options] [--] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
For each of the specified patches perform a three-way merge with the
same patch in the specified branch or series. The command can be used
for keeping patches on several branches in sync. Note that the
operation may fail for some patches because of conflicts. The patches
in the series must apply cleanly."""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches)]
options = [
    opt('-a', '--all', action = 'store_true',
        short = 'Synchronise all the applied patches'),
    opt('-B', '--ref-branch', args = [argparse.stg_branches],
        short = 'Syncronise patches with BRANCH'),
    opt('-s', '--series', args = [argparse.files],
        short = 'Syncronise patches with SERIES')]

directory = DirectoryGotoToplevel(log = True)

def __check_all():
    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

def __branch_merge_patch(remote_series, pname):
    """Merge a patch from a remote branch into the current tree.
    """
    patch = remote_series.get_patch(pname)
    git.merge_recursive(patch.get_bottom(), git.get_head(), patch.get_top())

def __series_merge_patch(base, patchdir, pname):
    """Merge a patch file with the given StGIT patch.
    """
    patchfile = os.path.join(patchdir, pname)
    git.apply_patch(filename = patchfile, base = base)

def func(parser, options, args):
    """Synchronise a range of patches
    """
    if options.ref_branch:
        remote_series = stack.Series(options.ref_branch)
        if options.ref_branch == crt_series.get_name():
            raise CmdException, 'Cannot synchronise with the current branch'
        remote_patches = remote_series.get_applied()

        # the merge function merge_patch(patch, pname)
        merge_patch = lambda patch, pname: \
                      __branch_merge_patch(remote_series, pname)
    elif options.series:
        patchdir = os.path.dirname(options.series)

        remote_patches = []
        f = file(options.series)
        for line in f:
            p = re.sub('#.*$', '', line).strip()
            if not p:
                continue
            remote_patches.append(p)
        f.close()

        # the merge function merge_patch(patch, pname)
        merge_patch = lambda patch, pname: \
                      __series_merge_patch(patch.get_bottom(), patchdir, pname)
    else:
        raise CmdException, 'No remote branch or series specified'

    applied = crt_series.get_applied()
    unapplied = crt_series.get_unapplied()
    
    if options.all:
        patches = applied
    elif len(args) != 0:
        patches = parse_patches(args, applied + unapplied, len(applied),
                                ordered = True)
    elif applied:
        patches = [crt_series.get_current()]
    else:
        parser.error('no patches applied')

    if not patches:
        raise CmdException, 'No patches to synchronise'

    __check_all()

    # only keep the patches to be synchronised
    sync_patches = [p for p in patches if p in remote_patches]
    if not sync_patches:
        raise CmdException, 'No common patches to be synchronised'

    # pop to the one before the first patch to be synchronised
    first_patch = sync_patches[0]
    if first_patch in applied:
        to_pop = applied[applied.index(first_patch) + 1:]
        if to_pop:
            pop_patches(crt_series, to_pop[::-1])
        pushed = [first_patch]
    else:
        to_pop = []
        pushed = []
    popped = to_pop + [p for p in patches if p in unapplied]

    for p in pushed + popped:
        if p in popped:
            # push this patch
            push_patches(crt_series, [p])
        if p not in sync_patches:
            # nothing to synchronise
            continue

        # the actual sync
        out.start('Synchronising "%s"' % p)

        patch = crt_series.get_patch(p)
        bottom = patch.get_bottom()
        top = patch.get_top()

        # reset the patch backup information.
        patch.set_top(top, backup = True)

        # the actual merging (either from a branch or an external file)
        merge_patch(patch, p)

        if git.local_changes(verbose = False):
            # index (cache) already updated by the git merge. The
            # backup information was already reset above
            crt_series.refresh_patch(cache_update = False, backup = False,
                                     log = 'sync')
            out.done('updated')
        else:
            out.done()
