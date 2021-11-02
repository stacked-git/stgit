import io
import os
import re

from stgit.argparse import opt, patch_range
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    check_head_top_equal,
    check_index_and_worktree_clean,
    parse_patches,
)
from stgit.lib.git import CommitData
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

help = 'Synchronise patches with a branch or a series'
kind = 'patch'
usage = ['[options] [--] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
For each of the specified patches perform a three-way merge with the
same patch in the specified branch or series. The command can be used
for keeping patches on several branches in sync. Note that the
operation may fail for some patches because of conflicts. The patches
in the series must apply cleanly."""

args = [patch_range('applied_patches', 'unapplied_patches')]
options = [
    opt(
        '-a',
        '--all',
        action='store_true',
        short='Synchronise all the applied patches',
    ),
    opt(
        '-B',
        '--ref-branch',
        args=['stg_branches'],
        short='Syncronise patches with BRANCH',
    ),
    opt(
        '-s',
        '--series',
        args=['files'],
        short='Syncronise patches with SERIES',
    ),
]

directory = DirectoryGotoTopLevel()


def __branch_merge_patch(remote_stack, stack, commit, pname):
    """Merge a patch from a remote branch into the current tree."""
    remote = remote_stack.patches[pname]
    iw = stack.repository.default_iw
    iw.checkout(new_tree=commit.data.tree, old_tree=stack.head.data.tree)
    iw.refresh_index()
    iw.merge(
        base=remote.data.parent.data.tree,
        ours=commit.data.tree,
        theirs=remote.data.tree,
    )
    if iw.index.is_clean(commit.data.tree):
        return None
    else:
        return iw.index.write_tree()


def __series_merge_patch(patchdir, stack, commit, pname):
    """Merge a patch file with the given StGit patch."""
    with io.open(os.path.join(patchdir, pname), 'rb') as f:
        diff = f.read()
    base = commit.data.parent
    orig_head = stack.head
    iw = stack.repository.default_iw
    iw.refresh_index()
    iw.checkout(new_tree=base.data.tree, old_tree=orig_head.data.tree)
    stack.set_head(base, msg='apply patch')
    iw.apply(diff, quiet=False)
    iw.update_index(iw.changed_files(base.data.tree))
    new_tree = iw.index.write_tree()
    stack.repository.commit(
        CommitData(
            tree=new_tree,
            message='temp commit for applying patch',
            parents=[base],
        )
    )
    iw.checkout(new_tree=orig_head.data.tree, old_tree=new_tree)
    stack.set_head(orig_head, msg='post apply')
    iw.merge(base=base.data.tree, ours=orig_head.data.tree, theirs=new_tree)
    if iw.index.is_clean(orig_head.data.tree):
        return None
    else:
        return new_tree


def func(parser, options, args):
    """Synchronise a range of patches"""
    repository = directory.repository
    stack = repository.get_stack()

    if options.ref_branch:
        remote_stack = repository.get_stack(options.ref_branch)
        if remote_stack.name == stack.name:
            raise CmdException('Cannot synchronise with the current branch')
        remote_patches = remote_stack.patchorder.applied

        def merge_patch(commit, pname):
            return __branch_merge_patch(remote_stack, stack, commit, pname)

    elif options.series:
        patchdir = os.path.dirname(options.series)

        remote_patches = []
        with open(options.series) as f:
            for line in f:
                pn = re.sub('#.*$', '', line).strip()
                if not pn:
                    continue
                remote_patches.append(pn)

        def merge_patch(commit, pname):
            return __series_merge_patch(patchdir, stack, commit, pname)

    else:
        raise CmdException('No remote branch or series specified')

    applied = list(stack.patchorder.applied)
    unapplied = list(stack.patchorder.unapplied)

    if options.all:
        patches = applied
    elif len(args) != 0:
        patches = parse_patches(args, applied + unapplied, len(applied), ordered=True)
    elif applied:
        patches = [applied[-1]]
    else:
        parser.error('no patches applied')

    assert patches

    # only keep the patches to be synchronised
    sync_patches = [p for p in patches if p in remote_patches]
    if not sync_patches:
        raise CmdException('No common patches to be synchronised')

    iw = repository.default_iw
    check_head_top_equal(stack)
    check_index_and_worktree_clean(stack)

    # pop to the one before the first patch to be synchronised
    first_patch = sync_patches[0]
    if first_patch in applied:
        to_pop = applied[applied.index(first_patch) + 1 :]
        if to_pop:
            trans = StackTransaction(stack)
            popped_extra = trans.pop_patches(lambda pn: pn in to_pop)
            assert not popped_extra
            retval = trans.run('sync (pop)', iw)
            assert not retval
        pushed = [first_patch]
    else:
        to_pop = []
        pushed = []
    popped = to_pop + [p for p in patches if p in unapplied]

    trans = StackTransaction(stack)
    try:
        for p in pushed + popped:
            if p in popped:
                trans.push_patch(p, iw=iw)

            if p not in sync_patches:
                # nothing to synchronise
                continue

            # the actual sync
            out.start('Synchronising "%s"' % p)

            commit = trans.patches[p]

            # the actual merging (either from a branch or an external file)
            tree = merge_patch(commit, p)
            if tree:
                trans.patches[p] = commit.data.set_tree(tree).commit(repository)
                out.done('updated')
            else:
                out.done()
    except TransactionHalted:
        pass
    return trans.run('sync', iw)
