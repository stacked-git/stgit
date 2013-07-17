# -*- coding: utf-8 -*-

__copyright__ = """
Copyright (C) 2005, Catalin Marinas <catalin.marinas@gmail.com>
Copyright (C) 2008, Karl Hasselström <kha@treskal.com>

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
from stgit.lib import git, transaction, edit
from stgit.out import out
from stgit import argparse, utils

help = 'Generate a new commit for the current patch'
kind = 'patch'
usage = ['[options] [--] [<files or dirs>]']
description = """
Include the latest work tree and index changes in the current patch.
This command generates a new git commit object for the patch; the old
commit is no longer visible.

Refresh will warn if the index is dirty, and require use of either the '--index'
or '--force' options to override this check. This is to prevent accidental full
refresh when only some changes were staged using git add interative mode.

You may optionally list one or more files or directories relative to
the current working directory; if you do, only matching files will be
updated.

Behind the scenes, stg refresh first creates a new temporary patch
with your updates, and then merges that patch into the patch you asked
to have refreshed. If you asked to refresh a patch other than the
topmost patch, there can be conflicts; in that case, the temporary
patch will be left for you to take care of, for example with stg
squash.

The creation of the temporary patch is recorded in a separate entry in
the patch stack log; this means that one undo step will undo the merge
between the other patch and the temp patch, and two undo steps will
additionally get rid of the temp patch."""

args = [argparse.dirty_files]
options = [
    opt('-u', '--update', action = 'store_true',
        short = 'Only update the current patch files'),
    opt('-i', '--index', action = 'store_true',
        short = 'Refresh from index instead of worktree', long = """
        Instead of setting the patch top to the current contents of
        the worktree, set it to the current contents of the index."""),
    opt('-F', '--force', action = 'store_true',
        short = 'Force refresh even if index is dirty', long = """
        Instead of warning the user when some work has already been staged (such
        as with git add interactive mode) force a full refresh."""),
    opt('-p', '--patch', args = [argparse.other_applied_patches,
                                 argparse.unapplied_patches],
        short = 'Refresh (applied) PATCH instead of the top patch'),
    opt('-e', '--edit', action = 'store_true',
        short = 'Invoke an editor for the patch description'),
    opt('-a', '--annotate', metavar = 'NOTE',
        short = 'Annotate the patch log entry')
    ] + (argparse.message_options(save_template = False) +
         argparse.sign_options() + argparse.author_options())

directory = common.DirectoryHasRepositoryLib()

def get_patch(stack, given_patch):
    """Get the name of the patch we are to refresh."""
    if given_patch:
        patch_name = given_patch
        if not stack.patches.exists(patch_name):
            raise common.CmdException('%s: no such patch' % patch_name)
        return patch_name
    else:
        if not stack.patchorder.applied:
            raise common.CmdException(
                'Cannot refresh top patch, because no patches are applied')
        return stack.patchorder.applied[-1]

def list_files(stack, patch_name, args, index, update):
    """Figure out which files to update."""
    if index:
        # --index: Don't update the index.
        return set()
    paths = stack.repository.default_iw.changed_files(
        stack.head.data.tree, args or [])
    if update:
        # --update: Restrict update to the paths that were already
        # part of the patch.
        paths &= stack.patches.get(patch_name).files()
    return paths

def write_tree(stack, paths, temp_index):
    """Possibly update the index, and then write its tree.
    @return: The written tree.
    @rtype: L{Tree<stgit.git.Tree>}"""
    def go(index):
        if paths:
            iw = git.IndexAndWorktree(index, stack.repository.default_worktree)
            iw.update_index(paths)
        return index.write_tree()
    if temp_index:
        index = stack.repository.temp_index()
        try:
            index.read_tree(stack.head)
            return go(index)
        finally:
            index.delete()
            stack.repository.default_iw.update_index(paths)
    else:
        return go(stack.repository.default_index)

def make_temp_patch(stack, patch_name, paths, temp_index):
    """Commit index to temp patch, in a complete transaction. If any path
    limiting is in effect, use a temp index."""
    tree = write_tree(stack, paths, temp_index)
    commit = stack.repository.commit(git.CommitData(
            tree = tree, parents = [stack.head],
            message = 'Refresh of %s' % patch_name))
    temp_name = utils.make_patch_name('refresh-temp', stack.patches.exists)
    trans = transaction.StackTransaction(stack,
                                         'refresh (create temporary patch)')
    trans.patches[temp_name] = commit
    trans.applied.append(temp_name)
    return trans.run(stack.repository.default_iw,
                     print_current_patch = False), temp_name

def absorb_applied(trans, iw, patch_name, temp_name, edit_fun):
    """Absorb the temp patch (C{temp_name}) into the given patch
    (C{patch_name}), which must be applied. If the absorption
    succeeds, call C{edit_fun} on the resulting
    L{CommitData<stgit.lib.git.CommitData>} before committing it and
    commit the return value.

    @return: C{True} if we managed to absorb the temp patch, C{False}
             if we had to leave it for the user to deal with."""
    temp_absorbed = False
    try:
        # Pop any patch on top of the patch we're refreshing.
        to_pop = trans.applied[trans.applied.index(patch_name)+1:]
        if len(to_pop) > 1:
            popped = trans.pop_patches(lambda pn: pn in to_pop)
            assert not popped # no other patches were popped
            trans.push_patch(temp_name, iw)
        assert to_pop.pop() == temp_name

        # Absorb the temp patch.
        temp_cd = trans.patches[temp_name].data
        assert trans.patches[patch_name] == temp_cd.parent
        trans.patches[patch_name] = trans.stack.repository.commit(
            edit_fun(trans.patches[patch_name].data.set_tree(temp_cd.tree)))
        popped = trans.delete_patches(lambda pn: pn == temp_name, quiet = True)
        assert not popped # the temp patch was topmost
        temp_absorbed = True

        # Push back any patch we were forced to pop earlier.
        for pn in to_pop:
            trans.push_patch(pn, iw)
    except transaction.TransactionHalted:
        pass
    return temp_absorbed

def absorb_unapplied(trans, iw, patch_name, temp_name, edit_fun):
    """Absorb the temp patch (C{temp_name}) into the given patch
    (C{patch_name}), which must be unapplied. If the absorption
    succeeds, call C{edit_fun} on the resulting
    L{CommitData<stgit.lib.git.CommitData>} before committing it and
    commit the return value.

    @param iw: Not used.
    @return: C{True} if we managed to absorb the temp patch, C{False}
             if we had to leave it for the user to deal with."""

    # Pop the temp patch.
    popped = trans.pop_patches(lambda pn: pn == temp_name)
    assert not popped # the temp patch was topmost

    # Try to create the new tree of the refreshed patch. (This is the
    # same operation as pushing the temp patch onto the patch we're
    # trying to refresh -- but we don't have a worktree to spill
    # conflicts to, so if the simple merge doesn't succeed, we have to
    # give up.)
    patch_cd = trans.patches[patch_name].data
    temp_cd = trans.patches[temp_name].data
    new_tree = trans.stack.repository.simple_merge(
        base = temp_cd.parent.data.tree,
        ours = patch_cd.tree, theirs = temp_cd.tree)
    if new_tree:
        # It worked. Refresh the patch with the new tree, and delete
        # the temp patch.
        trans.patches[patch_name] = trans.stack.repository.commit(
            edit_fun(patch_cd.set_tree(new_tree)))
        popped = trans.delete_patches(lambda pn: pn == temp_name, quiet = True)
        assert not popped # the temp patch was not applied
        return True
    else:
        # Nope, we couldn't create the new tree, so we'll just have to
        # leave the temp patch for the user.
        return False

def absorb(stack, patch_name, temp_name, edit_fun, annotate = None):
    """Absorb the temp patch into the target patch."""
    if annotate:
        log_msg = 'refresh\n\n' + annotate
    else:
        log_msg = 'refresh'
    trans = transaction.StackTransaction(stack, log_msg)
    iw = stack.repository.default_iw
    f = { True: absorb_applied, False: absorb_unapplied
          }[patch_name in trans.applied]
    if f(trans, iw, patch_name, temp_name, edit_fun):
        def info_msg(): pass
    else:
        def info_msg():
            out.warn('The new changes did not apply cleanly to %s.'
                     % patch_name, 'They were saved in %s.' % temp_name)
    r = trans.run(iw)
    info_msg()
    return r

def func(parser, options, args):
    """Generate a new commit for the current or given patch."""

    # Catch illegal argument combinations.
    path_limiting = bool(args or options.update)
    if options.index and path_limiting:
        raise common.CmdException(
            'Only full refresh is available with the --index option')

    if options.index and options.force:
        raise common.CmdException(
            'You cannot --force a full refresh when using --index mode')

    stack = directory.repository.current_stack
    patch_name = get_patch(stack, options.patch)
    paths = list_files(stack, patch_name, args, options.index, options.update)

    # Make sure there are no conflicts in the files we want to
    # refresh.
    if stack.repository.default_index.conflicts() & paths:
        raise common.CmdException(
            'Cannot refresh -- resolve conflicts first')

    # Make sure the index is clean before performing a full refresh
    if not options.index and not options.force:
        if not (stack.repository.default_index.is_clean(stack.head) or
                stack.repository.default_iw.worktree_clean()):
            raise common.CmdException(
                'The index is dirty. Did you mean --index? To force a full refresh use --force.')

    # Commit index to temp patch, and absorb it into the target patch.
    retval, temp_name = make_temp_patch(
        stack, patch_name, paths, temp_index = path_limiting)
    if retval != utils.STGIT_SUCCESS:
        return retval
    def edit_fun(cd):
        cd, failed_diff = edit.auto_edit_patch(
            stack.repository, cd, msg = options.message, contains_diff = False,
            author = options.author, committer = lambda p: p,
            sign_str = options.sign_str)
        assert not failed_diff
        if options.edit:
            cd, failed_diff = edit.interactive_edit_patch(
                stack.repository, cd, edit_diff = False,
                diff_flags = [], replacement_diff = None)
            assert not failed_diff
        return cd
    return absorb(stack, patch_name, temp_name, edit_fun,
                  annotate = options.annotate)
