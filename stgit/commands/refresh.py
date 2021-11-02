from stgit import argparse, utils
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    check_head_top_equal,
    run_commit_msg_hook,
)
from stgit.config import config
from stgit.lib.edit import auto_edit_patch, interactive_edit_patch
from stgit.lib.git import CommitData, IndexAndWorktree
from stgit.lib.transaction import (
    StackTransaction,
    TransactionException,
    TransactionHalted,
)
from stgit.out import out
from stgit.run import RunException
from stgit.utils import get_hook

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

help = 'Generate a new commit for the current patch'
kind = 'patch'
usage = ['[options] [--] [<files or dirs>]']
description = """
Include the latest work tree and index changes in the current patch.
This command generates a new git commit object for the patch; the old
commit is no longer visible.

Refresh will warn if the index is dirty, and require use of either the
'--index' or '--force' options to override this check. This is to prevent
accidental full refresh when only some changes were staged using git add
interative mode.

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
additionally get rid of the temp patch.

Additionally, the '--spill' option resets the topmost patch, emptying
the patch while leaving the patch's changes intact in the worktree."""

args = ['dirty_files']
options = [
    opt(
        '-u',
        '--update',
        action='store_true',
        short='Only update the current patch files',
    ),
    opt(
        '-i',
        '--index',
        action='store_true',
        short='Refresh from index instead of worktree',
        long="""
        Instead of setting the patch top to the current contents of
        the worktree, set it to the current contents of the index.""",
    ),
    opt(
        '-F',
        '--force',
        action='store_true',
        short='Force refresh even if index is dirty',
        long="""
        Instead of warning the user when some work has already been staged
        (such as with git add interactive mode) force a full refresh.""",
    ),
    opt(
        '-p',
        '--patch',
        args=['other_applied_patches', 'unapplied_patches'],
        short='Refresh (applied) PATCH instead of the top patch',
    ),
    opt(
        '-e',
        '--edit',
        action='store_true',
        short='Invoke an editor for the patch description',
    ),
    opt(
        '-d',
        '--diff',
        action='store_true',
        short='Show diff when editing patch description',
    ),
    opt(
        '-a',
        '--annotate',
        metavar='NOTE',
        short='Annotate the patch log entry',
    ),
    opt(
        '-s',
        '--submodules',
        action='store_true',
        short='Include submodules when refreshing patch contents',
    ),
    opt(
        '--no-submodules',
        action='store_false',
        dest='submodules',
        short='Exclude submodules when refreshing patch contents',
    ),
    opt(
        '--spill',
        action='store_true',
        dest='spill',
        short='Spill patch content to worktree and index, erasing patch content.',
    ),
]
options.extend(argparse.message_options(save_template=False))
options.extend(argparse.hook_options())
options.extend(argparse.trailer_options())
options.extend(argparse.author_options())
options.extend(argparse.diff_opts_option())

directory = DirectoryHasRepository()


def get_patch(stack, given_patch):
    """Get the name of the patch we are to refresh."""
    if given_patch:
        patch_name = given_patch
        if patch_name not in stack.patches:
            raise CmdException('%s: no such patch' % patch_name)
        return patch_name
    else:
        if not stack.patchorder.applied:
            raise CmdException(
                'Cannot refresh top patch because no patches are applied'
            )
        return stack.patchorder.applied[-1]


def list_files(stack, patch_name, path_limits, update, submodules):
    """Figure out which files to update."""
    iw = stack.repository.default_iw
    paths = iw.changed_files(stack.head.data.tree, path_limits or [])
    if update:
        # --update: Restrict update to the paths that were already part of the patch.
        patch_paths = set()
        commit = stack.patches[patch_name]
        for dt in stack.repository.diff_tree_files(
            commit.data.parent.data.tree, commit.data.tree
        ):
            _, _, _, _, _, oldname, newname = dt
            patch_paths.add(oldname)
            patch_paths.add(newname)
        paths &= patch_paths
    else:
        # Avoid including submodule files by default. This is to ensure that
        # users in repositories with submodueles do not accidentally include
        # submodule changes to patches just because they happen to have not
        # run "git submodule update" prior to running stg refresh. We won't
        # exclude them if we're explicitly told to include them, or if we're
        # given explicit paths.
        if not path_limits and not submodules:
            paths -= stack.repository.submodules(stack.head.data.tree)
    return paths


def write_tree(stack, paths, use_temp_index):
    """Possibly update the index, and then write its tree.

    If any path limiting is in effect, use a temp index.

    :return: The written tree.
    :rtype: :class:`Tree<stgit.git.Tree>`

    """

    def go(index):
        if paths:
            iw = IndexAndWorktree(index, stack.repository.default_worktree)
            iw.update_index(paths)
        return index.write_tree()

    if use_temp_index:
        with stack.repository.temp_index() as index:
            try:
                index.read_tree(stack.head)
                return go(index)
            finally:
                stack.repository.default_iw.update_index(paths)
    else:
        return go(stack.repository.default_index)


def make_temp_patch(stack, patch_name, tree):
    """Commit tree to temp patch, in a complete transaction."""
    commit = stack.repository.commit(
        CommitData(
            tree=tree,
            parents=[stack.head],
            message='Refresh of %s' % patch_name,
        )
    )
    temp_name = stack.patches.make_name('refresh-temp')
    trans = StackTransaction(stack)
    trans.patches[temp_name] = commit
    trans.applied.append(temp_name)
    return (
        trans.run(
            'refresh (create temporary patch)',
            stack.repository.default_iw,
            print_current_patch=False,
        ),
        temp_name,
    )


def absorb_applied(trans, iw, patch_name, temp_name, edit_fun):
    """Absorb temp patch into the given patch, which must be applied.

    If the absorption succeeds, call ``edit_fun`` on the resulting
    :class:`stgit.lib.git.CommitData` before committing it, then commit the return
    value.

    :returns: True if the temp patch is successfully absorbed or False if there remain
              conflicts for the user to resolve

    """
    temp_absorbed = False
    try:
        # Pop any patch on top of the patch we're refreshing.
        to_pop = trans.applied[trans.applied.index(patch_name) + 1 :]
        if len(to_pop) > 1:
            popped_extra = trans.pop_patches(lambda pn: pn in to_pop)
            assert not popped_extra  # no other patches were popped
            trans.push_patch(temp_name, iw)
        top_name = to_pop.pop()
        assert top_name == temp_name

        # Absorb the temp patch.
        temp_cd = trans.patches[temp_name].data
        assert trans.patches[patch_name] == temp_cd.parent
        cd, new_patch_name = edit_fun(
            trans.patches[patch_name].data.set_tree(temp_cd.tree)
        )
        trans.patches[patch_name] = trans.stack.repository.commit(cd)
        if new_patch_name and patch_name != new_patch_name:
            trans.rename_patch(patch_name, new_patch_name)

        popped_extra = trans.delete_patches(lambda pn: pn == temp_name, quiet=True)
        assert not popped_extra  # the temp patch was topmost
        temp_absorbed = True

        # Push back any patch we were forced to pop earlier.
        for pn in to_pop:
            trans.push_patch(pn, iw)
    except TransactionHalted:
        pass
    return temp_absorbed


def absorb_unapplied(trans, iw, patch_name, temp_name, edit_fun):
    """Absorb the temp patch into the given patch, which must be unapplied.

    If the absorption succeeds, call ``edit_fun`` on the resulting
    :class:`stgit.lib.git.CommitData` before committing it, then commit the return
    value.

    :param iw: Not used.
    :returns: True if the temp patch is absorbed successfully or False
              if user resolvable conflicts remain

    """
    # Pop the temp patch.
    popped_extra = trans.pop_patches(lambda pn: pn == temp_name)
    assert not popped_extra  # the temp patch was topmost

    # Try to create the new tree of the refreshed patch. (This is the
    # same operation as pushing the temp patch onto the patch we're
    # trying to refresh -- but we don't have a worktree to spill
    # conflicts to, so if the simple merge doesn't succeed, we have to
    # give up.)
    patch_cd = trans.patches[patch_name].data
    temp_cd = trans.patches[temp_name].data
    new_tree = trans.stack.repository.simple_merge(
        base=temp_cd.parent.data.tree,
        ours=patch_cd.tree,
        theirs=temp_cd.tree,
    )
    if new_tree:
        # It worked. Refresh the patch with the new tree, and delete
        # the temp patch.
        cd, new_patch_name = edit_fun(patch_cd.set_tree(new_tree))
        trans.patches[patch_name] = trans.stack.repository.commit(cd)
        if new_patch_name and patch_name != new_patch_name:
            trans.rename_patch(patch_name, new_patch_name)
        popped_extra = trans.delete_patches(lambda pn: pn == temp_name, quiet=True)
        assert not popped_extra  # the temp patch was not applied
        return True
    else:
        # Nope, we couldn't create the new tree, so we'll just have to
        # leave the temp patch for the user.
        return False


def absorb(stack, patch_name, temp_name, edit_fun, annotate=None):
    """Absorb the temp patch into the target patch."""
    log_msg = 'refresh'
    if annotate:
        log_msg += '\n\n' + annotate

    check_head_top_equal(stack)
    trans = StackTransaction(stack)
    iw = stack.repository.default_iw
    if patch_name in trans.applied:
        absorb_func = absorb_applied
    else:
        absorb_func = absorb_unapplied

    if absorb_func(trans, iw, patch_name, temp_name, edit_fun):

        def info_msg():
            pass

    else:

        def info_msg():
            out.warn(
                'The new changes did not apply cleanly to %s.' % patch_name,
                'They were saved in %s.' % temp_name,
            )

    r = trans.run(log_msg, iw)
    info_msg()
    return r


def __refresh_spill(annotate):
    stack = directory.repository.current_stack

    # Fetch the topmost patch.
    patchname = get_patch(stack, None)

    cd = stack.patches[patchname].data

    # Set the tree of the patch to the parent.
    cd = cd.set_tree(cd.parent.data.tree)

    log_msg = 'refresh (spill)'
    if annotate:
        log_msg += '\n\n' + annotate

    check_head_top_equal(stack)
    trans = StackTransaction(stack, allow_conflicts=True)
    trans.patches[patchname] = stack.repository.commit(cd)
    try:
        # Either a complete success, or a conflict during push. But in
        # either case, we've successfully effected the edits the user
        # asked us for.
        return trans.run(log_msg)
    except TransactionException:
        # Transaction aborted -- we couldn't check out files due to
        # dirty index/worktree. The edits were not carried out.
        out.error('Unable to spill the topmost patch')
        return utils.STGIT_COMMAND_ERROR


def __refresh(
    args,
    force=False,
    target_patch=None,
    message=None,
    author=None,
    trailers=None,
    annotate=None,
    use_temp_index=False,
    refresh_from_index=False,
    only_update_patchfiles=False,
    include_submodules=False,
    no_verify=False,
    invoke_editor=False,
    edit_diff=False,
    diff_flags=(),
):
    stack = directory.repository.current_stack

    patch_name = get_patch(stack, target_patch)

    if refresh_from_index:
        paths = set()
    else:
        paths = list_files(
            stack,
            patch_name,
            args,
            only_update_patchfiles,
            include_submodules,
        )

    # Make sure there are no conflicts in the files we want to
    # refresh.
    if stack.repository.default_index.conflicts() & paths:
        raise CmdException('Cannot refresh -- resolve conflicts first')

    # Make sure the index is clean before performing a full refresh
    if not refresh_from_index and not force:
        if not (
            stack.repository.default_index.is_clean(stack.head)
            or stack.repository.default_iw.worktree_clean()
        ):
            raise CmdException(
                'The index is dirty. Did you mean --index? '
                'To force a full refresh use --force.'
            )

    check_head_top_equal(stack)

    # Update index and write tree
    tree = write_tree(stack, paths, use_temp_index=use_temp_index)

    # Run pre-commit hook, if fails, abort refresh
    if not no_verify:
        pre_commit_hook = get_hook(
            stack.repository,
            'pre-commit',
            extra_env={} if invoke_editor else {'GIT_EDITOR': ':'},
        )
        if pre_commit_hook:
            try:
                pre_commit_hook()
            except RunException:
                raise CmdException(
                    'pre-commit hook failed, review the changes using `stg diff`, '
                    'run `stg add` to add them to index and run `stg refresh` again'
                )
            else:
                # Update index and rewrite tree if hook updated files in index
                if not stack.repository.default_index.is_clean(tree):
                    tree = write_tree(stack, paths, use_temp_index=use_temp_index)

    # Commit tree to temp patch, and absorb it into the target patch.
    retval, temp_name = make_temp_patch(stack, patch_name, tree)

    if retval != utils.STGIT_SUCCESS:
        return retval

    def edit_fun(cd):
        orig_msg = cd.message
        new_msg = None
        if message is not None:
            new_msg = message.encode(config.get('i18n.commitencoding'))
        cd = auto_edit_patch(
            stack.repository,
            cd,
            msg=new_msg,
            author=author,
            trailers=trailers,
        )
        new_patch_name = None
        if invoke_editor:
            cd, new_patch_name, failed_diff = interactive_edit_patch(
                stack.repository, cd, patch_name, edit_diff, diff_flags
            )
            assert not failed_diff
        if not no_verify and (invoke_editor or cd.message != orig_msg):
            cd = run_commit_msg_hook(stack.repository, cd, invoke_editor)
        # Refresh the committer information
        return cd.set_committer(None), new_patch_name

    return absorb(stack, patch_name, temp_name, edit_fun, annotate=annotate)


def func(parser, options, args):
    """Generate a new commit for the current or given patch."""

    if options.spill:
        if len(args) > 0:
            # TODO: would be nice if path limiting could be used with spill.
            raise CmdException('Cannot use path limiting with --spill')
        for opt_name, opt_value in [
            ('--index', options.index),
            ('--edit', options.edit),
            ('--update', options.update),
            ('--patch', options.patch),
            ('--force', options.force),
            ('--no-verify', options.no_verify),
            ('--sign', options.trailers),
            ('--ack', options.trailers),
            ('--review', options.trailers),
        ]:
            if opt_value:
                raise CmdException('Cannot combine --spill with %s' % opt_name)
        return __refresh_spill(annotate=options.annotate)
    else:
        # Catch illegal argument combinations.
        is_path_limiting = bool(args or options.update)
        if options.index and is_path_limiting:
            raise CmdException('Only full refresh is available with the --index option')

        if options.index and options.force:
            raise CmdException(
                'You cannot --force a full refresh when using --index mode'
            )

        if options.update and options.submodules:
            raise CmdException(
                '--submodules is meaningless when only updating modified files'
            )

        if options.index and options.submodules:
            raise CmdException(
                '--submodules is meaningless when keeping the current index'
            )

        # If submodules was not specified on the command line, infer a default
        # from configuration.
        if options.submodules is None:
            options.submodules = config.getbool('stgit.refreshsubmodules')

        return __refresh(
            args,
            force=options.force,
            target_patch=options.patch,
            message=options.message,
            author=options.author,
            trailers=options.trailers,
            annotate=options.annotate,
            use_temp_index=is_path_limiting,
            refresh_from_index=options.index,
            only_update_patchfiles=options.update,
            include_submodules=options.submodules,
            no_verify=options.no_verify,
            invoke_editor=options.edit,
            edit_diff=options.diff,
            diff_flags=options.diff_flags,
        )
