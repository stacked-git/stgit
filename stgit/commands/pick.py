from stgit.argparse import opt, patch_range
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    check_conflicts,
    check_head_top_equal,
    check_local_changes,
    git_commit,
    parse_patches,
    parse_rev,
    print_current_patch,
)
from stgit.config import config
from stgit.lib.git import CommitData, MergeConflictException, MergeException, Person
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out
from stgit.run import Run
from stgit.utils import STGIT_CONFLICT, STGIT_SUCCESS

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

help = 'Import a patch from a different branch or a commit object'
kind = 'patch'
usage = ['[options] [--] ([<patch1>] [<patch2>] [<patch3>..<patch4>])|<commit>']
description = """
Import one or more patches from a different branch or a commit object
into the current series. By default, the name of the imported patch is
used as the name of the current patch. It can be overridden with the
'--name' option. A commit object can be reverted with the '--revert'
option. The log and author information are those of the commit
object.

When using the '--expose' option, the format of the commit message is
determined by the 'stgit.pick.expose-format' configuration option. This option
is a format string as may supplied as the '--pretty' option to
linkgit:git-show[1]. The default is "format:%B%n(imported from commit %H)",
which appends the commit hash of the picked commit to the patch's commit
message.
"""

args = [patch_range('applied_patches', 'unapplied_patches', 'hidden_patches')]
options = [
    opt(
        '-n',
        '--name',
        short='Use NAME as the patch name',
    ),
    opt(
        '-B',
        '--ref-branch',
        args=['stg_branches'],
        short='Pick patches from BRANCH',
    ),
    opt(
        '-r',
        '--revert',
        action='store_true',
        short='Revert the given commit object',
    ),
    opt(
        '-p',
        '--parent',
        metavar='COMMITID',
        args=['commit'],
        short='Use COMMITID as parent',
    ),
    opt(
        '-x',
        '--expose',
        action='store_true',
        short='Append the imported commit id to the patch log',
    ),
    opt(
        '--fold',
        action='store_true',
        short='Fold the commit object into the current patch',
    ),
    opt(
        '--update',
        action='store_true',
        short='Like fold but only update the current patch files',
    ),
    opt(
        '-f',
        '--file',
        action='append',
        short='Only fold the given file (can be used multiple times)',
    ),
    opt(
        '--unapplied',
        action='store_true',
        short='Keep the patch unapplied',
    ),
]

directory = DirectoryGotoTopLevel()


def __pick_commit(stack, ref_stack, iw, commit, patchname, options):
    """Pick a commit."""
    repository = stack.repository

    if options.name:
        patchname = options.name
    elif patchname and options.revert:
        patchname = 'revert-' + patchname

    if patchname:
        patchname = stack.patches.make_name(patchname, lower=False)
    else:
        patchname = stack.patches.make_name(commit.data.message_str)

    if options.parent:
        parent = git_commit(options.parent, repository, ref_stack.name)
    else:
        parent = commit.data.parent

    if not options.revert:
        bottom = parent
        top = commit
    else:
        bottom = commit
        top = parent

    if options.fold:
        out.start('Folding commit %s' % commit.sha1)

        diff = repository.diff_tree(
            bottom.data.tree, top.data.tree, pathlimits=options.file
        )

        if diff:
            try:
                # try a direct git apply first
                iw.apply(diff, quiet=True)
            except MergeException:
                if options.file:
                    out.done('conflict(s)')
                    out.error('%s does not apply cleanly' % patchname)
                    return STGIT_CONFLICT
                else:
                    try:
                        iw.merge(
                            bottom.data.tree,
                            stack.head.data.tree,
                            top.data.tree,
                        )
                    except MergeConflictException as e:
                        out.done('%s conflicts' % len(e.conflicts))
                        out.error('%s does not apply cleanly' % patchname, *e.conflicts)
                        return STGIT_CONFLICT
            out.done()
        else:
            out.done('no changes')
        return STGIT_SUCCESS
    elif options.update:
        files = [
            fn1
            for _, _, _, _, _, fn1, fn2 in repository.diff_tree_files(
                stack.top.data.parent.data.tree, stack.top.data.tree
            )
        ]

        diff = repository.diff_tree(bottom.data.tree, top.data.tree, pathlimits=files)

        out.start('Updating with commit %s' % commit.sha1)

        try:
            iw.apply(diff, quiet=True)
        except MergeException:
            out.done('conflict(s)')
            out.error('%s does not apply cleanly' % patchname)
            return STGIT_CONFLICT
        else:
            out.done()
            return STGIT_SUCCESS
    else:
        author = commit.data.author
        message = commit.data.message_str

        if options.revert:
            author = Person.author()
            if message:
                lines = message.splitlines()
                subject = lines[0]
                body = '\n'.join(lines[2:])
            else:
                subject = commit.sha1
                body = ''
            message = 'Revert "%s"\n\nThis reverts commit %s.\n\n%s\n' % (
                subject,
                commit.sha1,
                body,
            )
        elif options.expose:
            fmt = config.get('stgit.pick.expose-format')
            message = Run(
                'git', 'show', '--no-patch', '--pretty=' + fmt, commit.sha1
            ).raw_output()
            message = message.rstrip() + '\n'

        out.start('Importing commit %s' % commit.sha1)

        new_commit = repository.commit(
            CommitData(
                tree=top.data.tree,
                parents=[bottom],
                message=message,
                author=author,
            )
        )

        if not options.unapplied:
            check_head_top_equal(stack)

        trans = StackTransaction(stack)
        trans.patches[patchname] = new_commit

        trans.unapplied.append(patchname)
        if not options.unapplied:
            try:
                trans.push_patch(patchname, iw, allow_interactive=True)
            except TransactionHalted:
                pass

        retval = trans.run(
            'pick %s from %s' % (patchname, ref_stack.name),
            iw,
            print_current_patch=False,
        )

        if retval == STGIT_CONFLICT:
            out.done('conflict(s)')
        elif stack.patches[patchname].data.is_nochange():
            out.done('empty patch')
        else:
            out.done()

        return retval


def func(parser, options, args):
    """Import a commit object as a new patch"""
    if not args:
        parser.error('incorrect number of arguments')

    if options.file and not options.fold:
        parser.error('--file can only be specified with --fold')

    repository = directory.repository
    stack = repository.get_stack()
    iw = repository.default_iw

    if not options.unapplied:
        check_local_changes(repository)
        check_conflicts(iw)
        check_head_top_equal(stack)

    if options.ref_branch:
        ref_stack = repository.get_stack(options.ref_branch)
    else:
        ref_stack = stack

    try:
        patches = parse_patches(
            args,
            ref_stack.patchorder.all_visible,
            len(ref_stack.patchorder.applied),
        )
        commit = None
    except CmdException:
        if len(args) > 1:
            raise

        branch, patch = parse_rev(args[0])

        if not branch:
            commit = git_commit(patch, repository, options.ref_branch)
            patches = []
        else:
            ref_stack = repository.get_stack(branch)
            patches = parse_patches(
                [patch],
                ref_stack.patchorder.all_visible,
                len(ref_stack.patchorder.applied),
            )
            commit = None

    if not commit and len(patches) > 1:
        if options.name:
            raise CmdException('--name can only be specified with one patch')
        if options.parent:
            raise CmdException('--parent can only be specified with one patch')

    if options.update and not stack.patchorder.applied:
        raise CmdException('No patches applied')

    if commit:
        patchname = None
        retval = __pick_commit(stack, ref_stack, iw, commit, patchname, options)
    else:
        if options.unapplied:
            patches.reverse()
        for patchname in patches:
            commit = git_commit(patchname, repository, ref_stack.name)
            retval = __pick_commit(stack, ref_stack, iw, commit, patchname, options)
            if retval != STGIT_SUCCESS:
                break

    if retval == STGIT_SUCCESS:
        print_current_patch(stack)

    return retval
