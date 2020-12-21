import time

from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    check_conflicts,
    check_head_top_equal,
    check_local_changes,
    git_commit,
)
from stgit.config import config
from stgit.exception import StackException
from stgit.lib import log
from stgit.lib.git.branch import Branch
from stgit.lib.git.repository import DetachedHeadException
from stgit.lib.stack import Stack, StackRepository
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out
from stgit.run import RunException

__copyright__ = """
Copyright (C) 2005, Chuck Lever <cel@netapp.com>

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

help = 'Branch operations: switch, list, create, rename, delete, ...'
kind = 'stack'
usage = [
    '',
    '[--merge] [--] <branch>',
    '--list',
    '--create [--] <new-branch> [<committish>]',
    '--clone [--] [<new-branch>]',
    '--rename [--] [<old-name>] <new-name>',
    '--protect [--] [<branch>]',
    '--unprotect [--] [<branch>]',
    '--delete [--force] [--] <branch>',
    '--cleanup [--force] [--] [<branch>]',
    '--description=<description> [--] [<branch>]',
]
description = """
Create, clone, switch between, rename, or delete development branches
within a git repository.

'stg branch'::
        Display the name of the current branch.

'stg branch' <branch>::
        Switch to the given branch."""

args = ['all_branches']
options = [
    opt(
        '-l',
        '--list',
        action='store_true',
        short='List the branches contained in this repository',
        long="""
        List each branch in the current repository, followed by its
        branch description (if any). The current branch is prefixed
        with '>'. Branches that have been initialized for StGit (with
        linkstg:init[]) are prefixed with 's'. Protected branches are
        prefixed with 'p'.""",
    ),
    opt(
        '-c',
        '--create',
        action='store_true',
        short='Create (and switch to) a new branch',
        long="""
        Create (and switch to) a new branch. The new branch is already
        initialized as an StGit patch stack, so you do not have to run
        linkstg:init[] manually. If you give a committish argument,
        the new branch is based there; otherwise, it is based at the
        current HEAD.

        StGit will try to detect the branch off of which the new
        branch is forked, as well as the remote repository from which
        that parent branch is taken (if any), so that running
        linkstg:pull[] will automatically pull new commits from the
        correct branch. It will warn if it cannot guess the parent
        branch (e.g. if you do not specify a branch name as
        committish).""",
    ),
    opt(
        '--clone',
        action='store_true',
        short='Clone the contents of the current branch',
        long="""
        Clone the current branch, under the name <new-branch> if
        specified, or using the current branch's name plus a
        timestamp.

        The description of the new branch is set to tell it is a clone
        of the current branch. The parent information of the new
        branch is copied from the current branch.""",
    ),
    opt(
        '-r',
        '--rename',
        action='store_true',
        short='Rename an existing branch',
    ),
    opt(
        '-p',
        '--protect',
        action='store_true',
        short='Prevent StGit from modifying a branch',
        long="""
        Prevent StGit from modifying a branch -- either the current
        one, or one named on the command line.""",
    ),
    opt(
        '-u',
        '--unprotect',
        action='store_true',
        short='Allow StGit to modify a branch',
        long="""
        Allow StGit to modify a branch -- either the current one, or
        one named on the command line. This undoes the effect of an
        earlier 'stg branch --protect' command.""",
    ),
    opt(
        '--delete',
        action='store_true',
        short='Delete a branch',
        long="""
        Delete the named branch. If there are any patches left in the
        branch, StGit will refuse to delete it unless you give the
        '--force' flag.

        A protected branch cannot be deleted; it must be unprotected
        first (see '--unprotect' above).

        If you delete the current branch, you are switched to the
        "master" branch, if it exists.""",
    ),
    opt(
        '--cleanup',
        action='store_true',
        short='Clean up the StGit metadata for a branch',
        long="""
        Remove the StGit information for the current or given branch. If there
        are patches left in the branch, StGit refuses the operation unless
        '--force' is given.

        A protected branch cannot be cleaned up; it must be unprotected first
        (see '--unprotect' above).

        A cleaned up branch can be re-initialised using the 'stg init'
        command.""",
    ),
    opt(
        '-d',
        '--description',
        short='Set the branch description',
    ),
    opt(
        '--merge',
        action='store_true',
        short='Merge work tree changes into the other branch',
    ),
    opt(
        '--force',
        action='store_true',
        short='Force a delete when the series is not empty',
    ),
]

directory = DirectoryGotoTopLevel()


def __is_current_branch(branch_name):
    try:
        return directory.repository.current_branch_name == branch_name
    except DetachedHeadException:
        return False


def __print_branch(branch_name, length):
    branch = Branch(directory.repository, branch_name)
    current = '>' if __is_current_branch(branch_name) else ' '
    try:
        stack = directory.repository.get_stack(branch_name)
    except StackException:
        initialised = protected = ' '
    else:
        initialised = 's'
        protected = 'p' if stack.protected else ' '

    out.stdout(
        current
        + ' '
        + initialised
        + protected
        + '\t'
        + branch_name.ljust(length)
        + '  | '
        + (branch.get_description() or '')
    )


def __delete_branch(doomed_name, force=False):
    if __is_current_branch(doomed_name):
        raise CmdException('Cannot delete the current branch')

    branch = Branch(directory.repository, doomed_name)
    try:
        stack = directory.repository.get_stack(doomed_name)
    except StackException:
        stack = None

    if stack:
        if stack.protected:
            raise CmdException('This branch is protected. Delete is not permitted')
        if not force and stack.patchorder.all:
            raise CmdException('Cannot delete: the series still contains patches')

    out.start('Deleting branch "%s"' % doomed_name)
    if stack:
        stack.cleanup()
    branch.delete()
    out.done()


def __cleanup_branch(name, force=False):
    stack = directory.repository.get_stack(name)
    if stack.protected:
        raise CmdException('This branch is protected. Clean up is not permitted')
    if not force and stack.patchorder.all:
        raise CmdException('Cannot clean up: the series still contains patches')

    out.start('Cleaning up branch "%s"' % name)
    stack.cleanup()
    out.done()


def __create_branch(branch_name, committish):
    repository = directory.repository

    branch_commit = None
    if committish is not None:
        parentbranch = None
        try:
            branchpoint = repository.run(
                ['git', 'rev-parse', '--symbolic-full-name', committish]
            ).output_one_line()

            if branchpoint.startswith('refs/heads/') or branchpoint.startswith(
                'refs/remotes/'
            ):
                # committish is a valid ref from the branchpoint setting above
                parentbranch = committish

        except RunException:
            out.info(
                'Do not know how to determine parent branch from "%s"' % committish
            )
            # exception in branch = rev_parse() leaves branchpoint unbound
            branchpoint = None

        branch_commit = git_commit(branchpoint or committish, repository)

        if parentbranch:
            out.info('Recording "%s" as parent branch' % parentbranch)
        else:
            out.info(
                'Do not know how to determine parent branch from "%s"' % committish
            )
    else:
        try:
            # branch stack off current branch
            parentbranch = repository.head_ref
        except DetachedHeadException:
            parentbranch = None

    if parentbranch:
        parentremote = config.get('branch.%s.remote' % parentbranch)
        if parentremote:
            out.info('Using remote "%s" to pull parent from' % parentremote)
        else:
            out.info('Recording as a local branch')
    else:
        # no known parent branch, can't guess the remote
        parentremote = None

    stack = Stack.create(
        repository,
        name=branch_name,
        create_at=branch_commit,
        parent_remote=parentremote,
        parent_branch=parentbranch,
        switch_to=True,
    )

    return stack


def func(parser, options, args):
    repository = directory.repository

    if options.create:
        if len(args) == 0 or len(args) > 2:
            parser.error('incorrect number of arguments')

        branch_name = args[0]
        committish = None if len(args) < 2 else args[1]

        if committish:
            check_local_changes(repository)
        check_conflicts(repository.default_iw)
        try:
            stack = repository.get_stack()
        except (DetachedHeadException, StackException):
            pass
        else:
            check_head_top_equal(stack)

        stack = __create_branch(branch_name, committish)

        out.info('Branch "%s" created' % branch_name)
        log.log_entry(stack, 'branch --create %s' % stack.name)
        return

    elif options.clone:

        cur_branch = Branch(repository, repository.current_branch_name)
        if len(args) == 0:
            clone_name = cur_branch.name + time.strftime('-%C%y%m%d-%H%M%S')
        elif len(args) == 1:
            clone_name = args[0]
        else:
            parser.error('incorrect number of arguments')

        check_local_changes(repository)
        check_conflicts(repository.default_iw)
        try:
            stack = repository.current_stack
        except StackException:
            stack = None
            base = repository.refs.get(repository.head_ref)
        else:
            check_head_top_equal(stack)
            base = stack.base

        out.start('Cloning current branch to "%s"' % clone_name)
        clone = Stack.create(
            repository,
            name=clone_name,
            create_at=base,
            parent_remote=cur_branch.parent_remote,
            parent_branch=cur_branch.name,
        )
        if stack:
            for pn in stack.patchorder.all_visible:
                patch = stack.patches.get(pn)
                clone.patches.new(pn, patch.commit, 'clone %s' % stack.name)
            clone.patchorder.set_order(
                applied=[], unapplied=stack.patchorder.all_visible, hidden=[]
            )
            trans = StackTransaction(clone, 'clone')
            try:
                for pn in stack.patchorder.applied:
                    trans.push_patch(pn)
            except TransactionHalted:
                pass
            trans.run()
        prefix = 'branch.%s.' % cur_branch.name
        new_prefix = 'branch.%s.' % clone.name
        for n, v in list(config.getstartswith(prefix)):
            config.set(n.replace(prefix, new_prefix, 1), v)
        clone.set_description('clone of "%s"' % cur_branch.name)
        clone.switch_to()
        out.done()

        log.copy_log(
            StackRepository.default(), cur_branch.name, clone.name, 'branch --clone'
        )
        return

    elif options.delete:

        if len(args) != 1:
            parser.error('incorrect number of arguments')
        __delete_branch(args[0], options.force)
        log.delete_log(StackRepository.default(), args[0])
        return

    elif options.cleanup:

        if not args:
            name = repository.current_branch_name
        elif len(args) == 1:
            name = args[0]
        else:
            parser.error('incorrect number of arguments')
        __cleanup_branch(name, options.force)
        log.delete_log(StackRepository.default(), name)
        return

    elif options.list:

        if len(args) != 0:
            parser.error('incorrect number of arguments')

        branch_names = sorted(
            ref.replace('refs/heads/', '', 1)
            for ref in repository.refs
            if ref.startswith('refs/heads/') and not ref.endswith('.stgit')
        )

        if branch_names:
            out.info('Available branches:')
            max_len = max(len(name) for name in branch_names)
            for branch_name in branch_names:
                __print_branch(branch_name, max_len)
        else:
            out.info('No branches')
        return

    elif options.protect:

        if len(args) == 0:
            branch_name = repository.current_branch_name
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')

        try:
            stack = repository.get_stack(branch_name)
        except StackException:
            raise CmdException('Branch "%s" is not controlled by StGIT' % branch_name)

        out.start('Protecting branch "%s"' % branch_name)
        stack.protected = True
        out.done()

        return

    elif options.rename:

        if len(args) == 1:
            stack = repository.current_stack
            new_name = args[0]
        elif len(args) == 2:
            stack = repository.get_stack(args[0])
            new_name = args[1]
        else:
            parser.error('incorrect number of arguments')

        old_name = stack.name
        stack.rename(new_name)

        out.info('Renamed branch "%s" to "%s"' % (old_name, new_name))
        return

    elif options.unprotect:

        if len(args) == 0:
            branch_name = repository.current_branch_name
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')

        try:
            stack = repository.get_stack(branch_name)
        except StackException:
            raise CmdException('Branch "%s" is not controlled by StGIT' % branch_name)

        out.info('Unprotecting branch "%s"' % branch_name)
        stack.protected = False
        out.done()

        return

    elif options.description is not None:

        if len(args) == 0:
            branch_name = repository.current_branch_name
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')

        Branch(repository, branch_name).set_description(options.description)
        return

    elif len(args) == 1:
        branch_name = args[0]
        if branch_name == repository.current_branch_name:
            raise CmdException(
                'Branch "%s" is already the current branch' % branch_name
            )

        if not options.merge:
            check_local_changes(repository)
        check_conflicts(repository.default_iw)
        try:
            stack = repository.get_stack()
        except StackException:
            pass
        else:
            check_head_top_equal(stack)

        out.start('Switching to branch "%s"' % branch_name)
        Branch(repository, branch_name).switch_to()
        out.done()
        return

    # default action: print the current branch
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    out.stdout(directory.repository.current_branch_name)
