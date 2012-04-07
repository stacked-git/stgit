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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

import sys, os, time, re
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import argparse, stack, git, basedir
from stgit.lib import log

help = 'Branch operations: switch, list, create, rename, delete, ...'
kind = 'stack'
usage = ['',
         '[--merge] [--] <branch>',
         '--list',
         '--create [--] <new-branch> [<committish>]',
         '--clone [--] [<new-branch>]',
         '--rename [--] <old-name> <new-name>',
         '--protect [--] [<branch>]',
         '--unprotect [--] [<branch>]',
         '--delete [--force] [--] <branch>',
         '--cleanup [--force] [--] [<branch>]',
         '--description=<description> [--] [<branch>]']
description = """
Create, clone, switch between, rename, or delete development branches
within a git repository.

'stg branch'::
        Display the name of the current branch.

'stg branch' <branch>::
        Switch to the given branch."""

args = [argparse.all_branches]
options = [
    opt('-l', '--list', action = 'store_true',
        short = 'List the branches contained in this repository', long = """
        List each branch in the current repository, followed by its
        branch description (if any). The current branch is prefixed
        with '>'. Branches that have been initialized for StGit (with
        linkstg:init[]) are prefixed with 's'. Protected branches are
        prefixed with 'p'."""),
    opt('-c', '--create', action = 'store_true',
        short = 'Create (and switch to) a new branch', long = """
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
        committish)."""),
    opt('--clone', action = 'store_true',
        short = 'Clone the contents of the current branch', long = """
        Clone the current branch, under the name <new-branch> if
        specified, or using the current branch's name plus a
        timestamp.

        The description of the new branch is set to tell it is a clone
        of the current branch. The parent information of the new
        branch is copied from the current branch."""),
    opt('-r', '--rename', action = 'store_true',
        short = 'Rename an existing branch'),
    opt('-p', '--protect', action = 'store_true',
        short = 'Prevent StGit from modifying a branch', long = """
        Prevent StGit from modifying a branch -- either the current
        one, or one named on the command line."""),
    opt('-u', '--unprotect', action = 'store_true',
        short = 'Allow StGit to modify a branch', long = """
        Allow StGit to modify a branch -- either the current one, or
        one named on the command line. This undoes the effect of an
        earlier 'stg branch --protect' command."""),
    opt('--delete', action = 'store_true',
        short = 'Delete a branch', long = """
        Delete the named branch. If there are any patches left in the
        branch, StGit will refuse to delete it unless you give the
        '--force' flag.

        A protected branch cannot be deleted; it must be unprotected
        first (see '--unprotect' above).

        If you delete the current branch, you are switched to the
        "master" branch, if it exists."""),
    opt('--cleanup', action = 'store_true',
        short = 'Clean up the StGit metadata for a branch', long = """
        Remove the StGit information for the current or given branch. If there
        are patches left in the branch, StGit refuses the operation unless
        '--force' is given.

        A protected branch cannot be cleaned up; it must be unprotected first
        (see '--unprotect' above).

        A cleaned up branch can be re-initialised using the 'stg init'
        command."""),
    opt('-d', '--description', short = 'Set the branch description'),
    opt('--merge', action = 'store_true',
        short = 'Merge work tree changes into the other branch'),
    opt('--force', action = 'store_true',
        short = 'Force a delete when the series is not empty')]

directory = DirectoryGotoToplevel(log = False)

def __is_current_branch(branch_name):
    return crt_series.get_name() == branch_name

def __print_branch(branch_name, length):
    initialized = ' '
    current = ' '
    protected = ' '

    branch = stack.Series(branch_name)

    if branch.is_initialised():
        initialized = 's'
    if __is_current_branch(branch_name):
        current = '>'
    if branch.get_protected():
        protected = 'p'
    out.stdout(current + ' ' + initialized + protected + '\t'
               + branch_name.ljust(length) + '  | ' + branch.get_description())

def __delete_branch(doomed_name, force = False):
    doomed = stack.Series(doomed_name)

    if __is_current_branch(doomed_name):
        raise CmdException('Cannot delete the current branch')
    if doomed.get_protected():
        raise CmdException, 'This branch is protected. Delete is not permitted'

    out.start('Deleting branch "%s"' % doomed_name)
    doomed.delete(force)
    out.done()

def __cleanup_branch(name, force = False):
    branch = stack.Series(name)
    if branch.get_protected():
        raise CmdExcpetion('This branch is protected. Clean up is not permitted')

    out.start('Cleaning up branch "%s"' % name)
    branch.delete(force = force, cleanup = True)
    out.done()

def func(parser, options, args):

    if options.create:

        if len(args) == 0 or len(args) > 2:
            parser.error('incorrect number of arguments')

        check_local_changes()
        check_conflicts()
        check_head_top_equal(crt_series)

        tree_id = None
        if len(args) >= 2:
            parentbranch = None
            try:
                branchpoint = git.rev_parse(args[1])

                # parent branch?
                head_re = re.compile('refs/(heads|remotes)/')
                ref_re = re.compile(args[1] + '$')
                for ref in git.all_refs():
                    if head_re.match(ref) and ref_re.search(ref):
                        # args[1] is a valid ref from the branchpoint
                        # setting above
                        parentbranch = args[1]
                        break;
            except git.GitException:
                # should use a more specific exception to catch only
                # non-git refs ?
                out.info('Don\'t know how to determine parent branch'
                         ' from "%s"' % args[1])
                # exception in branch = rev_parse() leaves branchpoint unbound
                branchpoint = None

            tree_id = git_id(crt_series, branchpoint or args[1])

            if parentbranch:
                out.info('Recording "%s" as parent branch' % parentbranch)
            else:
                out.info('Don\'t know how to determine parent branch'
                         ' from "%s"' % args[1])                
        else:
            # branch stack off current branch
            parentbranch = git.get_head_file()

        if parentbranch:
            parentremote = git.identify_remote(parentbranch)
            if parentremote:
                out.info('Using remote "%s" to pull parent from'
                         % parentremote)
            else:
                out.info('Recording as a local branch')
        else:
            # no known parent branch, can't guess the remote
            parentremote = None

        stack.Series(args[0]).init(create_at = tree_id,
                                   parent_remote = parentremote,
                                   parent_branch = parentbranch)

        out.info('Branch "%s" created' % args[0])
        log.compat_log_entry('branch --create')
        return

    elif options.clone:

        if len(args) == 0:
            clone = crt_series.get_name() + \
                    time.strftime('-%C%y%m%d-%H%M%S')
        elif len(args) == 1:
            clone = args[0]
        else:
            parser.error('incorrect number of arguments')

        check_local_changes()
        check_conflicts()
        check_head_top_equal(crt_series)

        out.start('Cloning current branch to "%s"' % clone)
        crt_series.clone(clone)
        out.done()

        log.copy_log(log.default_repo(), crt_series.get_name(), clone,
                     'branch --clone')
        return

    elif options.delete:

        if len(args) != 1:
            parser.error('incorrect number of arguments')
        __delete_branch(args[0], options.force)
        log.delete_log(log.default_repo(), args[0])
        return

    elif options.cleanup:

        if not args:
            name = crt_series.get_name()
        elif len(args) == 1:
            name = args[0]
        else:
            parser.error('incorrect number of arguments')
        __cleanup_branch(name, options.force)
        log.delete_log(log.default_repo(), name)
        return

    elif options.list:

        if len(args) != 0:
            parser.error('incorrect number of arguments')

        branches = set(git.get_heads())
        for br in set(branches):
            m = re.match(r'^(.*)\.stgit$', br)
            if m and m.group(1) in branches:
                branches.remove(br)

        if branches:
            out.info('Available branches:')
            max_len = max([len(i) for i in branches])
            for i in sorted(branches):
                __print_branch(i, max_len)
        else:
            out.info('No branches')
        return

    elif options.protect:

        if len(args) == 0:
            branch_name = crt_series.get_name()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGIT' \
                  % branch_name

        out.start('Protecting branch "%s"' % branch_name)
        branch.protect()
        out.done()

        return

    elif options.rename:

        if len(args) != 2:
            parser.error('incorrect number of arguments')

        if __is_current_branch(args[0]):
            raise CmdException, 'Renaming the current branch is not supported'

        stack.Series(args[0]).rename(args[1])

        out.info('Renamed branch "%s" to "%s"' % (args[0], args[1]))
        log.rename_log(log.default_repo(), args[0], args[1], 'branch --rename')
        return

    elif options.unprotect:

        if len(args) == 0:
            branch_name = crt_series.get_name()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGIT' \
                  % branch_name

        out.info('Unprotecting branch "%s"' % branch_name)
        branch.unprotect()
        out.done()

        return

    elif options.description is not None:

        if len(args) == 0:
            branch_name = crt_series.get_name()
        elif len(args) == 1:
            branch_name = args[0]
        else:
            parser.error('incorrect number of arguments')
        branch = stack.Series(branch_name)

        if not branch.is_initialised():
            raise CmdException, 'Branch "%s" is not controlled by StGIT' \
                  % branch_name

        branch.set_description(options.description)

        return

    elif len(args) == 1:

        if __is_current_branch(args[0]):
            raise CmdException, 'Branch "%s" is already the current branch' \
                  % args[0]

        if not options.merge:
            check_local_changes()
        check_conflicts()
        check_head_top_equal(crt_series)

        out.start('Switching to branch "%s"' % args[0])
        git.switch_branch(args[0])
        out.done()
        return

    # default action: print the current branch
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    print crt_series.get_name()
