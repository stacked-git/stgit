from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    git_commit,
    post_rebase,
    prepare_rebase,
    rebase,
)
from stgit.config import GitConfigException, config
from stgit.lib.git import MergeConflictException, RepositoryException
from stgit.out import out

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

help = 'Pull changes from a remote repository'
kind = 'stack'
usage = ['[options] [--] [<repository>]']
description = """
Pull the latest changes from the given remote repository (defaulting
to branch.<name>.remote, or 'origin' if not set). This command works
by popping all the patches from the stack, pulling the changes in the
parent repository, setting the base of the stack to the latest parent
HEAD and pushing the patches back (unless '--nopush' is specified).
The 'push' operation can fail if there are conflicts. They need to be
resolved and the patch pushed again.

Check the 'git fetch' documentation for the <repository> format."""

args = ['repo']
options = [
    opt(
        '-n',
        '--nopush',
        action='store_true',
        short='Do not push the patches back after pulling',
    ),
    opt(
        '-m',
        '--merged',
        action='store_true',
        short='Check for patches merged upstream',
    ),
]

directory = DirectoryGotoTopLevel()


def pull(repository, remote_repository):
    """Pull changes from remote repository using 'git pull' by default."""
    command = (
        config.get('branch.%s.stgit.pullcmd' % repository.current_branch_name)
        or config.get('stgit.pullcmd')
    ).split()
    command.append(remote_repository)
    runner = repository.run(command)

    # Assumption: pullcmd will return 1 for a conflict and some non-1 code for
    # other errors. This is probably an incorrect assumption because pullcmd
    # may not be `git pull` and even `git pull` does not make any clear
    # guarantees about its return code. However, the consequence of any of this
    # going wrong will be that `stg pull` will return 3 (conflict) when 2
    # (generic error) might be more correct.
    runner.returns([0, 1]).run()
    if runner.exitcode:
        raise MergeConflictException([])

    repository.refs.reset_cache()


def fetch(repository, remote_repository):
    """Fetch changes from remote repository using 'git fetch' by default"""
    command = (
        config.get('branch.%s.stgit.fetchcmd' % repository.current_branch_name)
        or config.get('stgit.fetchcmd')
    ).split()
    command.append(remote_repository)
    repository.run(command).run()


def func(parser, options, args):
    """Pull the changes from a remote repository
    """
    repository = directory.repository
    iw = repository.default_iw
    stack = repository.get_stack()
    policy = config.get('branch.%s.stgit.pull-policy' % stack.name) or config.get(
        'stgit.pull-policy'
    )

    if policy == 'rebase':
        # parent is local
        if len(args) == 1:
            parser.error(
                'specifying a repository is meaningless for policy="%s"' % (policy,)
            )
        elif len(args) > 0:
            parser.error('incorrect number of arguments')
    else:
        # parent is remote
        if len(args) > 1:
            parser.error('incorrect number of arguments')

        if len(args) >= 1:
            remote_name = args[0]
        else:
            remote_name = stack.parent_remote

    if stack.protected:
        raise CmdException('This branch is protected. Pulls are not permitted')

    if policy not in ['pull', 'fetch-rebase', 'rebase']:
        raise GitConfigException('Unsupported pull-policy "%s"' % policy)

    applied = stack.patchorder.applied

    retval = prepare_rebase(stack, 'pull')
    if retval:
        return retval

    # pull the remote changes
    if policy == 'pull':
        out.info('Pulling from "%s"' % remote_name)
        pull(repository, remote_name)
    elif policy == 'fetch-rebase':
        out.info('Fetching from "%s"' % remote_name)
        fetch(repository, remote_name)
        try:
            target = repository.rev_parse('FETCH_HEAD')
        except RepositoryException:
            out.error(
                'Could not find the remote head to rebase onto - '
                'fix branch.%s.merge in .git/config' % stack.name
            )
            out.error('Pushing any patches back...')
            post_rebase(stack, applied, 'pull', check_merged=False)
            raise

        rebase(stack, iw, target)
    elif policy == 'rebase':
        value = config.get('branch.%s.stgit.parentbranch' % stack.name)
        if value:
            parent_commit = git_commit(value, repository)
        else:
            try:
                parent_commit = repository.rev_parse('heads/origin')
            except RepositoryException:
                raise CmdException('Cannot find a parent branch for "%s"' % stack.name)
            else:
                out.warn(
                    'No parent branch declared for stack "%s", defaulting to'
                    '"heads/origin".' % stack.name,
                    'Consider setting "branch.%s.stgit.parentbranch" with '
                    '"git config".' % stack.name,
                )
        rebase(stack, iw, parent_commit)

    if not options.nopush:
        post_rebase(stack, applied, 'pull', check_merged=options.merged)

    # maybe tidy up
    if config.getbool('stgit.keepoptimized'):
        repository.repack()
