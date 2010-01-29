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
from stgit.config import GitConfigException
from stgit import argparse, stack, git

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

args = [argparse.repo]
options = [
    opt('-n', '--nopush', action = 'store_true',
        short = 'Do not push the patches back after pulling'),
    opt('-m', '--merged', action = 'store_true',
        short = 'Check for patches merged upstream')]

directory = DirectoryGotoToplevel(log = True)

def func(parser, options, args):
    """Pull the changes from a remote repository
    """
    policy = config.get('branch.%s.stgit.pull-policy' % crt_series.get_name()) or \
             config.get('stgit.pull-policy')

    if policy == 'rebase':
        # parent is local
        if len(args) == 1:
            parser.error('specifying a repository is meaningless for policy="%s"' % policy)
        if len(args) > 0:
            parser.error('incorrect number of arguments')

    else:
        # parent is remote
        if len(args) > 1:
            parser.error('incorrect number of arguments')

        if len(args) >= 1:
            repository = args[0]
        else:
            repository = crt_series.get_parent_remote()

    if crt_series.get_protected():
        raise CmdException, 'This branch is protected. Pulls are not permitted'

    check_local_changes()
    check_conflicts()
    check_head_top_equal(crt_series)

    if policy not in ['pull', 'fetch-rebase', 'rebase']:
        raise GitConfigException, 'Unsupported pull-policy "%s"' % policy

    applied = prepare_rebase(crt_series)

    # pull the remote changes
    if policy == 'pull':
        out.info('Pulling from "%s"' % repository)
        git.pull(repository)
    elif policy == 'fetch-rebase':
        out.info('Fetching from "%s"' % repository)
        git.fetch(repository)
        try:
            target = git.fetch_head()
        except git.GitException:
            out.error('Could not find the remote head to rebase onto - fix branch.%s.merge in .git/config' % crt_series.get_name())
            out.error('Pushing any patches back...')
            post_rebase(crt_series, applied, False, False)
            raise

        rebase(crt_series, target)
    elif policy == 'rebase':
        rebase(crt_series, crt_series.get_parent_branch())

    post_rebase(crt_series, applied, options.nopush, options.merged)

    # maybe tidy up
    if config.get('stgit.keepoptimized') == 'yes':
        git.repack()

    print_crt_patch(crt_series)
