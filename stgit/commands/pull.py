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
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'pull the changes from the remote repository'
usage = """%prog [options]

Pull the latest changes from the parent repository. This command works
by popping all the patches from the stack, pulling the changes in the
parent repository, setting the base of the stack to the latest parent
HEAD and pusing the patches back (unless '--nopush' is specified). The
'push' operation can fail if there are conflicts. They need to be
resolved and the patch pushed again. The URL of the parent repository
is specified in the .git/branches/<head> file.

Note that this command doesn't perform any merge operation for the
base of the stack, it only performs merges with the patches being
pushed."""

options = [make_option('-n', '--nopush',
                       help = 'do not push the patches back after pulling',
                       action = 'store_true'),
           make_option('--head', metavar='OTHER_HEAD',
                       help = 'pull OTHER_HEAD instead of HEAD'),
           make_option('--tag',
                       help = 'pull TAG')]


def func(parser, options, args):
    """Pull the changes from a remote repository
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    branch = git.get_head_file()
    location = read_string(os.path.join(git.base_dir, 'branches', branch))

    print 'Pulling from "%s"...' % location
    new_head = git.fetch(location, options.head, options.tag)
    print 'done'

    if new_head == git_id('base'):
        print 'Branch already up-to-date'
    else:
        applied = crt_series.get_applied()

        if len(applied) > 0:
            print 'Popping all patches...',
            sys.stdout.flush()
            crt_series.pop_patch(applied[0])
            print 'done'

        git.switch(new_head)

        if options.nopush:
            applied = []
        for p in applied:
            print 'Pushing patch "%s"...' % p,
            sys.stdout.flush()
            crt_series.push_patch(p)
            if crt_series.empty_patch(p):
                print 'done (empty patch)'
            else:
                print 'done'

    print_crt_patch()
