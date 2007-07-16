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

import sys, os, time
from optparse import OptionParser, make_option
from pydoc import pager
from stgit.commands.common import *
from stgit import stack, git

help = 'display the patch changelog'
usage = """%prog [options] [patch]

List all the current and past commit ids of the given patch. The
--graphical option invokes gitk instead of printing. The changelog
commit messages have the form '<action> <new-patch-id>'. The <action>
can be one of the following:

  new     - new patch created
  refresh - local changes were added to the patch
  push    - the patch was cleanly pushed onto the stack
  push(m) - the patch was pushed onto the stack with a three-way merge
  push(f) - the patch was fast-forwarded
  undo    - the patch boundaries were restored to the old values

Note that only the diffs shown in the 'refresh', 'undo' and 'sync'
actions are meaningful for the patch changes. The 'push' actions
represent the changes to the entire base of the current
patch. Conflicts reset the patch content and a subsequent 'refresh'
will show the entire patch."""

options = [make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-p', '--patch',
                       help = 'show the refresh diffs',
                       action = 'store_true'),
           make_option('-f', '--full',
                       help = 'show the full commit ids',
                       action = 'store_true'),
           make_option('-g', '--graphical',
                       help = 'run gitk instead of printing',
                       action = 'store_true')]

def show_log(log, options):
    """List the patch changelog
    """
    commit = git.get_commit(log)
    diff_str = ''
    while commit:
        log = commit.get_log().split('\n')

        cmd_rev = log[0].split()
        if len(cmd_rev) >= 2:
            cmd = cmd_rev[0]
            rev = cmd_rev[1]
        elif len(cmd_rev) == 1:
            cmd = cmd_rev[0]
            rev = ''
        else:
            cmd = rev = ''

        if options.patch:
            if cmd in ['refresh', 'undo', 'sync']:
                diff_str = '%s%s\n' % (diff_str,
                                       git.pretty_commit(commit.get_id_hash()))
        else:
            if len(log) >= 3:
                notes = log[2]
            else:
                notes = ''
            author_name, author_email, author_date = \
                         name_email_date(commit.get_author())
            secs, tz = author_date.split()
            date = '%s %s' % (time.ctime(int(secs)), tz)

            if options.full:
                out.stdout('%-7s %-40s %s' % (cmd[:7], rev[:40], date))
            else:
                out.stdout('%-8s [%-7s] %-28s  %s' % \
                           (rev[:8], cmd[:7], notes[:28], date))

        parent = commit.get_parent()
        if parent:
            commit = git.get_commit(parent)
        else:
            commit = None

    if options.patch and diff_str:
        pager(diff_str.rstrip())

def func(parser, options, args):
    """Show the patch changelog
    """
    if len(args) == 0:
        name = crt_series.get_current()
        if not name:
            raise CmdException, 'No patches applied'
    elif len(args) == 1:
        name = args[0]
        if not name in crt_series.get_applied() + crt_series.get_unapplied() + \
               crt_series.get_hidden():
            raise CmdException, 'Unknown patch "%s"' % name
    else:
        parser.error('incorrect number of arguments')

    patch = crt_series.get_patch(name)

    log = patch.get_log()
    if not log:
        raise CmdException, 'No changelog for patch "%s"' % name

    if options.graphical:
        if os.system('gitk %s' % log) != 0:
            raise CmdException, 'gitk execution failed'
    else:
        show_log(log, options)
