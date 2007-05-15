
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


help = 'create a new patch and make it the topmost one'
usage = """%prog [options] [name]

Create a new, empty patch and make it the topmost one. If the
'--message' option is not passed, an editor is invoked with the
.git/patchdescr.tmpl, ~/.stgit/templates/patchdescr.tmpl or
/usr/share/stgit/templates/patchdescr.tmpl file used a as template,
together with generated lines. By default, the local changes in the
working tree are not included in the patch. A 'refresh' command is
needed for this.

If no name is given for the new patch, one is generated from the first
line of the commit message."""

options = [make_option('-m', '--message',
                       help = 'use MESSAGE as the patch description'),
           make_option('-s', '--showpatch',
                       help = 'show the patch content in the editor buffer',
                       action = 'store_true'),
           make_option('-a', '--author', metavar = '"NAME <EMAIL>"',
                       help = 'use "NAME <EMAIL>" as the author details'),
           make_option('--authname',
                       help = 'use AUTHNAME as the author name'),
           make_option('--authemail',
                       help = 'use AUTHEMAIL as the author e-mail'),
           make_option('--authdate',
                       help = 'use AUTHDATE as the author date'),
           make_option('--commname',
                       help = 'use COMMNAME as the committer name'),
           make_option('--commemail',
                       help = 'use COMMEMAIL as the committer e-mail')]


def func(parser, options, args):
    """Creates a new patch
    """
    if len(args) == 0:
        name = None # autogenerate a name
    elif len(args) == 1:
        name = args[0]
    else:
        parser.error('incorrect number of arguments')

    check_conflicts()
    check_head_top_equal()

    if options.author:
        options.authname, options.authemail = name_email(options.author)

    crt_series.new_patch(name, message = options.message,
                         show_patch = options.showpatch,
                         author_name = options.authname,
                         author_email = options.authemail,
                         author_date = options.authdate,
                         committer_name = options.commname,
                         committer_email = options.commemail)
