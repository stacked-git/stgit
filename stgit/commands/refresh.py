
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
from stgit.config import config


help = 'generate a new commit for the current patch'
usage = '%prog [options]'

options = [make_option('-f', '--force',
                       help = 'force the refresh even if HEAD and '\
                       'top differ',
                       action = 'store_true'),
           make_option('-e', '--edit',
                       help = 'invoke an editor for the patch '\
                       'description',
                       action = 'store_true'),
           make_option('-m', '--message',
                       help = 'use MESSAGE as the patch ' \
                       'description'),
           make_option('--authname',
                       help = 'use AUTHNAME as the author name'),
           make_option('--authemail',
                       help = 'use AUTHEMAIL as the author e-mail'),
           make_option('--authdate',
                       help = 'use AUTHDATE as the author date'),
           make_option('--commname',
                       help = 'use COMMNAME as the committer name'),
           make_option('--commemail',
                       help = 'use COMMEMAIL as the committer ' \
                       'e-mail')]


def func(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    if config.has_option('stgit', 'autoresolved'):
        autoresolved = config.get('stgit', 'autoresolved')
    else:
        autoresolved = 'no'

    if autoresolved != 'yes':
        check_conflicts()

    patch = crt_series.get_current()
    if not patch:
        raise CmdException, 'No patches applied'

    if not options.force:
        check_head_top_equal()

    if git.local_changes() \
           or not crt_series.head_top_equal() \
           or options.edit or options.message \
           or options.authname or options.authemail or options.authdate \
           or options.commname or options.commemail:
        print 'Refreshing patch "%s"...' % patch,
        sys.stdout.flush()

        if autoresolved == 'yes':
            resolved_all()
        crt_series.refresh_patch(message = options.message,
                                 edit = options.edit,
                                 author_name = options.authname,
                                 author_email = options.authemail,
                                 author_date = options.authdate,
                                 committer_name = options.commname,
                                 committer_email = options.commemail)

        print 'done'
    else:
        print 'Patch "%s" is already up to date' % patch
