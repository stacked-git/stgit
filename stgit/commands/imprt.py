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


help = 'import a GNU diff file as a new patch'
usage = """%prog [options] [<file>]

Create a new patch and apply the given GNU diff file (or the standard
input). By default, the file name is used as the patch name but this
can be overriden with the '--name' option. The patch can either be a
normal file with the description at the top or it can have standard
mail format, the Subject, From and Date headers being used for
generating the patch information.

The patch description has to be separated from the data with a '---'
line. For a normal file, if no author information is given, the first
'Signed-off-by:' line is used."""

options = [make_option('-m', '--mail',
                       help = 'import the patch from a standard e-mail file',
                       action = 'store_true'),
           make_option('-n', '--name',
                       help = 'use NAME as the patch name'),
           make_option('-e', '--edit',
                       help = 'invoke an editor for the patch description',
                       action = 'store_true'),
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


def __parse_mail(filename = None):
    """Parse the input file in a mail format and return (description,
    authname, authemail, authdate)
    """
    if filename:
        f = file(filename)
    else:
        f = sys.stdin

    descr = authname = authemail = authdate = None

    # parse the headers
    for line in f:
        line = line.strip()
        if re.match('from:\s+', line, re.I):
            auth = re.findall('^.*?:\s+(.*)$', line)[0]
            authname, authemail = name_email(auth)
        elif re.match('date:\s+', line, re.I):
            authdate = re.findall('^.*?:\s+(.*)$', line)[0]
        elif re.match('subject:\s+', line, re.I):
            descr = re.findall('^.*?:\s+(.*)$', line)[0]
        elif line == '':
            # end of headers
            break

    # remove the '[*PATCH*]' expression in the subject
    if descr:
        descr = re.findall('^(\[[^\s]*PATCH.*?\])?\s*(.*)$', descr)[0][1]
        descr += '\n\n'
    else:
        raise CmdException, 'Subject: line not found'

    # the rest of the patch description
    for line in f:
        if re.match('---\s*$', line) or re.match('diff -', line):
            break
        else:
            descr += line
    descr.rstrip()

    if filename:
        f.close()

    return (descr, authname, authemail, authdate)

def __parse_patch(filename = None):
    """Parse the input file and return (description, authname,
    authemail, authdate)
    """
    if filename:
        f = file(filename)
    else:
        f = sys.stdin

    authname = authemail = authdate = None

    descr = ''
    for line in f:
        # the first 'Signed-of-by:' is the author
        if not authname and re.match('signed-off-by:\s+', line, re.I):
            auth = re.findall('^.*?:\s+(.*)$', line)[0]
            authname, authemail = name_email(auth)

        if re.match('---\s*$', line) or re.match('diff -', line):
            break
        else:
            descr += line
    descr.rstrip()

    if descr == '':
        descr = None

    if filename:
        f.close()

    return (descr, authname, authemail, authdate)

def func(parser, options, args):
    """Import a GNU diff file as a new patch
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    check_local_changes()
    check_conflicts()
    check_head_top_equal()

    if len(args) == 1:
        filename = args[0]
    else:
        filename = None

    if options.name:
        patch = options.name
    elif filename:
        patch = os.path.basename(filename)
    else:
        raise CmdException, 'Unkown patch name'

    # the defaults
    message = author_name = author_email = author_date = committer_name = \
              committer_email = None

    if options.author:
        options.authname, options.authemail = name_email(options.author)

    if options.mail:
        message, author_name, author_email, author_date = \
                 __parse_mail(filename)
    else:
        message, author_name, author_email, author_date = \
                 __parse_patch(filename)

    # new_patch() will invoke the editor in this case
    if not message:
        edit = False

    # override the automatically parsed settings
    if options.authname:
        author_name = options.authname
    if options.authemail:
        author_email = options.authemail
    if options.authdate:
        author_date = options.authdate
    if options.commname:
        committer_name = options.commname
    if options.commemail:
        committer_email = options.commemail

    crt_series.new_patch(patch, message = message,
                         author_name = author_name,
                         author_email = author_email,
                         author_date = author_date,
                         committer_name = committer_name,
                         committer_email = committer_email)

    print 'Importing patch %s...' % patch,
    sys.stdout.flush()

    git.apply_patch(filename)
    crt_series.refresh_patch(edit = options.edit,
                             show_patch = options.showpatch)

    print 'done'
    print_crt_patch()
