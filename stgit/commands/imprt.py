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

import sys, os, re
from email.Header import decode_header, make_header
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'import a GNU diff file as a new patch'
usage = """%prog [options] [<file>]

Create a new patch and apply the given GNU diff file (or the standard
input). By default, the file name is used as the patch name but this
can be overridden with the '--name' option. The patch can either be a
normal file with the description at the top or it can have standard
mail format, the Subject, From and Date headers being used for
generating the patch information.

The patch description has to be separated from the data with a '---'
line."""

options = [make_option('-m', '--mail',
                       help = 'import the patch from a standard e-mail file',
                       action = 'store_true'),
           make_option('-n', '--name',
                       help = 'use NAME as the patch name'),
           make_option('-t', '--strip',
                       help = 'strip numbering and extension from patch name',
                       action = 'store_true'),
           make_option('-s', '--series',
                       help = 'import a series of patches',
                       action = 'store_true'),
           make_option('-i', '--ignore',
                       help = 'ignore the applied patches in the series',
                       action = 'store_true'),
           make_option('--replace',
                       help = 'replace the unapplied patches in the series',
                       action = 'store_true'),
           make_option('-b', '--base',
                       help = 'use BASE instead of HEAD for file importing'),
           make_option('-e', '--edit',
                       help = 'invoke an editor for the patch description',
                       action = 'store_true'),
           make_option('-p', '--showpatch',
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


def __end_descr(line):
    return re.match('---\s*$', line) or re.match('diff -', line) or \
            re.match('Index: ', line)

def __strip_patch_name(name):
    stripped = re.sub('^[0-9]+-(.*)$', '\g<1>', name)
    stripped = re.sub('^(.*)\.(diff|patch)$', '\g<1>', stripped)

    return stripped

def __replace_slashes_with_dashes(name):
    stripped = name.replace('/', '-')

    return stripped

def __parse_description(descr):
    """Parse the patch description and return the new description and
    author information (if any).
    """
    subject = body = ''
    authname = authemail = authdate = None

    descr_lines = [line.rstrip() for line in  descr.split('\n')]
    if not descr_lines:
        raise CmdException, "Empty patch description"

    lasthdr = 0
    end = len(descr_lines)

    # Parse the patch header
    for pos in range(0, end):
        if not descr_lines[pos]:
           continue
        # check for a "From|Author:" line
        if re.match('\s*(?:from|author):\s+', descr_lines[pos], re.I):
            auth = re.findall('^.*?:\s+(.*)$', descr_lines[pos])[0]
            authname, authemail = name_email(auth)
            lasthdr = pos + 1
            continue
        # check for a "Date:" line
        if re.match('\s*date:\s+', descr_lines[pos], re.I):
            authdate = re.findall('^.*?:\s+(.*)$', descr_lines[pos])[0]
            lasthdr = pos + 1
            continue
        if subject:
            break
        # get the subject
        subject = descr_lines[pos]
        lasthdr = pos + 1

    # get the body
    if lasthdr < end:
        body = reduce(lambda x, y: x + '\n' + y, descr_lines[lasthdr:], '')

    return (subject + body, authname, authemail, authdate)

def __parse_mail(filename = None):
    """Parse the input file in a mail format and return (description,
    authname, authemail, authdate)
    """
    def __decode_header(header):
        """Decode a qp-encoded e-mail header as per rfc2047"""
        try:
            words_enc = decode_header(header)
            hobj = make_header(words_enc)
        except Exception, ex:
            raise CmdException, 'header decoding error: %s' % str(ex)
        return unicode(hobj).encode('utf-8')

    if filename:
        f = file(filename)
    else:
        f = sys.stdin

    descr = authname = authemail = authdate = None

    # parse the headers
    while True:
        line = f.readline()
        if not line:
            break
        line = line.strip()
        if re.match('from:\s+', line, re.I):
            auth = __decode_header(re.findall('^.*?:\s+(.*)$', line)[0])
            authname, authemail = name_email(auth)
        elif re.match('date:\s+', line, re.I):
            authdate = re.findall('^.*?:\s+(.*)$', line)[0]
        elif re.match('subject:\s+', line, re.I):
            descr = __decode_header(re.findall('^.*?:\s+(.*)$', line)[0])
        elif line == '':
            # end of headers
            break

    # remove the '[*PATCH*]' expression in the subject
    if descr:
        descr = re.findall('^(\[[^\s]*[Pp][Aa][Tt][Cc][Hh].*?\])?\s*(.*)$',
                           descr)[0][1]
        descr += '\n\n'
    else:
        raise CmdException, 'Subject: line not found'

    # the rest of the patch description
    while True:
        line = f.readline()
        if not line:
            break
        if __end_descr(line):
            break
        else:
            descr += line
    descr.rstrip()

    if filename:
        f.close()

    # parse the description for author information
    descr, descr_authname, descr_authemail, descr_authdate = __parse_description(descr)
    if descr_authname:
        authname = descr_authname
    if descr_authemail:
        authemail = descr_authemail
    if descr_authdate:
       authdate = descr_authdate

    return (descr, authname, authemail, authdate)

def __parse_patch(filename = None):
    """Parse the input file and return (description, authname,
    authemail, authdate)
    """
    if filename:
        f = file(filename)
    else:
        f = sys.stdin

    descr = ''
    while True:
        line = f.readline()
        if not line:
            break

        if __end_descr(line):
            break
        else:
            descr += line
    descr.rstrip()

    if filename:
        f.close()

    descr, authname, authemail, authdate = __parse_description(descr)

    # we don't yet have an agreed place for the creation date.
    # Just return None
    return (descr, authname, authemail, authdate)

def __import_patch(patch, filename, options):
    """Import a patch from a file or standard input
    """
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

    if not patch:
        patch = make_patch_name(message, crt_series.patch_exists)

    # refresh_patch() will invoke the editor in this case, with correct
    # patch content
    if not message:
        can_edit = False

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

    if options.replace and patch in crt_series.get_unapplied():
        crt_series.delete_patch(patch)

    crt_series.new_patch(patch, message = message, can_edit = False,
                         author_name = author_name,
                         author_email = author_email,
                         author_date = author_date,
                         committer_name = committer_name,
                         committer_email = committer_email)

    print 'Importing patch "%s"...' % patch,
    sys.stdout.flush()

    if options.base:
        git.apply_patch(filename, git_id(options.base))
    else:
        git.apply_patch(filename)

    crt_series.refresh_patch(edit = options.edit,
                             show_patch = options.showpatch)

    print 'done'

def __import_series(filename, options):
    """Import a series of patches
    """
    applied = crt_series.get_applied()

    if filename:
        f = file(filename)
        patchdir = os.path.dirname(filename)
    else:
        f = sys.stdin
        patchdir = ''

    for line in f:
        patch = re.sub('#.*$', '', line).strip()
        if not patch:
            continue
        patchfile = os.path.join(patchdir, patch)

        if options.strip:
            patch = __strip_patch_name(patch)
        patch = __replace_slashes_with_dashes(patch);
        if options.ignore and patch in applied:
            print 'Ignoring already applied patch "%s"' % patch
            continue

        __import_patch(patch, patchfile, options)

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

    if options.series:
        __import_series(filename, options)
    else:
        if options.name:
            patch = options.name
        elif filename:
            patch = os.path.basename(filename)
        else:
            patch = ''
        if options.strip:
            patch = __strip_patch_name(patch)

        __import_patch(patch, filename, options)

    print_crt_patch()
