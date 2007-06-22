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

import sys, os, re, email
from email.Header import decode_header, make_header
from mailbox import UnixMailbox
from StringIO import StringIO
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'import a GNU diff file as a new patch'
usage = """%prog [options] [<file>|<url>]

Create a new patch and apply the given GNU diff file (or the standard
input). By default, the file name is used as the patch name but this
can be overridden with the '--name' option. The patch can either be a
normal file with the description at the top or it can have standard
mail format, the Subject, From and Date headers being used for
generating the patch information. The command can also read series and
mbox files.

If a patch does not apply cleanly, the failed diff is written to the
.stgit-failed.patch file and an empty StGIT patch is added to the
stack.

The patch description has to be separated from the data with a '---'
line."""

options = [make_option('-m', '--mail',
                       help = 'import the patch from a standard e-mail file',
                       action = 'store_true'),
           make_option('-M', '--mbox',
                       help = 'import a series of patches from an mbox file',
                       action = 'store_true'),
           make_option('-s', '--series',
                       help = 'import a series of patches',
                       action = 'store_true'),
           make_option('-u', '--url',
                       help = 'import a patch from a URL',
                       action = 'store_true'),
           make_option('-n', '--name',
                       help = 'use NAME as the patch name'),
           make_option('-t', '--strip',
                       help = 'strip numbering and extension from patch name',
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

def __split_descr_diff(string):
    """Return the description and the diff from the given string
    """
    descr = diff = ''
    top = True

    for line in string.split('\n'):
        if top:
            if not __end_descr(line):
                descr += line + '\n'
                continue
            else:
                top = False
        diff += line + '\n'

    return (descr.rstrip(), diff)

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

def __parse_mail(msg):
    """Parse the message object and return (description, authname,
    authemail, authdate, diff)
    """
    def __decode_header(header):
        """Decode a qp-encoded e-mail header as per rfc2047"""
        try:
            words_enc = decode_header(header)
            hobj = make_header(words_enc)
        except Exception, ex:
            raise CmdException, 'header decoding error: %s' % str(ex)
        return unicode(hobj).encode('utf-8')

    # parse the headers
    if msg.has_key('from'):
        authname, authemail = name_email(__decode_header(msg['from']))
    else:
        authname = authemail = None

    # '\n\t' can be found on multi-line headers
    descr = __decode_header(msg['subject']).replace('\n\t', ' ')
    authdate = msg['date']

    # remove the '[*PATCH*]' expression in the subject
    if descr:
        descr = re.findall('^(\[.*?[Pp][Aa][Tt][Cc][Hh].*?\])?\s*(.*)$',
                           descr)[0][1]
    else:
        raise CmdException, 'Subject: line not found'

    # the rest of the message
    msg_text = ''
    for part in msg.walk():
        if part.get_content_type() == 'text/plain':
            msg_text += part.get_payload(decode = True)

    rem_descr, diff = __split_descr_diff(msg_text)
    if rem_descr:
        descr += '\n\n' + rem_descr
    if not diff:
        out.warn('Message does not contain any diff')

    # parse the description for author information
    descr, descr_authname, descr_authemail, descr_authdate = \
           __parse_description(descr)
    if descr_authname:
        authname = descr_authname
    if descr_authemail:
        authemail = descr_authemail
    if descr_authdate:
       authdate = descr_authdate

    return (descr, authname, authemail, authdate, diff)

def __parse_patch(fobj):
    """Parse the input file and return (description, authname,
    authemail, authdate, diff)
    """
    descr, diff = __split_descr_diff(fobj.read())
    descr, authname, authemail, authdate = __parse_description(descr)

    # we don't yet have an agreed place for the creation date.
    # Just return None
    return (descr, authname, authemail, authdate, diff)

def __create_patch(filename, message, author_name, author_email,
                   author_date, diff, options):
    """Create a new patch on the stack
    """
    if options.name:
        patch = options.name
    elif filename:
        patch = os.path.basename(filename)
    else:
        patch = ''
    if options.strip:
        patch = __strip_patch_name(patch)

    if not patch:
        if options.ignore or options.replace:
            unacceptable_name = lambda name: False
        else:
            unacceptable_name = crt_series.patch_exists
        patch = make_patch_name(message, unacceptable_name)
    else:
        # fix possible invalid characters in the patch name
        patch = re.sub('[^\w.]+', '-', patch).strip('-')

    if not diff:
        raise CmdException, 'No diff found inside the patch'

    if options.ignore and patch in crt_series.get_applied():
        out.info('Ignoring already applied patch "%s"' % patch)
        return
    if options.replace and patch in crt_series.get_unapplied():
        crt_series.delete_patch(patch)

    # refresh_patch() will invoke the editor in this case, with correct
    # patch content
    if not message:
        can_edit = False

    committer_name = committer_email = None

    if options.author:
        options.authname, options.authemail = name_email(options.author)

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

    crt_series.new_patch(patch, message = message, can_edit = False,
                         author_name = author_name,
                         author_email = author_email,
                         author_date = author_date,
                         committer_name = committer_name,
                         committer_email = committer_email)

    out.start('Importing patch "%s"' % patch)
    if options.base:
        git.apply_patch(diff = diff, base = git_id(options.base))
    else:
        git.apply_patch(diff = diff)
    crt_series.refresh_patch(edit = options.edit,
                             show_patch = options.showpatch)
    out.done()

def __import_file(filename, options, patch = None):
    """Import a patch from a file or standard input
    """
    if filename:
        f = file(filename)
    else:
        f = sys.stdin

    if options.mail:
        try:
            msg = email.message_from_file(f)
        except Exception, ex:
            raise CmdException, 'error parsing the e-mail file: %s' % str(ex)
        message, author_name, author_email, author_date, diff = \
                 __parse_mail(msg)
    else:
        message, author_name, author_email, author_date, diff = \
                 __parse_patch(f)

    if filename:
        f.close()

    if patch:
        pname = patch
    else:
        pname = filename

    __create_patch(pname, message, author_name, author_email,
                   author_date, diff, options)

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
        patch = __replace_slashes_with_dashes(patch);

        __import_file(patchfile, options, patch)

    if filename:
        f.close()

def __import_mbox(filename, options):
    """Import a series from an mbox file
    """
    if filename:
        f = file(filename, 'rb')
    else:
        f = StringIO(sys.stdin.read())

    try:
        mbox = UnixMailbox(f, email.message_from_file)
    except Exception, ex:
        raise CmdException, 'error parsing the mbox file: %s' % str(ex)

    for msg in mbox:
        message, author_name, author_email, author_date, diff = \
                 __parse_mail(msg)
        __create_patch(None, message, author_name, author_email,
                       author_date, diff, options)

    f.close()

def __import_url(url, options):
    """Import a patch from a URL
    """
    import urllib
    import tempfile

    if not url:
        parser.error('URL argument required')

    patch = os.path.basename(urllib.unquote(url))
    filename = os.path.join(tempfile.gettempdir(), patch)
    urllib.urlretrieve(url, filename)
    __import_file(filename, options)

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
    elif options.mbox:
        __import_mbox(filename, options)
    elif options.url:
        __import_url(filename, options)
    else:
        __import_file(filename, options)

    print_crt_patch()
