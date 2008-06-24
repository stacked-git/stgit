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
from mailbox import UnixMailbox
from StringIO import StringIO
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
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

directory = DirectoryHasRepository()
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
                       help = 'use COMMEMAIL as the committer e-mail')
           ] + make_sign_options()


def __strip_patch_name(name):
    stripped = re.sub('^[0-9]+-(.*)$', '\g<1>', name)
    stripped = re.sub('^(.*)\.(diff|patch)$', '\g<1>', stripped)

    return stripped

def __replace_slashes_with_dashes(name):
    stripped = name.replace('/', '-')

    return stripped

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

    if options.ignore and patch in crt_series.get_applied():
        out.info('Ignoring already applied patch "%s"' % patch)
        return
    if options.replace and patch in crt_series.get_unapplied():
        crt_series.delete_patch(patch, keep_log = True)

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

    if not diff:
        out.warn('No diff found, creating empty patch')
    else:
        out.start('Importing patch "%s"' % patch)
        if options.base:
            git.apply_patch(diff = diff,
                            base = git_id(crt_series, options.base))
        else:
            git.apply_patch(diff = diff)
        crt_series.refresh_patch(edit = options.edit,
                                 show_patch = options.showpatch,
                                 sign_str = options.sign_str,
                                 backup = False)
        out.done()

def __mkpatchname(name, suffix):
    if name.lower().endswith(suffix.lower()):
        return name[:-len(suffix)]
    return name

def __get_handle_and_name(filename):
    """Return a file object and a patch name derived from filename
    """
    # see if it's a gzip'ed or bzip2'ed patch
    import bz2, gzip
    for copen, ext in [(gzip.open, '.gz'), (bz2.BZ2File, '.bz2')]:
        try:
            f = copen(filename)
            f.read(1)
            f.seek(0)
            return (f, __mkpatchname(filename, ext))
        except IOError, e:
            pass

    # plain old file...
    return (open(filename), filename)

def __import_file(filename, options, patch = None):
    """Import a patch from a file or standard input
    """
    pname = None
    if filename:
        (f, pname) = __get_handle_and_name(filename)
    else:
        f = sys.stdin

    if patch:
        pname = patch
    elif not pname:
        pname = filename

    if options.mail:
        try:
            msg = email.message_from_file(f)
        except Exception, ex:
            raise CmdException, 'error parsing the e-mail file: %s' % str(ex)
        message, author_name, author_email, author_date, diff = \
                 parse_mail(msg)
    else:
        message, author_name, author_email, author_date, diff = \
                 parse_patch(f.read())

    if filename:
        f.close()

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
                 parse_mail(msg)
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
    check_head_top_equal(crt_series)

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

    print_crt_patch(crt_series)
