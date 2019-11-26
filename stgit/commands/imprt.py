# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from contextlib import closing
import bz2
import gzip
import mailbox
import os
import re
import sys
import tarfile

from stgit import argparse
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepositoryLib,
    check_conflicts,
    check_head_top_equal,
    check_local_changes,
    parse_mail,
    parse_patch,
    update_commit_data,
)
from stgit.compat import message_from_binary_file
from stgit.lib.git import CommitData, Date, Person
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out
from stgit.utils import make_patch_name

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

name = 'import'
help = 'Import a GNU diff file as a new patch'
kind = 'patch'
usage = ['[options] [--] [<file>|<url>]']
description = """
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

args = ['files']
options = [
    opt(
        '-m',
        '--mail',
        action='store_true',
        short='Import the patch from a standard e-mail file',
    ),
    opt(
        '-M',
        '--mbox',
        action='store_true',
        short='Import a series of patches from an mbox file',
    ),
    opt(
        '-s',
        '--series',
        action='store_true',
        short='Import a series of patches',
        long="""
        Import a series of patches from a series file or a tar archive.""",
    ),
    opt(
        '-u',
        '--url',
        action='store_true',
        short='Import a patch from a URL',
    ),
    opt(
        '-n',
        '--name',
        short='Use NAME as the patch name',
    ),
    opt(
        '-p',
        '--strip',
        type='int',
        metavar='N',
        short='Remove N leading slashes from diff paths (default 1)',
    ),
    opt(
        '-t',
        '--stripname',
        action='store_true',
        short='Strip numbering and extension from patch name',
    ),
    opt(
        '-i',
        '--ignore',
        action='store_true',
        short='Ignore the applied patches in the series',
    ),
    opt(
        '--replace',
        action='store_true',
        short='Replace the unapplied patches in the series',
    ),
    opt(
        '-b',
        '--base',
        args=['commit'],
        short='Use BASE instead of HEAD for file importing',
    ),
    opt(
        '--reject',
        action='store_true',
        short='Leave the rejected hunks in corresponding *.rej files',
    ),
    opt(
        '-e',
        '--edit',
        action='store_true',
        short='Invoke an editor for the patch description',
    ),
    opt(
        '-d',
        '--showdiff',
        action='store_true',
        short='Show the patch content in the editor buffer',
    ),
] + argparse.author_options() + argparse.sign_options()

directory = DirectoryHasRepositoryLib()


def __strip_patch_name(name):
    stripped = re.sub('^[0-9]+-(.*)$', r'\g<1>', name)
    stripped = re.sub(r'^(.*)\.(diff|patch)$', r'\g<1>', stripped)
    return stripped


def __replace_slashes_with_dashes(name):
    stripped = name.replace('/', '-')
    return stripped


def __create_patch(filename, message, author_name, author_email,
                   author_date, diff, options):
    """Create a new patch on the stack
    """
    stack = directory.repository.current_stack

    if options.name:
        name = options.name
        if not stack.patches.is_name_valid(name):
            raise CmdException('Invalid patch name: %s' % name)
    elif filename:
        name = os.path.basename(filename)
    else:
        name = ''
    if options.stripname:
        name = __strip_patch_name(name)

    if not name:
        if options.ignore or options.replace:
            def unacceptable_name(name):
                return False
        else:
            unacceptable_name = stack.patches.exists
        name = make_patch_name(message, unacceptable_name)
    else:
        # fix possible invalid characters in the patch name
        name = re.sub(r'[^\w.]+', '-', name).strip('-')

    assert stack.patches.is_name_valid(name)

    if options.ignore and name in stack.patchorder.applied:
        out.info('Ignoring already applied patch "%s"' % name)
        return

    out.start('Importing patch "%s"' % name)

    author = Person()
    if author_name:
        author = author.set_name(author_name)
    if author_email:
        author = author.set_email(author_email)
    if author_date:
        author = author.set_date(Date(author_date))
    author = options.author(author)

    try:
        if not diff:
            out.warn('No diff found, creating empty patch')
            tree = stack.head.data.tree
        else:
            iw = stack.repository.default_iw
            iw.apply(
                diff, quiet=False, reject=options.reject, strip=options.strip
            )
            tree = iw.index.write_tree()

        cd = CommitData(
            tree=tree,
            parents=[stack.head],
            author=author,
            message=message,
        )
        cd = update_commit_data(cd, options)
        commit = stack.repository.commit(cd)

        trans = StackTransaction(stack, 'import: %s' % name)

        try:
            if options.replace and name in stack.patchorder.unapplied:
                trans.delete_patches(lambda pn: pn == name, quiet=True)

            trans.patches[name] = commit
            trans.applied.append(name)
        except TransactionHalted:
            pass
        trans.run()
    finally:
        out.done()


def __mkpatchname(name, suffix):
    if name.lower().endswith(suffix.lower()):
        return name[:-len(suffix)]
    return name


def __get_handle_and_name(filename):
    """Return a file object and a patch name derived from filename
    """
    # see if it's a gzip'ed or bzip2'ed patch
    for copen, ext in [(gzip.open, '.gz'), (bz2.BZ2File, '.bz2')]:
        try:
            f = copen(filename)
            f.read(1)
            f.seek(0)
            return (f, __mkpatchname(filename, ext))
        except IOError:
            pass

    # plain old file...
    return (open(filename, 'rb'), filename)


def __import_file(filename, options, patch=None):
    """Import a patch from a file or standard input
    """
    pname = None
    if filename:
        (f, pname) = __get_handle_and_name(filename)
    else:
        f = os.fdopen(sys.__stdin__.fileno(), 'rb')

    if patch:
        pname = patch
    elif not pname:
        pname = filename

    if options.mail:
        try:
            msg = message_from_binary_file(f)
        except Exception as ex:
            raise CmdException('error parsing the e-mail file: %s' % str(ex))
        (
            message, author_name, author_email, author_date, diff
        ) = parse_mail(msg)
    else:
        patch_str = f.read()
        (
            message, author_name, author_email, author_date, diff
        ) = parse_patch(patch_str, contains_diff=True)

    if filename:
        f.close()

    __create_patch(pname, message, author_name, author_email,
                   author_date, diff, options)


def __import_series(filename, options):
    """Import a series of patches
    """
    if filename:
        if tarfile.is_tarfile(filename):
            __import_tarfile(filename, options)
            return
        f = open(filename)
        patchdir = os.path.dirname(filename)
    else:
        f = sys.stdin
        patchdir = ''

    for line in f:
        patch = re.sub('#.*$', '', line).strip()
        if not patch:
            continue
        # Quilt can have "-p0", "-p1" or "-pab" patches stacked in the
        # series but as strip level default to 1, only "-p0" can actually
        # be found in the series file, the other ones are implicit
        m = re.match(
            r'(?P<patchfilename>.*)\s+-p\s*(?P<striplevel>(\d+|ab)?)\s*$',
            patch,
        )
        options.strip = 1
        if m:
            patch = m.group('patchfilename')
            if m.group('striplevel') != '0':
                raise CmdException("error importing quilt series, patch '%s'"
                                   " has unsupported strip level: '-p%s'" %
                                   (patch, m.group('striplevel')))
            options.strip = 0
        patchfile = os.path.join(patchdir, patch)
        patch = __replace_slashes_with_dashes(patch)

        __import_file(patchfile, options, patch)

    if filename:
        f.close()


def __import_mbox(filename, options):
    """Import a series from an mbox file
    """
    if filename:
        namedtemp = None
    else:
        from tempfile import NamedTemporaryFile
        stdin = os.fdopen(sys.__stdin__.fileno(), 'rb')
        namedtemp = NamedTemporaryFile('wb', suffix='.mbox', delete=False)
        namedtemp.write(stdin.read())
        namedtemp.close()
        filename = namedtemp.name

    try:
        try:
            mbox = mailbox.mbox(filename, message_from_binary_file,
                                create=False)
        except Exception as ex:
            raise CmdException('error parsing the mbox file: %s' % str(ex))

        with closing(mbox):
            for msg in mbox:
                (message,
                 author_name,
                 author_email,
                 author_date,
                 diff) = parse_mail(msg)
                __create_patch(None, message, author_name, author_email,
                               author_date, diff, options)
    finally:
        if namedtemp is not None:
            os.unlink(namedtemp.name)


def __import_url(url, options):
    """Import a patch from a URL
    """
    try:
        from urllib.request import urlretrieve
        from urllib.parse import unquote
    except ImportError:
        from urllib import urlretrieve, unquote
    import tempfile

    if not url:
        raise CmdException('URL argument required')

    patch = os.path.basename(unquote(url))
    filename = os.path.join(tempfile.gettempdir(), patch)
    urlretrieve(url, filename)
    __import_file(filename, options)


def __import_tarfile(tar, options):
    """Import patch series from a tar archive
    """
    import tempfile
    import shutil

    if not tarfile.is_tarfile(tar):
        raise CmdException("%s is not a tarfile!" % tar)

    t = tarfile.open(tar, 'r')
    names = t.getnames()

    # verify paths in the tarfile are safe
    for n in names:
        if n.startswith('/'):
            raise CmdException("Absolute path found in %s" % tar)
        if n.find("..") > -1:
            raise CmdException("Relative path found in %s" % tar)

    # find the series file
    seriesfile = ''
    for m in names:
        if m.endswith('/series') or m == 'series':
            seriesfile = m
            break
    if seriesfile == '':
        raise CmdException("no 'series' file found in %s" % tar)

    # unpack into a tmp dir
    tmpdir = tempfile.mkdtemp('.stg')
    t.extractall(tmpdir)

    # apply the series
    __import_series(os.path.join(tmpdir, seriesfile), options)

    # cleanup the tmpdir
    shutil.rmtree(tmpdir)


def func(parser, options, args):
    """Import a GNU diff file as a new patch
    """
    if len(args) > 1:
        parser.error('incorrect number of arguments')

    if len(args) == 1:
        filename = args[0]
    else:
        filename = None

    if not options.url and filename:
        filename = os.path.abspath(filename)
    directory.cd_to_topdir()

    repository = directory.repository
    stack = repository.current_stack
    check_local_changes(repository)
    check_conflicts(repository.default_iw)
    check_head_top_equal(stack)

    if options.series:
        __import_series(filename, options)
    elif options.mbox:
        __import_mbox(filename, options)
    elif options.url:
        __import_url(filename, options)
    else:
        __import_file(filename, options)
