import os
import re
import sys
import tempfile

from stgit import argparse
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    check_conflicts,
    check_head_top_equal,
    check_local_changes,
    parse_patch,
    update_commit_data,
)
from stgit.compat import decode_utf8_with_latin1
from stgit.lib.git import CommitData, Date, Person
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out
from stgit.run import Run

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
.stgit-failed.patch file and an empty StGit patch is added to the
stack.

The patch description has to be separated from the data with a '---'
line."""

args = ['files']
options = (
    [
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
            long="Import a series of patches from a series file or a tar archive.",
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
            '-C',
            dest='context_lines',
            type='int',
            metavar='N',
            short='Ensure N lines of surrounding context for each change',
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
            '--keep-cr',
            action='store_true',
            short='Do not remove "\\r" from email lines ending with "\\r\\n"',
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
    ]
    + argparse.author_options()
    + argparse.sign_options()
)

directory = DirectoryHasRepository()


def __create_patch(
    filename, message, patch_name, author_name, author_email, author_date, diff, options
):
    """Create a new patch on the stack"""
    stack = directory.repository.current_stack

    if patch_name:
        name = patch_name
    elif options.name:
        name = options.name
    elif filename:
        name = os.path.basename(filename)
    else:
        name = ''

    if options.stripname:
        # Removing leading numbers and trailing extension
        name = re.sub(
            r'''^
                (?:[0-9]+-)?           # Optional leading patch number
                (.*?)                  # Patch name group (non-greedy)
                (?:\.(?:diff|patch))?  # Optional .diff or .patch extension
                $
            ''',
            r'\g<1>',
            name,
            flags=re.VERBOSE,
        )

    need_unique = not (options.ignore or options.replace)

    if name:
        name = stack.patches.make_name(name, unique=need_unique, lower=False)
    else:
        name = stack.patches.make_name(message, unique=need_unique, lower=True)

    if options.ignore and name in stack.patchorder.applied:
        out.info('Ignoring already applied patch "%s"' % name)
        return

    out.start('Importing patch "%s"' % name)

    author = options.author(
        Person(
            author_name,
            author_email,
            Date.maybe(author_date),
        )
    )

    try:
        if not diff:
            out.warn('No diff found, creating empty patch')
            tree = stack.head.data.tree
        else:
            iw = stack.repository.default_iw
            iw.apply(
                diff,
                quiet=False,
                reject=options.reject,
                strip=options.strip,
                context_lines=options.context_lines,
            )
            tree = iw.index.write_tree()

        cd = CommitData(
            tree=tree,
            parents=[stack.head],
            author=author,
            message=message,
        )
        cd = update_commit_data(
            stack.repository,
            cd,
            message=None,
            author=None,
            sign_str=options.sign_str,
            edit=options.edit,
        )
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


def __get_handle_and_name(filename):
    """Return a file object and a patch name derived from filename"""
    import bz2
    import gzip

    # see if it's a gzip'ed or bzip2'ed patch
    for copen, ext in [(gzip.open, '.gz'), (bz2.BZ2File, '.bz2')]:
        try:
            f = copen(filename)
            f.read(1)
            f.seek(0)
            if filename.lower().endswith(ext):
                filename = filename[: -len(ext)]
            return (f, filename)
        except IOError:
            pass

    # plain old file...
    return (open(filename, 'rb'), filename)


def __import_file(filename, options):
    """Import a patch from a file or standard input"""
    if filename:
        f, filename = __get_handle_and_name(filename)
    else:
        f = os.fdopen(sys.__stdin__.fileno(), 'rb')

    patch_data = f.read()

    if filename:
        f.close()

    message, patch_name, author_name, author_email, author_date, diff = parse_patch(
        patch_data, contains_diff=True
    )

    __create_patch(
        filename,
        message,
        patch_name,
        author_name,
        author_email,
        author_date,
        diff,
        options,
    )


def __import_series(filename, options):
    """Import a series of patches"""
    import tarfile

    if filename and tarfile.is_tarfile(filename):
        __import_tarfile(filename, options)
        return
    elif filename:
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
            r'(?P<patchfilename>.*)\s+-p\s*(?P<striplevel>(\d+|ab)?)\s*$', patch
        )
        if m:
            patch = m.group('patchfilename')
            if m.group('striplevel') != '0':
                raise CmdException(
                    "error importing quilt series, patch '%s'"
                    " has unsupported strip level: '-p%s'"
                    % (patch, m.group('striplevel'))
                )
            options.strip = 0
        else:
            options.strip = 1
        patchfile = os.path.join(patchdir, patch)

        __import_file(patchfile, options)

    if filename:
        f.close()


def __import_mail(filename, options):
    """Import a patch from an email file or mbox"""
    with tempfile.TemporaryDirectory('.stg') as tmpdir:
        mail_paths = __mailsplit(tmpdir, filename, options)
        for mail_path in mail_paths:
            __import_mail_path(mail_path, filename, options)


def __mailsplit(tmpdir, filename, options):
    mailsplit_cmd = ['git', 'mailsplit', '-d4', '-o' + tmpdir]
    if options.mail:
        mailsplit_cmd.append('-b')
    if options.keep_cr:
        mailsplit_cmd.append('--keep-cr')

    if filename:
        mailsplit_cmd.extend(['--', filename])
        r = Run(*mailsplit_cmd)
    else:
        stdin = os.fdopen(sys.__stdin__.fileno(), 'rb')
        r = Run(*mailsplit_cmd).encoding(None).raw_input(stdin.read())

    num_patches = int(r.output_one_line())

    mail_paths = [os.path.join(tmpdir, '%04d' % n) for n in range(1, num_patches + 1)]

    return mail_paths


def __import_mail_path(mail_path, filename, options):
    with open(mail_path, 'rb') as f:
        mail = f.read()

    msg_path = mail_path + '-msg'
    patch_path = mail_path + '-patch'

    mailinfo_lines = (
        Run('git', 'mailinfo', msg_path, patch_path)
        .encoding(None)
        .decoding(None)
        .raw_input(mail)
        .output_lines(b'\n')
    )

    mailinfo = dict(line.split(b': ', 1) for line in mailinfo_lines if line)

    with open(msg_path, 'rb') as f:
        msg_body = f.read()

    msg_bytes = mailinfo[b'Subject'] + b'\n\n' + msg_body

    with open(patch_path, 'rb') as f:
        diff = f.read()

    __create_patch(
        None if options.mbox else filename,
        decode_utf8_with_latin1(msg_bytes),
        None,
        mailinfo[b'Author'].decode('utf-8'),
        mailinfo[b'Email'].decode('utf-8'),
        mailinfo[b'Date'].decode('utf-8'),
        diff,
        options,
    )


def __import_url(url, options):
    """Import a patch from a URL"""
    from urllib.parse import unquote
    from urllib.request import urlretrieve

    with tempfile.TemporaryDirectory('.stg') as tmpdir:
        base = os.path.basename(unquote(url))
        filename = os.path.join(tmpdir, base)
        urlretrieve(url, filename)
        if options.series:
            __import_series(filename, options)
        elif options.mail or options.mbox:
            __import_mail(filename, options)
        else:
            __import_file(filename, options)


def __import_tarfile(tarpath, options):
    """Import patch series from a tar archive"""
    import tarfile

    assert tarfile.is_tarfile(tarpath)

    tar = tarfile.open(tarpath, 'r')
    names = tar.getnames()

    # verify paths in the tarfile are safe
    for n in names:
        if n.startswith('/'):
            raise CmdException("Absolute path found in %s" % tarpath)
        if n.find("..") > -1:
            raise CmdException("Relative path found in %s" % tarpath)

    # find the series file
    for seriesfile in names:
        if seriesfile.endswith('/series') or seriesfile == 'series':
            break
    else:
        raise CmdException("no 'series' file found in %s" % tarpath)

    # unpack into a tmp dir
    with tempfile.TemporaryDirectory('.stg') as tmpdir:
        tar.extractall(tmpdir)
        __import_series(os.path.join(tmpdir, seriesfile), options)


def func(parser, options, args):
    if len(args) > 1:
        parser.error('incorrect number of arguments')
    elif len(args) == 1:
        filename = args[0]
    elif options.url:
        raise CmdException('URL argument required')
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

    if options.url:
        __import_url(filename, options)
    elif options.series:
        __import_series(filename, options)
    elif options.mail or options.mbox:
        __import_mail(filename, options)
    else:
        __import_file(filename, options)
