# -*- coding: utf-8 -*-
"""Common utility functions"""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from io import open
import errno
import os
import re
import sys
import tempfile

from stgit.compat import environ_get, text
from stgit.config import config
from stgit.exception import StgException
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


def mkdir_file(filename, mode, encoding='utf-8'):
    """Opens filename with the given mode, creating the directory it's
    in if it doesn't already exist."""
    create_dirs(os.path.dirname(filename))
    return open(filename, mode, encoding=encoding)


def read_strings(filename, encoding='utf-8'):
    """Reads the lines from a file
    """
    with open(filename, encoding=encoding) as f:
        return [line.strip() for line in f.readlines()]


def read_string(filename, encoding='utf-8'):
    """Reads the first line from a file
    """
    with open(filename, encoding=encoding) as f:
        return f.readline().strip()


def write_strings(filename, lines, encoding='utf-8'):
    """Write 'lines' sequence to file
    """
    with open(filename, 'w+', encoding=encoding) as f:
        for line in lines:
            print(line, file=f)


def write_string(filename, line, multiline=False, encoding='utf-8'):
    """Writes 'line' to file and truncates it
    """
    with mkdir_file(filename, 'w+', encoding) as f:
        line = text(line)
        print(line, end='' if multiline else '\n', file=f)


def create_empty_file(name):
    """Creates an empty file
    """
    mkdir_file(name, 'w+').close()


def strip_prefix(prefix, string):
    """Return string, without the prefix. Blow up if string doesn't
    start with prefix."""
    assert string.startswith(prefix)
    return string[len(prefix):]


def create_dirs(directory):
    """Create the given directory, if the path doesn't already exist."""
    if directory and not os.path.isdir(directory):
        create_dirs(os.path.dirname(directory))
        try:
            os.mkdir(directory)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise e


def rename(basedir, file1, file2):
    """Rename join(basedir, file1) to join(basedir, file2), not
    leaving any empty directories behind and creating any directories
    necessary."""
    full_file2 = os.path.join(basedir, file2)
    create_dirs(os.path.dirname(full_file2))
    os.rename(os.path.join(basedir, file1), full_file2)
    try:
        os.removedirs(os.path.join(basedir, os.path.dirname(file1)))
    except OSError:
        # file1's parent dir may not be empty after move
        pass


class EditorException(StgException):
    pass


def get_editor():
    for editor in [
        environ_get('GIT_EDITOR'),
        config.get('stgit.editor'),  # legacy
        config.get('core.editor'),
        environ_get('VISUAL'),
        environ_get('EDITOR'),
        'vi'
    ]:
        if editor:
            return editor


def call_editor(filename):
    """Run the editor on the specified filename."""
    cmd = '%s %s' % (get_editor(), filename)
    out.start('Invoking the editor: "%s"' % cmd)
    err = os.system(cmd)
    if err:
        raise EditorException('editor failed, exit code: %d' % err)
    out.done()


def get_hook(repository, hook_name, extra_env={}):
    hook_path = os.path.join(repository.directory, 'hooks', hook_name)
    if not (os.path.isfile(hook_path) and os.access(hook_path, os.X_OK)):
        return None

    default_iw = repository.default_iw
    prefix_dir = os.path.relpath(os.getcwd(), default_iw.cwd)
    if prefix_dir == os.curdir:
        prefix = ''
    else:
        prefix = os.path.join(prefix_dir, '')
    extra_env = add_dict(extra_env, {'GIT_PREFIX': prefix})

    def hook(*parameters):
        argv = [hook_path]
        argv.extend(parameters)

        # On Windows, run the hook using "bash" explicitly
        if os.name != 'posix':
            argv.insert(0, 'bash')

        default_iw.run(argv, extra_env).run()

    hook.__name__ = str(hook_name)
    return hook


def run_hook_on_bytes(hook, byte_data, *args):
    if hook is not None:
        temp = tempfile.NamedTemporaryFile('wb', delete=False)
        try:
            try:
                temp.write(byte_data)
            finally:
                temp.close()

            if sys.version_info[0] >= 3:
                data_path = temp.name
            else:
                data_path = temp.name.decode(sys.getfilesystemencoding())
            hook(data_path, *args)

            with open(data_path, 'rb') as data_file:
                byte_data = data_file.read()
        finally:
            os.unlink(temp.name)

    return byte_data


def edit_string(s, filename, encoding='utf-8'):
    with open(filename, 'w', encoding=encoding) as f:
        f.write(s)
    call_editor(filename)
    with open(filename, encoding=encoding) as f:
        s = f.read()
    os.remove(filename)
    return s


def edit_bytes(s, filename):
    with open(filename, 'wb') as f:
        f.write(s)
    call_editor(filename)
    with open(filename, 'rb') as f:
        s = f.read()
    os.remove(filename)
    return s


def find_patch_name(patchname, unacceptable):
    """Find a patch name which is acceptable."""
    if unacceptable(patchname):
        suffix = 0
        while unacceptable('%s-%d' % (patchname, suffix)):
            suffix += 1
        patchname = '%s-%d' % (patchname, suffix)
    return patchname


def patch_name_from_msg(msg):
    """Return a string to be used as a patch name. This is generated
    from the top line of the string passed as argument."""
    if not msg:
        return None

    name_len = config.getint('stgit.namelength')
    if not name_len:
        name_len = 30

    subject_line = msg.split('\n', 1)[0].lstrip().lower()
    words = re.sub(r'(?u)[\W]+', ' ', subject_line).split()

    # use loop to avoid truncating the last name
    name = words and words[0] or 'unknown'
    for word in words[1:]:
        new = name + '-' + word
        if len(new) > name_len:
            break
        name = new

    return name


def make_patch_name(msg, unacceptable, default_name='patch'):
    """Return a patch name generated from the given commit message,
    guaranteed to make unacceptable(name) be false. If the commit
    message is empty, base the name on default_name instead."""
    patchname = patch_name_from_msg(msg)
    if not patchname:
        patchname = default_name
    return find_patch_name(patchname, unacceptable)


def add_trailer(message, trailer, name, email):
    trailer_line = '%s: %s <%s>' % (trailer, name, email)
    return Run(
        'git', 'interpret-trailers', '--trailer', trailer_line
    ).raw_input(message).raw_output()


def parse_name_email(address):
    """Return a tuple consisting of the name and email parsed from a
    standard 'name <email>' or 'email (name)' string."""
    address = re.sub(r'[\\"]', r'\\\g<0>', address)
    str_list = re.findall(r'^(.*)\s*<(.*)>\s*$', address)
    if not str_list:
        str_list = re.findall(r'^(.*)\s*\((.*)\)\s*$', address)
        if not str_list:
            return None
        return (str_list[0][1], str_list[0][0])
    return str_list[0]


# Exit codes.
STGIT_SUCCESS = 0        # everything's OK
STGIT_GENERAL_ERROR = 1  # seems to be non-command-specific error
STGIT_COMMAND_ERROR = 2  # seems to be a command that failed
STGIT_CONFLICT = 3       # merge conflict, otherwise OK
STGIT_BUG_ERROR = 4      # a bug in StGit


def add_dict(d1, d2):
    """Return a new dict with the contents of both d1 and d2. In case of
    conflicting mappings, d2 takes precedence."""
    d = dict(d1)
    d.update(d2)
    return d
