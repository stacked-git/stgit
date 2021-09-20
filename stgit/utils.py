"""Common utility functions"""

import os
import re
import shutil
import sys
import tempfile
from io import open

from stgit.compat import environ_get
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


def strip_prefix(prefix, string):
    """Return string, without the specified prefix.

    The string must start with the prefix.

    """
    assert string.startswith(prefix)
    return string[len(prefix) :]


class EditorException(StgException):
    pass


def get_editor():
    for editor in [
        environ_get('GIT_EDITOR'),
        config.get('stgit.editor'),  # legacy
        config.get('core.editor'),
        environ_get('VISUAL'),
        environ_get('EDITOR'),
        'vi',
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


def get_hooks_path(repository):
    hooks_path = config.get('core.hookspath')
    if hooks_path is None:
        return os.path.join(repository.directory, 'hooks')
    elif os.path.isabs(hooks_path):
        return hooks_path
    else:
        return os.path.join(repository.default_worktree.directory, hooks_path)


def get_hook(repository, hook_name, extra_env=None):
    if not extra_env:
        extra_env = {}
    hook_path = os.path.join(get_hooks_path(repository), hook_name)
    if not (os.path.isfile(hook_path) and os.access(hook_path, os.X_OK)):
        return None

    prefix_dir = os.path.relpath(os.getcwd(), repository.default_worktree.directory)
    if prefix_dir == os.curdir:
        prefix = ''
    else:
        prefix = os.path.join(prefix_dir, '')
    extra_env = add_dict(extra_env, {'GIT_PREFIX': prefix})

    def hook(*parameters):
        if sys.platform == 'win32':
            # On Windows, run the hook using "bash" explicitly.
            # Try to locate bash.exe in user's PATH, but avoid the WSL
            # shim/bootstrapper %SYSTEMROOT%/System32/bash.exe
            systemroot = os.environ.get('SYSTEMROOT')
            if systemroot:
                system32 = os.path.normcase(os.path.join(systemroot, 'system32'))
                path = os.pathsep.join(
                    p
                    for p in os.environ.get('PATH', '').split(os.pathsep)
                    if os.path.normcase(p) != system32
                )
            else:
                path = None

            # Find bash with user's path (sans System32).
            bash_exe = shutil.which('bash.exe', path=path)
            if not bash_exe:
                # Next try finding the bash.exe that came with Git for Windows.
                git_exe = shutil.which('git.exe', path=path)
                if not git_exe:
                    raise StgException('Failed to locate either bash.exe or git.exe')
                bash_exe = os.path.join(
                    os.path.dirname(os.path.dirname(git_exe)),
                    'bin',
                    'bash.exe',
                )

            argv = [bash_exe, hook_path]
        else:
            argv = [hook_path]

        argv.extend(parameters)

        repository.default_iw.run(argv, extra_env).run()

    hook.__name__ = hook_name
    return hook


def run_hook_on_bytes(hook, byte_data, *args):
    temp = tempfile.NamedTemporaryFile('wb', prefix='stgit-hook', delete=False)
    try:
        with temp:
            temp.write(byte_data)
        hook(temp.name)
        with open(temp.name, 'rb') as data_file:
            return data_file.read()
    finally:
        os.unlink(temp.name)


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


def add_trailers(message, trailers, name, email):
    trailer_args = []
    for trailer in trailers:
        trailer_args.extend(['--trailer', '%s: %s <%s>' % (trailer, name, email)])
    return (
        Run('git', 'interpret-trailers', *trailer_args).raw_input(message).raw_output()
    )


def parse_name_email(address):
    """Parse an email address string.

    Returns a tuple consisting of the name and email parsed from a
    standard 'name <email>' or 'email (name)' string.

    """
    address = re.sub(r'[\\"]', r'\\\g<0>', address)
    str_list = re.findall(r'^(.*)\s*<(.*)>\s*$', address)
    if not str_list:
        str_list = re.findall(r'^(.*)\s*\((.*)\)\s*$', address)
        if not str_list:
            return None
        return (str_list[0][1], str_list[0][0])
    return str_list[0]


# Exit codes.
STGIT_SUCCESS = 0  # everything's OK
STGIT_GENERAL_ERROR = 1  # seems to be non-command-specific error
STGIT_COMMAND_ERROR = 2  # seems to be a command that failed
STGIT_CONFLICT = 3  # merge conflict, otherwise OK
STGIT_BUG_ERROR = 4  # a bug in StGit


def add_dict(d1, d2):
    """Return a new dict with the contents of both d1 and d2.

    In case of conflicting mappings, d2 takes precedence.

    """
    d = dict(d1)
    d.update(d2)
    return d
