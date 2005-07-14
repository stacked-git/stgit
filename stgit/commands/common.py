"""Function/variables commmon to all the commands
"""

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
from optparse import OptionParser, make_option

from stgit.utils import *
from stgit import stack, git


# Command exception class
class CmdException(Exception):
    pass


# Utility functions
def git_id(string):
    """Return the GIT id
    """
    if not string:
        return None
    
    string_list = string.split('/')

    if len(string_list) == 1:
        patch_name = None
        git_id = string_list[0]

        if git_id == 'HEAD':
            return git.get_head()
        if git_id == 'base':
            return read_string(crt_series.get_base_file())

        for path in [os.path.join(git.base_dir, 'refs', 'heads'),
                     os.path.join(git.base_dir, 'refs', 'tags')]:
            id_file = os.path.join(path, git_id)
            if os.path.isfile(id_file):
                return read_string(id_file)
    elif len(string_list) == 2:
        patch_name = string_list[0]
        if patch_name == '':
            patch_name = crt_series.get_current()
        git_id = string_list[1]

        if not patch_name:
            raise CmdException, 'No patches applied'
        elif not (patch_name in crt_series.get_applied()
                + crt_series.get_unapplied()):
            raise CmdException, 'Unknown patch "%s"' % patch_name

        if git_id == 'bottom':
            return crt_series.get_patch(patch_name).get_bottom()
        if git_id == 'top':
            return crt_series.get_patch(patch_name).get_top()

    raise CmdException, 'Unknown id: %s' % string

def check_local_changes():
    if git.local_changes():
        raise CmdException, \
              'local changes in the tree. Use "refresh" to commit them'

def check_head_top_equal():
    if not crt_series.head_top_equal():
        raise CmdException, \
              'HEAD and top are not the same. You probably committed\n' \
              '  changes to the tree ouside of StGIT. If you know what you\n' \
              '  are doing, use the "refresh -f" command'

def check_conflicts():
    if os.path.exists(os.path.join(git.base_dir, 'conflicts')):
        raise CmdException, 'Unsolved conflicts. Please resolve them first'

def print_crt_patch():
    patch = crt_series.get_current()
    if patch:
        print 'Now at patch "%s"' % patch
    else:
        print 'No patches applied'

def resolved(filename):
    git.update_cache([filename])
    for ext in ['.local', '.older', '.remote']:
        fn = filename + ext
        if os.path.isfile(fn):
            os.remove(fn)

def resolved_all():
    conflicts = git.get_conflicts()
    if conflicts:
        for filename in conflicts:
            resolved(filename)
        os.remove(os.path.join(git.base_dir, 'conflicts'))

def name_email(string):
    """Return a tuple consisting of the name and email parsed from a
    standard 'name <email>' string
    """
    names = re.split('([^<>]*)<([^<>]*)>', string)
    if len(names) != 4:
        raise CmdException, 'Incorrect "name <email>" string: %s' % string

    return tuple([names[1].strip(), names[2].strip()])
