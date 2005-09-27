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
    if len(string_list) == 2:
        patch_id = string_list[1]
        if not patch_id:
            patch_id = 'top'
    elif len(string_list) == 1:
        patch_id = 'top'
    else:
        patch_id = None

    patch_branch = string_list[0].split('@')
    if len(patch_branch) == 1:
        series = crt_series
    elif len(patch_branch) == 2:
        series = stack.Series(patch_branch[1])
    else:
        raise CmdException, 'Unknown id: %s' % string

    patch_name = patch_branch[0]
    if not patch_name:
        patch_name = series.get_current()
        if not patch_name:
            raise CmdException, 'No patches applied'

    # patch
    if patch_name in series.get_applied() \
           or patch_name in series.get_unapplied():
        if patch_id == 'top':
            return series.get_patch(patch_name).get_top()
        elif patch_id == 'bottom':
            return series.get_patch(patch_name).get_bottom()

    # base
    if patch_name == 'base' and len(string_list) == 1:
        return read_string(series.get_base_file())

    # anything else failed
    return git.rev_parse(string)

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
    git.update_cache([filename], force = True)
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
    str_list = re.findall('^(.*)\s+<(.*)>$', string)
    if not str_list:
        raise CmdException, 'Incorrect "name <email>" string: %s' % string

    return str_list[0]

def name_email_date(string):
    """Return a tuple consisting of the name, email and date parsed
    from a 'name <email> date' string
    """
    str_list = re.findall('^(.*)\s+<(.*)>\s+(.*)$', string)
    if not str_list:
        raise CmdException, 'Incorrect "name <email> date" string: %s' % string

    return str_list[0]
