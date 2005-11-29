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

crt_series = None


# Command exception class
class CmdException(Exception):
    pass


# Utility functions
def git_id(rev):
    """Return the GIT id
    """
    if not rev:
        return None
    
    rev_list = rev.split('/')
    if len(rev_list) == 2:
        patch_id = rev_list[1]
        if not patch_id:
            patch_id = 'top'
    elif len(rev_list) == 1:
        patch_id = 'top'
    else:
        patch_id = None

    patch_branch = rev_list[0].split('@')
    if len(patch_branch) == 1:
        series = crt_series
    elif len(patch_branch) == 2:
        series = stack.Series(patch_branch[1])
    else:
        raise CmdException, 'Unknown id: %s' % rev

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
        # Note we can return None here.
        elif patch_id == 'top.old':
            return series.get_patch(patch_name).get_old_top()
        elif patch_id == 'bottom.old':
            return series.get_patch(patch_name).get_old_bottom()

    # base
    if patch_name == 'base' and len(rev_list) == 1:
        return read_string(series.get_base_file())

    # anything else failed
    return git.rev_parse(rev)

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
    if os.path.exists(os.path.join(git.get_base_dir(), 'conflicts')):
        raise CmdException, 'Unsolved conflicts. Please resolve them first'

def print_crt_patch(branch = None):
    if not branch:
        patch = crt_series.get_current()
    else:
        patch = stack.Series(branch).get_current()

    if patch:
        print 'Now at patch "%s"' % patch
    else:
        print 'No patches applied'

def resolved(filename, reset = None):
    if reset:
        reset_file = filename + '.' + reset
        if os.path.isfile(reset_file):
            if os.path.isfile(filename):
                os.remove(filename)
            os.rename(reset_file, filename)

    git.update_cache([filename], force = True)

    for ext in ['.local', '.older', '.remote']:
        fn = filename + ext
        if os.path.isfile(fn):
            os.remove(fn)

def resolved_all(reset = None):
    conflicts = git.get_conflicts()
    if conflicts:
        for filename in conflicts:
            resolved(filename, reset)
        os.remove(os.path.join(git.get_base_dir(), 'conflicts'))

def name_email(address):
    """Return a tuple consisting of the name and email parsed from a
    standard 'name <email>' string
    """
    address = re.sub('([^\w\s<>@.])', '\\\\\\1', address)
    str_list = re.findall('^(.*)\s*<(.*)>\s*$', address)
    if not str_list:
        raise CmdException, 'Incorrect "name <email>" string: %s' % address

    return str_list[0]

def name_email_date(address):
    """Return a tuple consisting of the name, email and date parsed
    from a 'name <email> date' string
    """
    address = re.sub('([^\w\s<>@.])', '\\\\\\1', address)
    str_list = re.findall('^(.*)\s*<(.*)>\s*(.*)\s*$', address)
    if not str_list:
        raise CmdException, 'Incorrect "name <email> date" string: %s' % address

    return str_list[0]
