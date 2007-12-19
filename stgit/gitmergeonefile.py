"""Performs a 3-way merge for GIT files
"""

__copyright__ = """
Copyright (C) 2006, Catalin Marinas <catalin.marinas@gmail.com>

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

import sys, os
from stgit.exception import *
from stgit import basedir
from stgit.config import config, file_extensions, ConfigOption
from stgit.utils import append_string
from stgit.out import *
from stgit.run import *

class GitMergeException(StgException):
    pass


#
# Options
#
autoimerge = ConfigOption('stgit', 'autoimerge')
keeporig = ConfigOption('stgit', 'keeporig')

#
# Utility functions
#
def __str2none(x):
    if x == '':
        return None
    else:
        return x

class MRun(Run):
    exc = GitMergeException # use a custom exception class on errors

def __checkout_stages(filename):
    """Check-out the merge stages in the index for the give file
    """
    extensions = file_extensions()
    line = MRun('git', 'checkout-index', '--stage=all', '--', filename
                ).output_one_line()
    stages, path = line.split('\t')
    stages = dict(zip(['ancestor', 'current', 'patched'],
                      stages.split(' ')))

    for stage, fn in stages.iteritems():
        if stages[stage] == '.':
            stages[stage] = None
        else:
            newname = filename + extensions[stage]
            if os.path.exists(newname):
                # remove the stage if it is already checked out
                os.remove(newname)
            os.rename(stages[stage], newname)
            stages[stage] = newname

    return stages

def __remove_stages(filename):
    """Remove the merge stages from the working directory
    """
    extensions = file_extensions()
    for ext in extensions.itervalues():
        fn = filename + ext
        if os.path.isfile(fn):
            os.remove(fn)

def interactive_merge(filename):
    """Run the interactive merger on the given file. Stages will be
    removed according to stgit.keeporig. If successful and stages
    kept, they will be removed via git.resolved().
    """
    stages = __checkout_stages(filename)

    try:
        # Check whether we have all the files for the merge.
        if not (stages['current'] and stages['patched']):
            raise GitMergeException('Cannot run the interactive merge')

        if stages['ancestor']:
            three_way = True
            files_dict = {'branch1': stages['current'],
                          'ancestor': stages['ancestor'],
                          'branch2': stages['patched'],
                          'output': filename}
            imerger = config.get('stgit.i3merge')
        else:
            three_way = False
            files_dict = {'branch1': stages['current'],
                          'branch2': stages['patched'],
                          'output': filename}
            imerger = config.get('stgit.i2merge')

        if not imerger:
            raise GitMergeException, 'No interactive merge command configured'

        mtime = os.path.getmtime(filename)

        out.start('Trying the interactive %s merge'
                  % (three_way and 'three-way' or 'two-way'))
        err = os.system(imerger % files_dict)
        out.done()
        if err != 0:
            raise GitMergeException, 'The interactive merge failed'
        if not os.path.isfile(filename):
            raise GitMergeException, 'The "%s" file is missing' % filename
        if mtime == os.path.getmtime(filename):
            raise GitMergeException, 'The "%s" file was not modified' % filename
    finally:
        # keep the merge stages?
        if str(keeporig) != 'yes':
            __remove_stages(filename)

def clean_up(filename):
    """Remove merge conflict stages if they were generated.
    """
    if str(keeporig) == 'yes':
        __remove_stages(filename)

def merge(filename):
    """Merge one file if interactive is allowed or check out the stages
    if keeporig is set.
    """
    if str(autoimerge) == 'yes':
        try:
            interactive_merge(filename)
        except GitMergeException, ex:
            out.error(str(ex))
            return False
        return True

    if str(keeporig) == 'yes':
        __checkout_stages(filename)

    return False
