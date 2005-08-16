#!/usr/bin/env python
"""Performs a 3-way merge for GIT files
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

import sys, os

# Try to detect where it is run from and set prefix and the search path.
# It is assumed that the user installed StGIT using the --prefix= option
prefix, bin = os.path.split(sys.path[0])

if bin == 'bin' and prefix != sys.prefix:
    major, minor = sys.version_info[0:2]
    sys.path += [os.path.join(prefix, 'lib', 'python'),
                 os.path.join(prefix, 'lib', 'python%s.%s' % (major, minor)),
                 os.path.join(prefix, 'lib', 'python%s.%s' % (major, minor),
                              'site-packages')]

from stgit.config import config
from stgit.utils import append_string


#
# Options
#
try:
    merger = config.get('gitmergeonefile', 'merger')
except Exception, err:
    print >> sys.stderr, 'Configuration error: %s' % err
    sys.exit(1)

if config.has_option('gitmergeonefile', 'keeporig'):
    keeporig = config.get('gitmergeonefile', 'keeporig')
else:
    keeporig = 'yes'


#
# Global variables
#
if 'GIT_DIR' in os.environ:
    base_dir = os.environ['GIT_DIR']
else:
    base_dir = '.git'


#
# Utility functions
#
def __str2none(x):
    if x == '':
        return None
    else:
        return x

def __output(cmd):
    f = os.popen(cmd, 'r')
    string = f.readline().strip()
    if f.close():
        print >> sys.stderr, 'Error: failed to execute "%s"' % cmd
        sys.exit(1)
    return string

def __checkout_files():
    """Check out the files passed as arguments
    """
    global orig, src1, src2

    if orig_hash:
        orig = '%s.older' % path
        tmp = __output('git-unpack-file %s' % orig_hash)
        os.chmod(tmp, int(orig_mode, 8))
        os.renames(tmp, orig)
    if file1_hash:
        src1 = '%s.local' % path
        tmp = __output('git-unpack-file %s' % file1_hash)
        os.chmod(tmp, int(file1_mode, 8))
        os.renames(tmp, src1)
    if file2_hash:
        src2 = '%s.remote' % path
        tmp = __output('git-unpack-file %s' % file2_hash)
        os.chmod(tmp, int(file2_mode, 8))
        os.renames(tmp, src2)

def __remove_files():
    """Remove any temporary files
    """
    if orig_hash:
        os.remove(orig)
    if file1_hash:
        os.remove(src1)
    if file2_hash:
        os.remove(src2)
    pass

def __conflict():
    """Write the conflict file for the 'path' variable and exit
    """
    append_string(os.path.join(base_dir, 'conflicts'), path)
    sys.exit(1)


#   $1 - original file SHA1 (or empty)
#   $2 - file in branch1 SHA1 (or empty)
#   $3 - file in branch2 SHA1 (or empty)
#   $4 - pathname in repository
#   $5 - orignal file mode (or empty)
#   $6 - file in branch1 mode (or empty)
#   $7 - file in branch2 mode (or empty)
#
#print 'gitmergeonefile.py "%s" "%s" "%s" "%s" "%s" "%s" "%s"' \
#      % tuple(sys.argv[1:8])
orig_hash, file1_hash, file2_hash, path, orig_mode, file1_mode, file2_mode = \
           [__str2none(x) for x in sys.argv[1:8]]


#
# Main algorithm
#
__checkout_files()

# file exists in origin
if orig_hash:
    # modified in both
    if file1_hash and file2_hash:
        # if modes are the same (git-read-tree probably dealed with it)
        if file1_hash == file2_hash:
            if os.system('git-update-cache --cacheinfo %s %s %s'
                         % (file1_mode, file1_hash, path)) != 0:
                print >> sys.stderr, 'Error: git-update-cache failed'
                __conflict()
            if os.system('git-checkout-cache -u -f -- %s' % path):
                print >> sys.stderr, 'Error: git-checkout-cache failed'
                __conflict()
            if file1_mode != file2_mode:
                print >> sys.stderr, \
                      'Error: File added in both, permissions conflict'
                __conflict()
        # 3-way merge
        else:
            merge_ok = os.system(merger % {'branch1': src1,
                                           'ancestor': orig,
                                           'branch2': src2,
                                           'output': path }) == 0

            if merge_ok:
                os.system('git-update-cache -- %s' % path)
                __remove_files()
                sys.exit(0)
            else:
                print >> sys.stderr, \
                      'Error: three-way merge tool failed for file "%s"' % path
                # reset the cache to the first branch
                os.system('git-update-cache --cacheinfo %s %s %s'
                          % (file1_mode, file1_hash, path))
                if keeporig != 'yes':
                    __remove_files()
                __conflict()
    # file deleted in both or deleted in one and unchanged in the other
    elif not (file1_hash or file2_hash) \
           or file1_hash == orig_hash or file2_hash == orig_hash:
        if os.path.exists(path):
            os.remove(path)
        __remove_files()
        sys.exit(os.system('git-update-cache --remove -- %s' % path))
# file does not exist in origin
else:
    # file added in both
    if file1_hash and file2_hash:
        # files are the same
        if file1_hash == file2_hash:
            if os.system('git-update-cache --add --cacheinfo %s %s %s'
                         % (file1_mode, file1_hash, path)) != 0:
                print >> sys.stderr, 'Error: git-update-cache failed'
                __conflict()
            if os.system('git-checkout-cache -u -f -- %s' % path):
                print >> sys.stderr, 'Error: git-checkout-cache failed'
                __conflict()
            if file1_mode != file2_mode:
                print >> sys.stderr, \
                      'Error: File "s" added in both, permissions conflict' \
                      % path
                __conflict()
        # files are different
        else:
            print >> sys.stderr, \
                  'Error: File "%s" added in branches but different' % path
            __conflict()
    # file added in one
    elif file1_hash or file2_hash:
        if file1_hash:
            mode = file1_mode
            obj = file1_hash
        else:
            mode = file2_mode
            obj = file2_hash
        if os.system('git-update-cache --add --cacheinfo %s %s %s'
                     % (mode, obj, path)) != 0:
            print >> sys.stderr, 'Error: git-update-cache failed'
            __conflict()
        __remove_files()
        sys.exit(os.system('git-checkout-cache -u -f -- %s' % path))

# Un-handled case
print >> sys.stderr, 'Error: Un-handled merge conflict'
print >> sys.stderr, 'gitmergeonefile.py "%s" "%s" "%s" "%s" "%s" "%s" "%s"' \
      % tuple(sys.argv[1:8])
__conflict()
