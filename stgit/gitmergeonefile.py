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
from stgit import basedir
from stgit.config import config, file_extensions, ConfigOption
from stgit.utils import append_string, out


class GitMergeException(Exception):
    pass


#
# Options
#
merger = ConfigOption('stgit', 'merger')
keeporig = ConfigOption('stgit', 'keeporig')

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
    string = f.readline().rstrip()
    if f.close():
        raise GitMergeException, 'Error: failed to execute "%s"' % cmd
    return string

def __checkout_files(orig_hash, file1_hash, file2_hash,
                     path,
                     orig_mode, file1_mode, file2_mode):
    """Check out the files passed as arguments
    """
    global orig, src1, src2

    extensions = file_extensions()

    if orig_hash:
        orig = path + extensions['ancestor']
        tmp = __output('git-unpack-file %s' % orig_hash)
        os.chmod(tmp, int(orig_mode, 8))
        os.renames(tmp, orig)
    if file1_hash:
        src1 = path + extensions['current']
        tmp = __output('git-unpack-file %s' % file1_hash)
        os.chmod(tmp, int(file1_mode, 8))
        os.renames(tmp, src1)
    if file2_hash:
        src2 = path + extensions['patched']
        tmp = __output('git-unpack-file %s' % file2_hash)
        os.chmod(tmp, int(file2_mode, 8))
        os.renames(tmp, src2)

    if file1_hash and not os.path.exists(path):
        # the current file might be removed by GIT when it is a new
        # file added in both branches. Just re-generate it
        tmp = __output('git-unpack-file %s' % file1_hash)
        os.chmod(tmp, int(file1_mode, 8))
        os.renames(tmp, path)

def __remove_files(orig_hash, file1_hash, file2_hash):
    """Remove any temporary files
    """
    if orig_hash:
        os.remove(orig)
    if file1_hash:
        os.remove(src1)
    if file2_hash:
        os.remove(src2)

def __conflict(path):
    """Write the conflict file for the 'path' variable and exit
    """
    append_string(os.path.join(basedir.get(), 'conflicts'), path)


def interactive_merge(filename):
    """Run the interactive merger on the given file. Note that the
    index should not have any conflicts.
    """
    extensions = file_extensions()

    ancestor = filename + extensions['ancestor']
    current = filename + extensions['current']
    patched = filename + extensions['patched']

    if os.path.isfile(ancestor):
        three_way = True
        files_dict = {'branch1': current,
                      'ancestor': ancestor,
                      'branch2': patched,
                      'output': filename}
        imerger = config.get('stgit.i3merge')
    else:
        three_way = False
        files_dict = {'branch1': current,
                      'branch2': patched,
                      'output': filename}
        imerger = config.get('stgit.i2merge')

    if not imerger:
        raise GitMergeException, 'No interactive merge command configured'

    # check whether we have all the files for the merge
    for fn in [filename, current, patched]:
        if not os.path.isfile(fn):
            raise GitMergeException, \
                  'Cannot run the interactive merge: "%s" missing' % fn

    mtime = os.path.getmtime(filename)

    out.info('Trying the interactive %s merge'
             % (three_way and 'three-way' or 'two-way'))

    err = os.system(imerger % files_dict)
    if err != 0:
        raise GitMergeException, 'The interactive merge failed: %d' % err
    if not os.path.isfile(filename):
        raise GitMergeException, 'The "%s" file is missing' % filename
    if mtime == os.path.getmtime(filename):
        raise GitMergeException, 'The "%s" file was not modified' % filename


#
# Main algorithm
#
def merge(orig_hash, file1_hash, file2_hash,
          path,
          orig_mode, file1_mode, file2_mode):
    """Three-way merge for one file algorithm
    """
    __checkout_files(orig_hash, file1_hash, file2_hash,
                     path,
                     orig_mode, file1_mode, file2_mode)

    # file exists in origin
    if orig_hash:
        # modified in both
        if file1_hash and file2_hash:
            # if modes are the same (git-read-tree probably dealt with it)
            if file1_hash == file2_hash:
                if os.system('git-update-index --cacheinfo %s %s %s'
                             % (file1_mode, file1_hash, path)) != 0:
                    out.error('git-update-index failed')
                    __conflict(path)
                    return 1
                if os.system('git-checkout-index -u -f -- %s' % path):
                    out.error('git-checkout-index failed')
                    __conflict(path)
                    return 1
                if file1_mode != file2_mode:
                    out.error('File added in both, permissions conflict')
                    __conflict(path)
                    return 1
            # 3-way merge
            else:
                merge_ok = os.system(str(merger) % {'branch1': src1,
                                                    'ancestor': orig,
                                                    'branch2': src2,
                                                    'output': path }) == 0

                if merge_ok:
                    os.system('git-update-index -- %s' % path)
                    __remove_files(orig_hash, file1_hash, file2_hash)
                    return 0
                else:
                    out.error('Three-way merge tool failed for file "%s"'
                              % path)
                    # reset the cache to the first branch
                    os.system('git-update-index --cacheinfo %s %s %s'
                              % (file1_mode, file1_hash, path))

                    if config.get('stgit.autoimerge') == 'yes':
                        try:
                            interactive_merge(path)
                        except GitMergeException, ex:
                            # interactive merge failed
                            out.error(str(ex))
                            if str(keeporig) != 'yes':
                                __remove_files(orig_hash, file1_hash,
                                               file2_hash)
                            __conflict(path)
                            return 1
                        # successful interactive merge
                        os.system('git-update-index -- %s' % path)
                        __remove_files(orig_hash, file1_hash, file2_hash)
                        return 0
                    else:
                        # no interactive merge, just mark it as conflict
                        if str(keeporig) != 'yes':
                            __remove_files(orig_hash, file1_hash, file2_hash)
                        __conflict(path)
                        return 1

        # file deleted in both or deleted in one and unchanged in the other
        elif not (file1_hash or file2_hash) \
               or file1_hash == orig_hash or file2_hash == orig_hash:
            if os.path.exists(path):
                os.remove(path)
            __remove_files(orig_hash, file1_hash, file2_hash)
            return os.system('git-update-index --remove -- %s' % path)
        # file deleted in one and changed in the other
        else:
            # Do something here - we must at least merge the entry in
            # the cache, instead of leaving it in U(nmerged) state. In
            # fact, stg resolved does not handle that.

            # Do the same thing cogito does - remove the file in any case.
            os.system('git-update-index --remove -- %s' % path)

            #if file1_hash:
                ## file deleted upstream and changed in the patch. The
                ## patch is probably going to move the changes
                ## elsewhere.

                #os.system('git-update-index --remove -- %s' % path)
            #else:
                ## file deleted in the patch and changed upstream. We
                ## could re-delete it, but for now leave it there -
                ## and let the user check if he still wants to remove
                ## the file.

                ## reset the cache to the first branch
                #os.system('git-update-index --cacheinfo %s %s %s'
                #          % (file1_mode, file1_hash, path))
            __conflict(path)
            return 1

    # file does not exist in origin
    else:
        # file added in both
        if file1_hash and file2_hash:
            # files are the same
            if file1_hash == file2_hash:
                if os.system('git-update-index --add --cacheinfo %s %s %s'
                             % (file1_mode, file1_hash, path)) != 0:
                    out.error('git-update-index failed')
                    __conflict(path)
                    return 1
                if os.system('git-checkout-index -u -f -- %s' % path):
                    out.error('git-checkout-index failed')
                    __conflict(path)
                    return 1
                if file1_mode != file2_mode:
                    out.error('File "s" added in both, permissions conflict'
                              % path)
                    __conflict(path)
                    return 1
            # files added in both but different
            else:
                out.error('File "%s" added in branches but different' % path)
                # reset the cache to the first branch
                os.system('git-update-index --cacheinfo %s %s %s'
                          % (file1_mode, file1_hash, path))

                if config.get('stgit.autoimerge') == 'yes':
                    try:
                        interactive_merge(path)
                    except GitMergeException, ex:
                        # interactive merge failed
                        out.error(str(ex))
                        if str(keeporig) != 'yes':
                            __remove_files(orig_hash, file1_hash,
                                           file2_hash)
                        __conflict(path)
                        return 1
                    # successful interactive merge
                    os.system('git-update-index -- %s' % path)
                    __remove_files(orig_hash, file1_hash, file2_hash)
                    return 0
                else:
                    # no interactive merge, just mark it as conflict
                    if str(keeporig) != 'yes':
                        __remove_files(orig_hash, file1_hash, file2_hash)
                    __conflict(path)
                    return 1
        # file added in one
        elif file1_hash or file2_hash:
            if file1_hash:
                mode = file1_mode
                obj = file1_hash
            else:
                mode = file2_mode
                obj = file2_hash
            if os.system('git-update-index --add --cacheinfo %s %s %s'
                         % (mode, obj, path)) != 0:
                out.error('git-update-index failed')
                __conflict(path)
                return 1
            __remove_files(orig_hash, file1_hash, file2_hash)
            return os.system('git-checkout-index -u -f -- %s' % path)

    # Unhandled case
    out.error('Unhandled merge conflict: "%s" "%s" "%s" "%s" "%s" "%s" "%s"'
              % (orig_hash, file1_hash, file2_hash,
                 path,
                 orig_mode, file1_mode, file2_mode))
    __conflict(path)
    return 1
