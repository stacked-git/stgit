"""Python GIT interface
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

import sys, os, glob

from stgit.utils import *

# git exception class
class GitException(Exception):
    pass


# Different start-up variables read from the environment
if 'GIT_DIR' in os.environ:
    base_dir = os.environ['GIT_DIR']
else:
    base_dir = '.git'

head_link = os.path.join(base_dir, 'HEAD')


#
# Classes
#
class Commit:
    """Handle the commit objects
    """
    def __init__(self, id_hash):
        self.__id_hash = id_hash
        f = os.popen('git-cat-file commit %s' % id_hash, 'r')

        for line in f:
            if line == '\n':
                break
            field = line.strip().split(' ', 1)
            if field[0] == 'tree':
                self.__tree = field[1]
            elif field[0] == 'parent':
                self.__parent = field[1]
            if field[0] == 'author':
                self.__author = field[1]
            if field[0] == 'comitter':
                self.__committer = field[1]
        self.__log = f.read()

        if f.close():
            raise GitException, 'Unknown commit id'

    def get_id_hash(self):
        return self.__id_hash

    def get_tree(self):
        return self.__tree

    def get_parent(self):
        return self.__parent

    def get_author(self):
        return self.__author

    def get_committer(self):
        return self.__committer


#
# Functions
#
def get_conflicts():
    """Return the list of file conflicts
    """
    conflicts_file = os.path.join(base_dir, 'conflicts')
    if os.path.isfile(conflicts_file):
        f = file(conflicts_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names
    else:
        return None

def __output(cmd):
    f = os.popen(cmd, 'r')
    string = f.readline().strip()
    if f.close():
        raise GitException, '%s failed' % cmd
    return string

def __check_base_dir():
    return os.path.isdir(base_dir)

def __tree_status(files = [], tree_id = 'HEAD', unknown = False):
    """Returns a list of pairs - [status, filename]
    """
    os.system('git-update-cache --refresh > /dev/null')

    cache_files = []

    # unknown files
    if unknown:
        exclude_file = os.path.join(base_dir, 'exclude')
        extra_exclude = ''
        if os.path.exists(exclude_file):
            extra_exclude += ' --exclude-from=%s' % exclude_file
        fout = os.popen('git-ls-files --others'
                        ' --exclude="*.[ao]" --exclude=".*"'
                        ' --exclude=TAGS --exclude=tags --exclude="*~"'
                        ' --exclude="#*"' + extra_exclude, 'r')
        cache_files += [('?', line.strip()) for line in fout]

    # conflicted files
    conflicts = get_conflicts()
    if not conflicts:
        conflicts = []
    cache_files += [('C', filename) for filename in conflicts]

    # the rest
    files_str = reduce(lambda x, y: x + ' ' + y, files, '')
    fout = os.popen('git-diff-cache -r %s %s' % (tree_id, files_str), 'r')
    for line in fout:
        fs = tuple(line.split()[4:])
        if fs[1] not in conflicts:
            cache_files.append(fs)
    if fout.close():
        raise GitException, 'git-diff-cache failed'

    return cache_files

def local_changes():
    """Return true if there are local changes in the tree
    """
    return len(__tree_status()) != 0

def get_head():
    """Returns a string representing the HEAD
    """
    return read_string(head_link)

def get_head_file():
    """Returns the name of the file pointed to by the HEAD link
    """
    # valid link
    if os.path.islink(head_link) and os.path.isfile(head_link):
        return os.path.basename(os.readlink(head_link))
    else:
        raise GitException, 'Invalid .git/HEAD link. Git tree not initialised?'

def __set_head(val):
    """Sets the HEAD value
    """
    write_string(head_link, val)

def add(names):
    """Add the files or recursively add the directory contents
    """
    # generate the file list
    files = []
    for i in names:
        if not os.path.exists(i):
            raise GitException, 'Unknown file or directory: %s' % i

        if os.path.isdir(i):
            # recursive search. We only add files
            for root, dirs, local_files in os.walk(i):
                for name in [os.path.join(root, f) for f in local_files]:
                    if os.path.isfile(name):
                        files.append(os.path.normpath(name))
        elif os.path.isfile(i):
            files.append(os.path.normpath(i))
        else:
            raise GitException, '%s is not a file or directory' % i

    for f in files:
        print 'Adding file %s' % f
        if os.system('git-update-cache --add -- %s' % f) != 0:
            raise GitException, 'Unable to add %s' % f

def rm(files, force = False):
    """Remove a file from the repository
    """
    if force:
        git_opt = '--force-remove'
    else:
        git_opt = '--remove'

    for f in files:
        if force:
            print 'Removing file %s' % f
            if os.system('git-update-cache --force-remove -- %s' % f) != 0:
                raise GitException, 'Unable to remove %s' % f
        elif os.path.exists(f):
            raise GitException, '%s exists. Remove it first' %f
        else:
            print 'Removing file %s' % f
            if os.system('git-update-cache --remove -- %s' % f) != 0:
                raise GitException, 'Unable to remove %s' % f

def update_cache(files):
    """Update the cache information for the given files
    """
    for f in files:
        if os.path.exists(f):
            os.system('git-update-cache -- %s' % f)
        else:
            os.system('git-update-cache --remove -- %s' % f)

def commit(message, files = [], parents = [], allowempty = False,
           author_name = None, author_email = None, author_date = None,
           committer_name = None, committer_email = None):
    """Commit the current tree to repository
    """
    first = (parents == [])

    # Get the tree status
    if not first:
        cache_files = __tree_status(files)

    if not first and len(cache_files) == 0 and not allowempty:
        raise GitException, 'No changes to commit'

    # check for unresolved conflicts
    if not first and len(filter(lambda x: x[0] not in ['M', 'N', 'D'],
                                cache_files)) != 0:
        raise GitException, 'Commit failed: unresolved conflicts'

    # get the commit message
    f = file('.commitmsg', 'w+')
    if message[-1] == '\n':
        f.write(message)
    else:
        print >> f, message
    f.close()

    # update the cache
    if not first:
        for f in cache_files:
            if f[0] == 'N':
                git_flag = '--add'
            elif f[0] == 'D':
                git_flag = '--force-remove'
            else:
                git_flag = '--'

            if os.system('git-update-cache %s %s' % (git_flag, f[1])) != 0:
                raise GitException, 'Failed git-update-cache -- %s' % f[1]

    # write the index to repository
    tree_id = __output('git-write-tree')

    # the commit
    cmd = ''
    if author_name:
        cmd += 'GIT_AUTHOR_NAME="%s" ' % author_name
    if author_email:
        cmd += 'GIT_AUTHOR_EMAIL="%s" ' % author_email
    if author_date:
        cmd += 'GIT_AUTHOR_DATE="%s" ' % author_date
    if committer_name:
        cmd += 'GIT_COMMITTER_NAME="%s" ' % committer_name
    if committer_email:
        cmd += 'GIT_COMMITTER_EMAIL="%s" ' % committer_email
    cmd += 'git-commit-tree %s' % tree_id

    # get the parents
    for p in parents:
        cmd += ' -p %s' % p

    cmd += ' < .commitmsg'

    commit_id = __output(cmd)
    __set_head(commit_id)
    os.remove('.commitmsg')

    return commit_id

def merge(base, head1, head2):
    """Perform a 3-way merge between base, head1 and head2 into the
    local tree
    """
    if os.system('git-read-tree -u -m %s %s %s' % (base, head1, head2)) != 0:
        raise GitException, 'git-read-tree failed (local changes maybe?)'

    # this can fail if there are conflicts
    if os.system('git-merge-cache -o gitmergeonefile.py -a') != 0:
        raise GitException, 'git-merge-cache failed (possible conflicts)'

    # this should not fail
    if os.system('git-checkout-cache -f -a') != 0:
        raise GitException, 'Failed git-checkout-cache'

def status(files = [], modified = False, new = False, deleted = False,
           conflict = False, unknown = False):
    """Show the tree status
    """
    cache_files = __tree_status(files, unknown = True)
    all = not (modified or new or deleted or conflict or unknown)

    if not all:
        filestat = []
        if modified:
            filestat.append('M')
        if new:
            filestat.append('N')
        if deleted:
            filestat.append('D')
        if conflict:
            filestat.append('C')
        if unknown:
            filestat.append('?')
        cache_files = filter(lambda x: x[0] in filestat, cache_files)

    for fs in cache_files:
        if all:
            print '%s %s' % (fs[0], fs[1])
        else:
            print '%s' % fs[1]

def diff(files = [], rev1 = 'HEAD', rev2 = None, output = None,
         append = False):
    """Show the diff between rev1 and rev2
    """
    files_str = reduce(lambda x, y: x + ' ' + y, files, '')

    extra_args = ''
    if output:
        if append:
            extra_args += ' >> %s' % output
        else:
            extra_args += ' > %s' % output

    os.system('git-update-cache --refresh > /dev/null')

    if rev2:
        if os.system('git-diff-tree -p %s %s %s %s'
                     % (rev1, rev2, files_str, extra_args)) != 0:
            raise GitException, 'git-diff-tree failed'
    else:
        if os.system('git-diff-cache -p %s %s %s'
                     % (rev1, files_str, extra_args)) != 0:
            raise GitException, 'git-diff-cache failed'

def diffstat(files = [], rev1 = 'HEAD', rev2 = None):
    """Return the diffstat between rev1 and rev2
    """
    files_str = reduce(lambda x, y: x + ' ' + y, files, '')

    os.system('git-update-cache --refresh > /dev/null')
    ds_cmd = '| git-apply --stat'

    if rev2:
        f = os.popen('git-diff-tree -p %s %s %s %s'
                     % (rev1, rev2, files_str, ds_cmd), 'r')
        str = f.read().rstrip()
        if f.close():
            raise GitException, 'git-diff-tree failed'
    else:
        f = os.popen('git-diff-cache -p %s %s %s'
                     % (rev1, files_str, ds_cmd), 'r')
        str = f.read().rstrip()
        if f.close():
            raise GitException, 'git-diff-cache failed'

    return str

def files(rev1, rev2):
    """Return the files modified between rev1 and rev2
    """
    os.system('git-update-cache --refresh > /dev/null')

    str = ''
    f = os.popen('git-diff-tree -r %s %s' % (rev1, rev2),
                 'r')
    for line in f:
        str += '%s %s\n' % tuple(line.split()[4:])
    if f.close():
        raise GitException, 'git-diff-tree failed'

    return str.rstrip()

def checkout(files = [], force = False):
    """Check out the given or all files
    """
    git_flags = ''
    if force:
        git_flags += ' -f'
    if len(files) == 0:
        git_flags += ' -a'
    else:
        git_flags += reduce(lambda x, y: x + ' ' + y, files, ' --')

    if os.system('git-checkout-cache -q -u%s' % git_flags) != 0:
        raise GitException, 'Failed git-checkout-cache -q -u%s' % git_flags

def switch(tree_id):
    """Switch the tree to the given id
    """
    to_delete = filter(lambda x: x[0] == 'N', __tree_status(tree_id = tree_id))

    if os.system('git-read-tree -m %s' % tree_id) != 0:
        raise GitException, 'Failed git-read-tree -m %s' % tree_id

    checkout(force = True)
    __set_head(tree_id)

    # checkout doesn't remove files
    for fs in to_delete:
        os.remove(fs[1])
