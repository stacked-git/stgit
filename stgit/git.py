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

import sys, os, glob, popen2

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

        lines = _output_lines('git-cat-file commit %s' % id_hash)
        for i in range(len(lines)):
            line = lines[i]
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
        self.__log = ''.join(lines[i:])

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

def _input(cmd, file_desc):
    p = popen2.Popen3(cmd)
    for line in file_desc:
        p.tochild.write(line)
    p.tochild.close()
    if p.wait():
        raise GitException, '%s failed' % str(cmd)

def _output(cmd):
    p=popen2.Popen3(cmd)
    string = p.fromchild.read()
    if p.wait():
        raise GitException, '%s failed' % str(cmd)
    return string

def _output_one_line(cmd):
    p=popen2.Popen3(cmd)
    string = p.fromchild.readline().strip()
    if p.wait():
        raise GitException, '%s failed' % str(cmd)
    return string

def _output_lines(cmd):
    p=popen2.Popen3(cmd)
    lines = p.fromchild.readlines()
    if p.wait():
        raise GitException, '%s failed' % str(cmd)
    return lines

def __run(cmd, args=None):
    """__run: runs cmd using spawnvp.

    Runs cmd using spawnvp.  The shell is avoided so it won't mess up
    our arguments.  If args is very large, the command is run multiple
    times; args is split xargs style: cmd is passed on each
    invocation.  Unlike xargs, returns immediately if any non-zero
    return code is received.  
    """
    
    args_l=cmd.split()
    if args is None:
        args = []
    for i in range(0, len(args)+1, 100):
        r=os.spawnvp(os.P_WAIT, args_l[0], args_l + args[i:min(i+100, len(args))])
    if r:
        return r
    return 0

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
        extra_exclude = []
        if os.path.exists(exclude_file):
            extra_exclude.append('--exclude-from=%s' % exclude_file)
        lines = _output_lines(['git-ls-files', '--others',
                        '--exclude=*.[ao]', '--exclude=.*'
                        '--exclude=TAGS', '--exclude=tags', '--exclude=*~',
                        '--exclude=#*'] + extra_exclude)
        cache_files += [('?', line.strip()) for line in lines]

    # conflicted files
    conflicts = get_conflicts()
    if not conflicts:
        conflicts = []
    cache_files += [('C', filename) for filename in conflicts]

    # the rest
    for line in _output_lines(['git-diff-cache', '-r', tree_id] + files):
        fs = tuple(line.rstrip().split(' ',4)[-1].split('\t',1))
        if fs[1] not in conflicts:
            cache_files.append(fs)

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

    if files:
        if __run('git-update-cache --add --', files):
            raise GitException, 'Unable to add file'

def rm(files, force = False):
    """Remove a file from the repository
    """
    if force:
        git_opt = '--force-remove'
    else:
        git_opt = '--remove'

    if not force:
        for f in files:
            if os.path.exists(f):
                raise GitException, '%s exists. Remove it first' %f
        if files:
            __run('git-update-cache --remove --', files)
    else:
        if files:
            __run('git-update-cache --force-remove --', files)

def update_cache(files):
    """Update the cache information for the given files
    """
    files_here = []
    files_gone = []

    for f in files:
        if os.path.exists(f):
            files_here.append(f)
        else:
            files_gone.append(f)

    if files_here:
        __run('git-update-cache --', files_here)
    if files_gone:
        __run('git-update-cache --remove --', files_gone)

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
    if not first and len(filter(lambda x: x[0] not in ['M', 'N', 'A', 'D'],
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
        add_files=[]
        rm_files=[]
        m_files=[]
        for f in cache_files:
            if f[0] in ['N', 'A']:
                add_files.append(f[1])
            elif f[0] == 'D':
                rm_files.append(f[1])
            else:
                m_files.append(f[1])

    if add_files:
        if __run('git-update-cache --add --', add_files):
            raise GitException, 'Failed git-update-cache --add'
    if rm_files:
        if __run('git-update-cache --force-remove --', rm_files):
            raise GitException, 'Failed git-update-cache --rm'
    if m_files:
        if __run('git-update-cache --', m_files):
            raise GitException, 'Failed git-update-cache'

    # write the index to repository
    tree_id = _output_one_line('git-write-tree')

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

    commit_id = _output_one_line(cmd)
    __set_head(commit_id)
    os.remove('.commitmsg')

    return commit_id

def merge(base, head1, head2):
    """Perform a 3-way merge between base, head1 and head2 into the
    local tree
    """
    if __run('git-read-tree -u -m', [base, head1, head2]) != 0:
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
            filestat.append('A')
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

def diff(files = [], rev1 = 'HEAD', rev2 = None, out_fd = None):
    """Show the diff between rev1 and rev2
    """
    os.system('git-update-cache --refresh > /dev/null')

    if rev2:
        diff_str = _output(['git-diff-tree', '-p', rev1, rev2] + files)
    else:
        diff_str = _output(['git-diff-cache', '-p', rev1] + files)

    if out_fd:
        out_fd.write(diff_str)
    else:
        return diff_str

def diffstat(files = [], rev1 = 'HEAD', rev2 = None):
    """Return the diffstat between rev1 and rev2
    """

    os.system('git-update-cache --refresh > /dev/null')
    p=popen2.Popen3('git-apply --stat')
    diff(files, rev1, rev2, p.tochild)
    p.tochild.close()
    str = p.fromchild.read().rstrip()
    if p.wait():
        raise GitException, 'git.diffstat failed'
    return str

def files(rev1, rev2):
    """Return the files modified between rev1 and rev2
    """
    os.system('git-update-cache --refresh > /dev/null')

    str = ''
    for line in _output_lines('git-diff-tree -r %s %s' % (rev1, rev2)):
        str += '%s %s\n' % tuple(line.rstrip().split(' ',4)[-1].split('\t',1))

    return str.rstrip()

def checkout(files = [], tree_id = None, force = False):
    """Check out the given or all files
    """
    if tree_id and __run('git-read-tree -m', [tree_id]) != 0:
        raise GitException, 'Failed git-read-tree -m %s' % tree_id

    checkout_cmd = 'git-checkout-cache -q -u'
    if force:
        checkout_cmd += ' -f'
    if len(files) == 0:
        checkout_cmd += ' -a'
    else:
        checkout_cmd += ' --'

    if __run(checkout_cmd, files) != 0:
        raise GitException, 'Failed git-checkout-cache'

def switch(tree_id):
    """Switch the tree to the given id
    """
    to_delete = filter(lambda x: x[0] in ['N', 'A'],
                       __tree_status(tree_id = tree_id))

    checkout(tree_id = tree_id, force = True)
    __set_head(tree_id)

    # checkout doesn't remove files
    for fs in to_delete:
        os.remove(fs[1])

def pull(location, head = None, tag = None):
    """Fetch changes from the remote repository. At the moment, just
    use the 'git fetch' scripts
    """
    args = [location]
    if head:
        args += [head]
    elif tag:
        args += ['tag', tag]

    if __run('git pull', args) != 0:
        raise GitException, 'Failed "git fetch %s"' % location

def apply_patch(filename = None):
    """Apply a patch onto the current index. There must not be any
    local changes in the tree, otherwise the command fails
    """
    os.system('git-update-cache --refresh > /dev/null')

    if filename:
        if __run('git-apply --index', [filename]) != 0:
            raise GitException, 'Patch does not apply cleanly'
    else:
        _input('git-apply --index', sys.stdin)

def clone(repository, local_dir):
    """Clone a remote repository. At the moment, just use the
    'git clone' script
    """
    if __run('git clone', [repository, local_dir]) != 0:
        raise GitException, 'Failed "git clone %s %s"' \
              % (repository, local_dir)
