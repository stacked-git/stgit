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
        self.__parents = []
        for i in range(len(lines)):
            line = lines[i]
            if line == '\n':
                break
            field = line.strip().split(' ', 1)
            if field[0] == 'tree':
                self.__tree = field[1]
            elif field[0] == 'parent':
                self.__parents.append(field[1])
            if field[0] == 'author':
                self.__author = field[1]
            if field[0] == 'committer':
                self.__committer = field[1]
        self.__log = ''.join(lines[i+1:])

    def get_id_hash(self):
        return self.__id_hash

    def get_tree(self):
        return self.__tree

    def get_parent(self):
        return self.__parents[0]

    def get_parents(self):
        return self.__parents

    def get_author(self):
        return self.__author

    def get_committer(self):
        return self.__committer

    def get_log(self):
        return self.__log

# dictionary of Commit objects, used to avoid multiple calls to git
__commits = dict()

#
# Functions
#
def get_commit(id_hash):
    """Commit objects factory. Save/look-up them in the __commits
    dictionary
    """
    global __commits

    if id_hash in __commits:
        return __commits[id_hash]
    else:
        commit = Commit(id_hash)
        __commits[id_hash] = commit
        return commit

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
    while True:
        line = file_desc.readline()
        if not line:
            break
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

def _output_one_line(cmd, file_desc = None):
    p=popen2.Popen3(cmd)
    if file_desc != None:
        for line in file_desc:
            p.tochild.write(line)
        p.tochild.close()
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

def __tree_status(files = [], tree_id = 'HEAD', unknown = False,
                  noexclude = True):
    """Returns a list of pairs - [status, filename]
    """
    os.system('git-update-index --refresh > /dev/null')

    cache_files = []

    # unknown files
    if unknown:
        exclude_file = os.path.join(base_dir, 'info', 'exclude')
        base_exclude = ['--exclude=%s' % s for s in
                        ['*.[ao]', '*.pyc', '.*', '*~', '#*', 'TAGS', 'tags']]
        base_exclude.append('--exclude-per-directory=.gitignore')

        if os.path.exists(exclude_file):
            extra_exclude = ['--exclude-from=%s' % exclude_file]
        else:
            extra_exclude = []
        if noexclude:
            extra_exclude = base_exclude = []

        lines = _output_lines(['git-ls-files', '--others'] + base_exclude
                        + extra_exclude)
        cache_files += [('?', line.strip()) for line in lines]

    # conflicted files
    conflicts = get_conflicts()
    if not conflicts:
        conflicts = []
    cache_files += [('C', filename) for filename in conflicts]

    # the rest
    for line in _output_lines(['git-diff-index', '-r', tree_id] + files):
        fs = tuple(line.rstrip().split(' ',4)[-1].split('\t',1))
        if fs[1] not in conflicts:
            cache_files.append(fs)

    return cache_files

def local_changes():
    """Return true if there are local changes in the tree
    """
    return len(__tree_status()) != 0

# HEAD value cached
__head = None

def get_head():
    """Verifies the HEAD and returns the SHA1 id that represents it
    """
    global __head

    if not __head:
        __head = rev_parse('HEAD')
    return __head

def get_head_file():
    """Returns the name of the file pointed to by the HEAD link
    """
    return os.path.basename(_output_one_line('git-symbolic-ref HEAD'))

def set_head_file(ref):
    """Resets HEAD to point to a new ref
    """
    if __run('git-symbolic-ref HEAD', [ref]) != 0:
        raise GitException, 'Could not set head to "%s"' % ref

def __set_head(val):
    """Sets the HEAD value
    """
    global __head

    if not __head or __head != val:
        if __run('git-update-ref HEAD', [val]) != 0:
            raise GitException, 'Could not update HEAD to "%s".' % val
        __head = val

def __clear_head_cache():
    """Sets the __head to None so that a re-read is forced
    """
    global __head

    __head = None

def rev_parse(git_id):
    """Parse the string and return a verified SHA1 id
    """
    try:
        return _output_one_line(['git-rev-parse', '--verify', git_id])
    except GitException:
        raise GitException, 'Unknown revision: %s' % git_id

def branch_exists(branch):
    """Existance check for the named branch
    """
    for line in _output_lines(['git-rev-parse', '--symbolic', '--all']):
        if line.strip() == branch:
            return True
    return False

def create_branch(new_branch, tree_id = None):
    """Create a new branch in the git repository
    """
    new_head = os.path.join('refs', 'heads', new_branch)
    if branch_exists(new_head):
        raise GitException, 'Branch "%s" already exists' % new_branch

    current_head = get_head()
    set_head_file(new_head)
    __set_head(current_head)

    # a checkout isn't needed if new branch points to the current head
    if tree_id:
        git.switch(tree_id)

    if os.path.isfile(os.path.join(base_dir, 'MERGE_HEAD')):
        os.remove(os.path.join(base_dir, 'MERGE_HEAD'))

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
        if __run('git-update-index --add --', files):
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
            __run('git-update-index --remove --', files)
    else:
        if files:
            __run('git-update-index --force-remove --', files)

def update_cache(files = [], force = False):
    """Update the cache information for the given files
    """
    cache_files = __tree_status(files)

    # everything is up-to-date
    if len(cache_files) == 0:
        return False

    # check for unresolved conflicts
    if not force and [x for x in cache_files
                      if x[0] not in ['M', 'N', 'A', 'D']]:
        raise GitException, 'Updating cache failed: unresolved conflicts'

    # update the cache
    add_files = [x[1] for x in cache_files if x[0] in ['N', 'A']]
    rm_files =  [x[1] for x in cache_files if x[0] in ['D']]
    m_files =   [x[1] for x in cache_files if x[0] in ['M']]

    if add_files and __run('git-update-index --add --', add_files) != 0:
        raise GitException, 'Failed git-update-index --add'
    if rm_files and __run('git-update-index --force-remove --', rm_files) != 0:
        raise GitException, 'Failed git-update-index --rm'
    if m_files and __run('git-update-index --', m_files) != 0:
        raise GitException, 'Failed git-update-index'

    return True

def commit(message, files = [], parents = [], allowempty = False,
           cache_update = True, tree_id = None,
           author_name = None, author_email = None, author_date = None,
           committer_name = None, committer_email = None):
    """Commit the current tree to repository
    """
    # Get the tree status
    if cache_update and parents != []:
        changes = update_cache(files)
        if not changes and not allowempty:
            raise GitException, 'No changes to commit'

    # get the commit message
    if message[-1:] != '\n':
        message += '\n'

    must_switch = True
    # write the index to repository
    if tree_id == None:
        tree_id = _output_one_line('git-write-tree')
    else:
        must_switch = False

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

    commit_id = _output_one_line(cmd, message)
    if must_switch:
        __set_head(commit_id)

    return commit_id

def apply_diff(rev1, rev2):
    """Apply the diff between rev1 and rev2 onto the current
    index. This function doesn't need to raise an exception since it
    is only used for fast-pushing a patch. If this operation fails,
    the pushing would fall back to the three-way merge.
    """
    return os.system('git-diff-tree -p %s %s | git-apply --index 2> /dev/null'
                     % (rev1, rev2)) == 0

def merge(base, head1, head2):
    """Perform a 3-way merge between base, head1 and head2 into the
    local tree
    """
    if __run('git-read-tree -u -m', [base, head1, head2]) != 0:
        raise GitException, 'git-read-tree failed (local changes maybe?)'

    # this can fail if there are conflicts
    if os.system('git-merge-index -o -q gitmergeonefile.py -a') != 0:
        raise GitException, 'git-merge-cache failed (possible conflicts)'

def status(files = [], modified = False, new = False, deleted = False,
           conflict = False, unknown = False, noexclude = False):
    """Show the tree status
    """
    cache_files = __tree_status(files, unknown = True, noexclude = noexclude)
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
        cache_files = [x for x in cache_files if x[0] in filestat]

    for fs in cache_files:
        if all:
            print '%s %s' % (fs[0], fs[1])
        else:
            print '%s' % fs[1]

def diff(files = [], rev1 = 'HEAD', rev2 = None, out_fd = None):
    """Show the diff between rev1 and rev2
    """

    if rev2:
        diff_str = _output(['git-diff-tree', '-p', rev1, rev2] + files)
    else:
        os.system('git-update-index --refresh > /dev/null')
        diff_str = _output(['git-diff-index', '-p', rev1] + files)

    if out_fd:
        out_fd.write(diff_str)
    else:
        return diff_str

def diffstat(files = [], rev1 = 'HEAD', rev2 = None):
    """Return the diffstat between rev1 and rev2
    """

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

    str = ''
    for line in _output_lines('git-diff-tree -r %s %s' % (rev1, rev2)):
        str += '%s %s\n' % tuple(line.rstrip().split(' ',4)[-1].split('\t',1))

    return str.rstrip()

def barefiles(rev1, rev2):
    """Return the files modified between rev1 and rev2, without status info
    """

    str = ''
    for line in _output_lines('git-diff-tree -r %s %s' % (rev1, rev2)):
        str += '%s\n' % line.rstrip().split(' ',4)[-1].split('\t',1)[-1]

    return str.rstrip()

def checkout(files = [], tree_id = None, force = False):
    """Check out the given or all files
    """
    if tree_id and __run('git-read-tree -m', [tree_id]) != 0:
        raise GitException, 'Failed git-read-tree -m %s' % tree_id

    checkout_cmd = 'git-checkout-index -q -u'
    if force:
        checkout_cmd += ' -f'
    if len(files) == 0:
        checkout_cmd += ' -a'
    else:
        checkout_cmd += ' --'

    if __run(checkout_cmd, files) != 0:
        raise GitException, 'Failed git-checkout-index'

def switch(tree_id):
    """Switch the tree to the given id
    """
    if __run('git-read-tree -u -m', [get_head(), tree_id]) != 0:
        raise GitException, 'git-read-tree failed (local changes maybe?)'

    __set_head(tree_id)

def reset(tree_id = None):
    """Revert the tree changes relative to the given tree_id. It removes
    any local changes
    """
    if not tree_id:
        tree_id = get_head()

    cache_files = __tree_status(tree_id = tree_id)
    rm_files =  [x[1] for x in cache_files if x[0] in ['D']]

    checkout(tree_id = tree_id, force = True)
    __set_head(tree_id)

    # checkout doesn't remove files
    map(os.remove, rm_files)

def pull(repository = 'origin', refspec = None):
    """Pull changes from the remote repository. At the moment, just
    use the 'git pull' command
    """
    # 'git pull' updates the HEAD
    __clear_head_cache()

    args = [repository]
    if refspec:
        args.append(refspec)

    if __run('git pull', args) != 0:
        raise GitException, 'Failed "git pull %s"' % repository

def apply_patch(filename = None, base = None):
    """Apply a patch onto the current or given index. There must not
    be any local changes in the tree, otherwise the command fails
    """
    def __apply_patch():
        if filename:
            return __run('git-apply --index', [filename]) == 0
        else:
            try:
                _input('git-apply --index', sys.stdin)
            except GitException:
                return False
            return True

    os.system('git-update-index --refresh > /dev/null')

    if base:
        orig_head = get_head()
        switch(base)

    if not __apply_patch():
        if base:
            switch(orig_head)
        raise GitException, 'Patch does not apply cleanly'
    elif base:
        top = commit(message = 'temporary commit used for applying a patch',
                     parents = [base])
        switch(orig_head)
        merge(base, orig_head, top)

def clone(repository, local_dir):
    """Clone a remote repository. At the moment, just use the
    'git clone' script
    """
    if __run('git clone', [repository, local_dir]) != 0:
        raise GitException, 'Failed "git clone %s %s"' \
              % (repository, local_dir)
