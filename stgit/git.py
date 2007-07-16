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

import sys, os, popen2, re, gitmergeonefile
from shutil import copyfile

from stgit import basedir
from stgit.utils import *
from stgit.config import config
from sets import Set

# git exception class
class GitException(Exception):
    pass



#
# Classes
#

class Person:
    """An author, committer, etc."""
    def __init__(self, name = None, email = None, date = '',
                 desc = None):
        self.name = self.email = self.date = None
        if name or email or date:
            assert not desc
            self.name = name
            self.email = email
            self.date = date
        elif desc:
            assert not (name or email or date)
            def parse_desc(s):
                m = re.match(r'^(.+)<(.+)>(.*)$', s)
                assert m
                return [x.strip() or None for x in m.groups()]
            self.name, self.email, self.date = parse_desc(desc)
    def set_name(self, val):
        if val:
            self.name = val
    def set_email(self, val):
        if val:
            self.email = val
    def set_date(self, val):
        if val:
            self.date = val
    def __str__(self):
        if self.name and self.email:
            return '%s <%s>' % (self.name, self.email)
        else:
            raise GitException, 'not enough identity data'

class Commit:
    """Handle the commit objects
    """
    def __init__(self, id_hash):
        self.__id_hash = id_hash

        lines = _output_lines(['git-cat-file', 'commit', id_hash])
        for i in range(len(lines)):
            line = lines[i]
            if line == '\n':
                break
            field = line.strip().split(' ', 1)
            if field[0] == 'tree':
                self.__tree = field[1]
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
        parents = self.get_parents()
        if parents:
            return parents[0]
        else:
            return None

    def get_parents(self):
        return _output_lines(['git-rev-list', '--parents', '--max-count=1',
                              self.__id_hash])[0].split()[1:]

    def get_author(self):
        return self.__author

    def get_committer(self):
        return self.__committer

    def get_log(self):
        return self.__log

    def __str__(self):
        return self.get_id_hash()

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
    conflicts_file = os.path.join(basedir.get(), 'conflicts')
    if os.path.isfile(conflicts_file):
        f = file(conflicts_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names
    else:
        return None

def _input(cmd, file_desc):
    p = popen2.Popen3(cmd, True)
    while True:
        line = file_desc.readline()
        if not line:
            break
        p.tochild.write(line)
    p.tochild.close()
    if p.wait():
        raise GitException, '%s failed (%s)' % (' '.join(cmd),
                                                p.childerr.read().strip())

def _input_str(cmd, string):
    p = popen2.Popen3(cmd, True)
    p.tochild.write(string)
    p.tochild.close()
    if p.wait():
        raise GitException, '%s failed (%s)' % (' '.join(cmd),
                                                p.childerr.read().strip())

def _output(cmd):
    p=popen2.Popen3(cmd, True)
    output = p.fromchild.read()
    if p.wait():
        raise GitException, '%s failed (%s)' % (' '.join(cmd),
                                                p.childerr.read().strip())
    return output

def _output_one_line(cmd, file_desc = None):
    p=popen2.Popen3(cmd, True)
    if file_desc != None:
        for line in file_desc:
            p.tochild.write(line)
        p.tochild.close()
    output = p.fromchild.readline().strip()
    if p.wait():
        raise GitException, '%s failed (%s)' % (' '.join(cmd),
                                                p.childerr.read().strip())
    return output

def _output_lines(cmd):
    p=popen2.Popen3(cmd, True)
    lines = p.fromchild.readlines()
    if p.wait():
        raise GitException, '%s failed (%s)' % (' '.join(cmd),
                                                p.childerr.read().strip())
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

def tree_status(files = None, tree_id = 'HEAD', unknown = False,
                  noexclude = True, verbose = False, diff_flags = []):
    """Returns a list of pairs - [status, filename]
    """
    if verbose:
        out.start('Checking for changes in the working directory')

    refresh_index()

    if not files:
        files = []
    cache_files = []

    # unknown files
    if unknown:
        exclude_file = os.path.join(basedir.get(), 'info', 'exclude')
        base_exclude = ['--exclude=%s' % s for s in
                        ['*.[ao]', '*.pyc', '.*', '*~', '#*', 'TAGS', 'tags']]
        base_exclude.append('--exclude-per-directory=.gitignore')

        if os.path.exists(exclude_file):
            extra_exclude = ['--exclude-from=%s' % exclude_file]
        else:
            extra_exclude = []
        if noexclude:
            extra_exclude = base_exclude = []

        lines = _output_lines(['git-ls-files', '--others', '--directory']
                        + base_exclude + extra_exclude)
        cache_files += [('?', line.strip()) for line in lines]

    # conflicted files
    conflicts = get_conflicts()
    if not conflicts:
        conflicts = []
    cache_files += [('C', filename) for filename in conflicts]

    # the rest
    for line in _output_lines(['git-diff-index'] + diff_flags +
                              [ tree_id, '--'] + files):
        fs = tuple(line.rstrip().split(' ',4)[-1].split('\t',1))
        if fs[1] not in conflicts:
            cache_files.append(fs)

    if verbose:
        out.done()

    return cache_files

def local_changes(verbose = True):
    """Return true if there are local changes in the tree
    """
    return len(tree_status(verbose = verbose)) != 0

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
    return strip_prefix('refs/heads/',
                        _output_one_line(['git-symbolic-ref', 'HEAD']))

def set_head_file(ref):
    """Resets HEAD to point to a new ref
    """
    # head cache flushing is needed since we might have a different value
    # in the new head
    __clear_head_cache()
    if __run('git-symbolic-ref HEAD',
             [os.path.join('refs', 'heads', ref)]) != 0:
        raise GitException, 'Could not set head to "%s"' % ref

def set_branch(branch, val):
    """Point branch at a new commit object."""
    if __run('git-update-ref', [branch, val]) != 0:
        raise GitException, 'Could not update %s to "%s".' % (branch, val)

def __set_head(val):
    """Sets the HEAD value
    """
    global __head

    if not __head or __head != val:
        set_branch('HEAD', val)
        __head = val

    # only allow SHA1 hashes
    assert(len(__head) == 40)

def __clear_head_cache():
    """Sets the __head to None so that a re-read is forced
    """
    global __head

    __head = None

def refresh_index():
    """Refresh index with stat() information from the working directory.
    """
    __run('git-update-index -q --unmerged --refresh')

def rev_parse(git_id):
    """Parse the string and return a verified SHA1 id
    """
    try:
        return _output_one_line(['git-rev-parse', '--verify', git_id])
    except GitException:
        raise GitException, 'Unknown revision: %s' % git_id

def branch_exists(branch):
    """Existence check for the named branch
    """
    branch = os.path.join('refs', 'heads', branch)
    for line in _output_lines('git-rev-parse --symbolic --all 2>&1'):
        if line.strip() == branch:
            return True
        if re.compile('[ |/]'+branch+' ').search(line):
            raise GitException, 'Bogus branch: %s' % line
    return False

def create_branch(new_branch, tree_id = None):
    """Create a new branch in the git repository
    """
    if branch_exists(new_branch):
        raise GitException, 'Branch "%s" already exists' % new_branch

    current_head = get_head()
    set_head_file(new_branch)
    __set_head(current_head)

    # a checkout isn't needed if new branch points to the current head
    if tree_id:
        switch(tree_id)

    if os.path.isfile(os.path.join(basedir.get(), 'MERGE_HEAD')):
        os.remove(os.path.join(basedir.get(), 'MERGE_HEAD'))

def switch_branch(new_branch):
    """Switch to a git branch
    """
    global __head

    if not branch_exists(new_branch):
        raise GitException, 'Branch "%s" does not exist' % new_branch

    tree_id = rev_parse(os.path.join('refs', 'heads', new_branch)
                        + '^{commit}')
    if tree_id != get_head():
        refresh_index()
        if __run('git-read-tree -u -m', [get_head(), tree_id]) != 0:
            raise GitException, 'git-read-tree failed (local changes maybe?)'
        __head = tree_id
    set_head_file(new_branch)

    if os.path.isfile(os.path.join(basedir.get(), 'MERGE_HEAD')):
        os.remove(os.path.join(basedir.get(), 'MERGE_HEAD'))

def delete_branch(name):
    """Delete a git branch
    """
    if not branch_exists(name):
        raise GitException, 'Branch "%s" does not exist' % name
    remove_file_and_dirs(os.path.join(basedir.get(), 'refs', 'heads'),
                         name)

def rename_branch(from_name, to_name):
    """Rename a git branch
    """
    if not branch_exists(from_name):
        raise GitException, 'Branch "%s" does not exist' % from_name
    if branch_exists(to_name):
        raise GitException, 'Branch "%s" already exists' % to_name

    if get_head_file() == from_name:
        set_head_file(to_name)
    rename(os.path.join(basedir.get(), 'refs', 'heads'),
           from_name, to_name)

    reflog_dir = os.path.join(basedir.get(), 'logs', 'refs', 'heads')
    if os.path.exists(reflog_dir) \
           and os.path.exists(os.path.join(reflog_dir, from_name)):
        rename(reflog_dir, from_name, to_name)

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

def __copy_single(source, target, target2=''):
    """Copy file or dir named 'source' to name target+target2"""

    # "source" (file or dir) must match one or more git-controlled file
    realfiles = _output_lines(['git-ls-files', source])
    if len(realfiles) == 0:
        raise GitException, '"%s" matches no git-controled files' % source

    if os.path.isdir(source):
        # physically copy the files, and record them to add them in one run
        newfiles = []
        re_string='^'+source+'/(.*)$'
        prefix_regexp = re.compile(re_string)
        for f in [f.strip() for f in realfiles]:
            m = prefix_regexp.match(f)
            if not m:
                raise Exception, '"%s" does not match "%s"' % (f, re_string)
            newname = target+target2+'/'+m.group(1)
            if not os.path.exists(os.path.dirname(newname)):
                os.makedirs(os.path.dirname(newname))
            copyfile(f, newname)
            newfiles.append(newname)

        add(newfiles)
    else: # files, symlinks, ...
        newname = target+target2
        copyfile(source, newname)
        add([newname])


def copy(filespecs, target):
    if os.path.isdir(target):
        # target is a directory: copy each entry on the command line,
        # with the same name, into the target
        target = target.rstrip('/')
        
        # first, check that none of the children of the target
        # matching the command line aleady exist
        for filespec in filespecs:
            entry = target+ '/' + os.path.basename(filespec.rstrip('/'))
            if os.path.exists(entry):
                raise GitException, 'Target "%s" already exists' % entry
        
        for filespec in filespecs:
            filespec = filespec.rstrip('/')
            basename = '/' + os.path.basename(filespec)
            __copy_single(filespec, target, basename)

    elif os.path.exists(target):
        raise GitException, 'Target "%s" exists but is not a directory' % target
    elif len(filespecs) != 1:
        raise GitException, 'Cannot copy more than one file to non-directory'

    else:
        # at this point: len(filespecs)==1 and target does not exist

        # check target directory
        targetdir = os.path.dirname(target)
        if targetdir != '' and not os.path.isdir(targetdir):
            raise GitException, 'Target directory "%s" does not exist' % targetdir

        __copy_single(filespecs[0].rstrip('/'), target)
        

def rm(files, force = False):
    """Remove a file from the repository
    """
    if not force:
        for f in files:
            if os.path.exists(f):
                raise GitException, '%s exists. Remove it first' %f
        if files:
            __run('git-update-index --remove --', files)
    else:
        if files:
            __run('git-update-index --force-remove --', files)

# Persons caching
__user = None
__author = None
__committer = None

def user():
    """Return the user information.
    """
    global __user
    if not __user:
        name=config.get('user.name')
        email=config.get('user.email')
        __user = Person(name, email)
    return __user;

def author():
    """Return the author information.
    """
    global __author
    if not __author:
        try:
            # the environment variables take priority over config
            try:
                date = os.environ['GIT_AUTHOR_DATE']
            except KeyError:
                date = ''
            __author = Person(os.environ['GIT_AUTHOR_NAME'],
                              os.environ['GIT_AUTHOR_EMAIL'],
                              date)
        except KeyError:
            __author = user()
    return __author

def committer():
    """Return the author information.
    """
    global __committer
    if not __committer:
        try:
            # the environment variables take priority over config
            try:
                date = os.environ['GIT_COMMITTER_DATE']
            except KeyError:
                date = ''
            __committer = Person(os.environ['GIT_COMMITTER_NAME'],
                                 os.environ['GIT_COMMITTER_EMAIL'],
                                 date)
        except KeyError:
            __committer = user()
    return __committer

def update_cache(files = None, force = False):
    """Update the cache information for the given files
    """
    if not files:
        files = []

    cache_files = tree_status(files, verbose = False)

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

def commit(message, files = None, parents = None, allowempty = False,
           cache_update = True, tree_id = None, set_head = False,
           author_name = None, author_email = None, author_date = None,
           committer_name = None, committer_email = None):
    """Commit the current tree to repository
    """
    if not files:
        files = []
    if not parents:
        parents = []

    # Get the tree status
    if cache_update and parents != []:
        changes = update_cache(files)
        if not changes and not allowempty:
            raise GitException, 'No changes to commit'

    # get the commit message
    if not message:
        message = '\n'
    elif message[-1:] != '\n':
        message += '\n'

    # write the index to repository
    if tree_id == None:
        tree_id = _output_one_line(['git-write-tree'])
        set_head = True

    # the commit
    cmd = ['env']
    if author_name:
        cmd += ['GIT_AUTHOR_NAME=%s' % author_name]
    if author_email:
        cmd += ['GIT_AUTHOR_EMAIL=%s' % author_email]
    if author_date:
        cmd += ['GIT_AUTHOR_DATE=%s' % author_date]
    if committer_name:
        cmd += ['GIT_COMMITTER_NAME=%s' % committer_name]
    if committer_email:
        cmd += ['GIT_COMMITTER_EMAIL=%s' % committer_email]
    cmd += ['git-commit-tree', tree_id]

    # get the parents
    for p in parents:
        cmd += ['-p', p]

    commit_id = _output_one_line(cmd, message)
    if set_head:
        __set_head(commit_id)

    return commit_id

def apply_diff(rev1, rev2, check_index = True, files = None):
    """Apply the diff between rev1 and rev2 onto the current
    index. This function doesn't need to raise an exception since it
    is only used for fast-pushing a patch. If this operation fails,
    the pushing would fall back to the three-way merge.
    """
    if check_index:
        index_opt = ['--index']
    else:
        index_opt = []

    if not files:
        files = []

    diff_str = diff(files, rev1, rev2)
    if diff_str:
        try:
            _input_str(['git-apply'] + index_opt, diff_str)
        except GitException:
            return False

    return True

def merge(base, head1, head2, recursive = False):
    """Perform a 3-way merge between base, head1 and head2 into the
    local tree
    """
    refresh_index()

    err_output = None
    if recursive:
        # this operation tracks renames but it is slower (used in
        # general when pushing or picking patches)
        try:
            # use _output() to mask the verbose prints of the tool
            _output(['git-merge-recursive', base, '--', head1, head2])
        except GitException, ex:
            err_output = str(ex)
            pass
    else:
        # the fast case where we don't track renames (used when the
        # distance between base and heads is small, i.e. folding or
        # synchronising patches)
        if __run('git-read-tree -u -m --aggressive',
                 [base, head1, head2]) != 0:
            raise GitException, 'git-read-tree failed (local changes maybe?)'

    # check the index for unmerged entries
    files = {}
    stages_re = re.compile('^([0-7]+) ([0-9a-f]{40}) ([1-3])\t(.*)$', re.S)

    for line in _output(['git-ls-files', '--unmerged', '--stage', '-z']).split('\0'):
        if not line:
            continue

        mode, hash, stage, path = stages_re.findall(line)[0]

        if not path in files:
            files[path] = {}
            files[path]['1'] = ('', '')
            files[path]['2'] = ('', '')
            files[path]['3'] = ('', '')

        files[path][stage] = (mode, hash)

    if err_output and not files:
        # if no unmerged files, there was probably a different type of
        # error and we have to abort the merge
        raise GitException, err_output

    # merge the unmerged files
    errors = False
    for path in files:
        # remove additional files that might be generated for some
        # newer versions of GIT
        for suffix in [base, head1, head2]:
            if not suffix:
                continue
            fname = path + '~' + suffix
            if os.path.exists(fname):
                os.remove(fname)

        stages = files[path]
        if gitmergeonefile.merge(stages['1'][1], stages['2'][1],
                                 stages['3'][1], path, stages['1'][0],
                                 stages['2'][0], stages['3'][0]) != 0:
            errors = True

    if errors:
        raise GitException, 'GIT index merging failed (possible conflicts)'

def status(files = None, modified = False, new = False, deleted = False,
           conflict = False, unknown = False, noexclude = False,
           diff_flags = []):
    """Show the tree status
    """
    if not files:
        files = []

    cache_files = tree_status(files, unknown = True, noexclude = noexclude,
                                diff_flags = diff_flags)
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
        if files and not fs[1] in files:
            continue
        if all:
            out.stdout('%s %s' % (fs[0], fs[1]))
        else:
            out.stdout('%s' % fs[1])

def diff(files = None, rev1 = 'HEAD', rev2 = None, out_fd = None,
         diff_flags = []):
    """Show the diff between rev1 and rev2
    """
    if not files:
        files = []

    if rev1 and rev2:
        diff_str = _output(['git-diff-tree', '-p'] + diff_flags
                           + [rev1, rev2, '--'] + files)
    elif rev1 or rev2:
        refresh_index()
        if rev2:
            diff_str = _output(['git-diff-index', '-p', '-R']
                               + diff_flags + [rev2, '--'] + files)
        else:
            diff_str = _output(['git-diff-index', '-p']
                               + diff_flags + [rev1, '--'] + files)
    else:
        diff_str = ''

    if out_fd:
        out_fd.write(diff_str)
    else:
        return diff_str

def diffstat(files = None, rev1 = 'HEAD', rev2 = None):
    """Return the diffstat between rev1 and rev2
    """
    if not files:
        files = []

    p=popen2.Popen3('git-apply --stat')
    diff(files, rev1, rev2, p.tochild)
    p.tochild.close()
    diff_str = p.fromchild.read().rstrip()
    if p.wait():
        raise GitException, 'git.diffstat failed'
    return diff_str

def files(rev1, rev2, diff_flags = []):
    """Return the files modified between rev1 and rev2
    """

    result = ''
    for line in _output_lines(['git-diff-tree'] + diff_flags + ['-r', rev1, rev2]):
        result += '%s %s\n' % tuple(line.rstrip().split(' ',4)[-1].split('\t',1))

    return result.rstrip()

def barefiles(rev1, rev2):
    """Return the files modified between rev1 and rev2, without status info
    """

    result = ''
    for line in _output_lines(['git-diff-tree', '-r', rev1, rev2]):
        result += '%s\n' % line.rstrip().split(' ',4)[-1].split('\t',1)[-1]

    return result.rstrip()

def pretty_commit(commit_id = 'HEAD', diff_flags = []):
    """Return a given commit (log + diff)
    """
    return _output(['git-diff-tree'] + diff_flags +
                   ['--cc', '--always', '--pretty', '-r', commit_id])

def checkout(files = None, tree_id = None, force = False):
    """Check out the given or all files
    """
    if not files:
        files = []

    if tree_id and __run('git-read-tree --reset', [tree_id]) != 0:
        raise GitException, 'Failed git-read-tree --reset %s' % tree_id

    checkout_cmd = 'git-checkout-index -q -u'
    if force:
        checkout_cmd += ' -f'
    if len(files) == 0:
        checkout_cmd += ' -a'
    else:
        checkout_cmd += ' --'

    if __run(checkout_cmd, files) != 0:
        raise GitException, 'Failed git-checkout-index'

def switch(tree_id, keep = False):
    """Switch the tree to the given id
    """
    if not keep:
        refresh_index()
        if __run('git-read-tree -u -m', [get_head(), tree_id]) != 0:
            raise GitException, 'git-read-tree failed (local changes maybe?)'

    __set_head(tree_id)

def reset(files = None, tree_id = None, check_out = True):
    """Revert the tree changes relative to the given tree_id. It removes
    any local changes
    """
    if not tree_id:
        tree_id = get_head()

    if check_out:
        cache_files = tree_status(files, tree_id)
        # files which were added but need to be removed
        rm_files =  [x[1] for x in cache_files if x[0] in ['A']]

        checkout(files, tree_id, True)
        # checkout doesn't remove files
        map(os.remove, rm_files)

    # if the reset refers to the whole tree, switch the HEAD as well
    if not files:
        __set_head(tree_id)

def fetch(repository = 'origin', refspec = None):
    """Fetches changes from the remote repository, using 'git-fetch'
    by default.
    """
    # we update the HEAD
    __clear_head_cache()

    args = [repository]
    if refspec:
        args.append(refspec)

    command = config.get('branch.%s.stgit.fetchcmd' % get_head_file()) or \
              config.get('stgit.fetchcmd')
    if __run(command, args) != 0:
        raise GitException, 'Failed "%s %s"' % (command, repository)

def pull(repository = 'origin', refspec = None):
    """Fetches changes from the remote repository, using 'git-pull'
    by default.
    """
    # we update the HEAD
    __clear_head_cache()

    args = [repository]
    if refspec:
        args.append(refspec)

    command = config.get('branch.%s.stgit.pullcmd' % get_head_file()) or \
              config.get('stgit.pullcmd')
    if __run(command, args) != 0:
        raise GitException, 'Failed "%s %s"' % (command, repository)

def repack():
    """Repack all objects into a single pack
    """
    __run('git-repack -a -d -f')

def apply_patch(filename = None, diff = None, base = None,
                fail_dump = True):
    """Apply a patch onto the current or given index. There must not
    be any local changes in the tree, otherwise the command fails
    """
    if diff is None:
        if filename:
            f = file(filename)
        else:
            f = sys.stdin
        diff = f.read()
        if filename:
            f.close()

    if base:
        orig_head = get_head()
        switch(base)
    else:
        refresh_index()

    try:
        _input_str(['git-apply', '--index'], diff)
    except GitException:
        if base:
            switch(orig_head)
        if fail_dump:
            # write the failed diff to a file
            f = file('.stgit-failed.patch', 'w+')
            f.write(diff)
            f.close()
            out.warn('Diff written to the .stgit-failed.patch file')

        raise

    if base:
        top = commit(message = 'temporary commit used for applying a patch',
                     parents = [base])
        switch(orig_head)
        merge(base, orig_head, top)

def clone(repository, local_dir):
    """Clone a remote repository. At the moment, just use the
    'git-clone' script
    """
    if __run('git-clone', [repository, local_dir]) != 0:
        raise GitException, 'Failed "git-clone %s %s"' \
              % (repository, local_dir)

def modifying_revs(files, base_rev, head_rev):
    """Return the revisions from the list modifying the given files
    """
    cmd = ['git-rev-list', '%s..%s' % (base_rev, head_rev), '--']
    revs = [line.strip() for line in _output_lines(cmd + files)]

    return revs


def refspec_localpart(refspec):
    m = re.match('^[^:]*:([^:]*)$', refspec)
    if m:
        return m.group(1)
    else:
        raise GitException, 'Cannot parse refspec "%s"' % line

def refspec_remotepart(refspec):
    m = re.match('^([^:]*):[^:]*$', refspec)
    if m:
        return m.group(1)
    else:
        raise GitException, 'Cannot parse refspec "%s"' % line
    

def __remotes_from_config():
    return config.sections_matching(r'remote\.(.*)\.url')

def __remotes_from_dir(dir):
    d = os.path.join(basedir.get(), dir)
    if os.path.exists(d):
        return os.listdir(d)
    else:
        return None

def remotes_list():
    """Return the list of remotes in the repository
    """

    return Set(__remotes_from_config()) | \
           Set(__remotes_from_dir('remotes')) | \
           Set(__remotes_from_dir('branches'))

def remotes_local_branches(remote):
    """Returns the list of local branches fetched from given remote
    """

    branches = []
    if remote in __remotes_from_config():
        for line in config.getall('remote.%s.fetch' % remote):
            branches.append(refspec_localpart(line))
    elif remote in __remotes_from_dir('remotes'):
        stream = open(os.path.join(basedir.get(), 'remotes', remote), 'r')
        for line in stream:
            # Only consider Pull lines
            m = re.match('^Pull: (.*)\n$', line)
            if m:
                branches.append(refspec_localpart(m.group(1)))
        stream.close()
    elif remote in __remotes_from_dir('branches'):
        # old-style branches only declare one branch
        branches.append('refs/heads/'+remote);
    else:
        raise GitException, 'Unknown remote "%s"' % remote

    return branches

def identify_remote(branchname):
    """Return the name for the remote to pull the given branchname
    from, or None if we believe it is a local branch.
    """

    for remote in remotes_list():
        if branchname in remotes_local_branches(remote):
            return remote

    # if we get here we've found nothing, the branch is a local one
    return None

def fetch_head():
    """Return the git id for the tip of the parent branch as left by
    'git fetch'.
    """

    fetch_head=None
    stream = open(os.path.join(basedir.get(), 'FETCH_HEAD'), "r")
    for line in stream:
        # Only consider lines not tagged not-for-merge
        m = re.match('^([^\t]*)\t\t', line)
        if m:
            if fetch_head:
                raise GitException, "StGit does not support multiple FETCH_HEAD"
            else:
                fetch_head=m.group(1)
    stream.close()

    # here we are sure to have a single fetch_head
    return fetch_head

def all_refs():
    """Return a list of all refs in the current repository.
    """

    return [line.split()[1] for line in _output_lines(['git-show-ref'])]
