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

import sys, os, re
from shutil import copyfile

from stgit.exception import *
from stgit import basedir
from stgit.utils import *
from stgit.out import *
from stgit.run import *
from stgit.config import config

# git exception class
class GitException(StgException):
    pass

# When a subprocess has a problem, we want the exception to be a
# subclass of GitException.
class GitRunException(GitException):
    pass
class GRun(Run):
    exc = GitRunException
    def __init__(self, *cmd):
        """Initialise the Run object and insert the 'git' command name.
        """
        Run.__init__(self, 'git', *cmd)

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

        lines = GRun('cat-file', 'commit', id_hash).output_lines()
        for i in range(len(lines)):
            line = lines[i]
            if not line:
                break # we've seen all the header fields
            key, val = line.split(' ', 1)
            if key == 'tree':
                self.__tree = val
            elif key == 'author':
                self.__author = val
            elif key == 'committer':
                self.__committer = val
            else:
                pass # ignore other headers
        self.__log = '\n'.join(lines[i+1:])

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
        return GRun('rev-list', '--parents', '--max-count=1', self.__id_hash
                    ).output_one_line().split()[1:]

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
    names = set()
    for line in GRun('ls-files', '-z', '--unmerged'
                     ).raw_output().split('\0')[:-1]:
        stat, path = line.split('\t', 1)
        names.add(path)
    return list(names)

def exclude_files():
    files = [os.path.join(basedir.get(), 'info', 'exclude')]
    user_exclude = config.get('core.excludesfile')
    if user_exclude:
        files.append(user_exclude)
    return files

def ls_files(files, tree = 'HEAD', full_name = True):
    """Return the files known to GIT or raise an error otherwise. It also
    converts the file to the full path relative the the .git directory.
    """
    if not files:
        return []

    args = []
    if tree:
        args.append('--with-tree=%s' % tree)
    if full_name:
        args.append('--full-name')
    args.append('--')
    args.extend(files)
    try:
        # use a set to avoid file names duplication due to different stages
        fileset = set(GRun('ls-files', '--error-unmatch', *args).output_lines())
    except GitRunException:
        # just hide the details of the 'git ls-files' command we use
        raise GitException, \
            'Some of the given paths are either missing or not known to GIT'
    return list(fileset)

def parse_git_ls(output):
    """Parse the output of git diff-index, diff-files, etc. Doesn't handle
    rename/copy output, so don't feed it output generated with the -M
    or -C flags."""
    t = None
    for line in output.split('\0'):
        if not line:
            # There's a zero byte at the end of the output, which
            # gives us an empty string as the last "line".
            continue
        if t == None:
            mode_a, mode_b, sha1_a, sha1_b, t = line.split(' ')
        else:
            yield (t, line)
            t = None

def tree_status(files = None, tree_id = 'HEAD', unknown = False,
                  noexclude = True, verbose = False):
    """Get the status of all changed files, or of a selected set of
    files. Returns a list of pairs - (status, filename).

    If 'not files', it will check all files, and optionally all
    unknown files.  If 'files' is a list, it will only check the files
    in the list.
    """
    assert not files or not unknown

    if verbose:
        out.start('Checking for changes in the working directory')

    refresh_index()

    if files is None:
        files = []
    cache_files = []

    # unknown files
    if unknown:
        cmd = ['ls-files', '-z', '--others', '--directory',
               '--no-empty-directory']
        if not noexclude:
            cmd += ['--exclude=%s' % s for s in
                    ['*.[ao]', '*.pyc', '.*', '*~', '#*', 'TAGS', 'tags']]
            cmd += ['--exclude-per-directory=.gitignore']
            cmd += ['--exclude-from=%s' % fn
                    for fn in exclude_files()
                    if os.path.exists(fn)]

        lines = GRun(*cmd).raw_output().split('\0')
        cache_files += [('?', line) for line in lines if line]

    # conflicted files
    conflicts = get_conflicts()
    cache_files += [('C', filename) for filename in conflicts
                    if not files or filename in files]
    reported_files = set(conflicts)
    files_left = [f for f in files if f not in reported_files]

    # files in the index. Only execute this code if no files were
    # specified when calling the function (i.e. report all files) or
    # files were specified but already found in the previous step
    if not files or files_left:
        args = [tree_id]
        if files_left:
            args += ['--'] + files_left
        for t, fn in parse_git_ls(GRun('diff-index', '-z', *args).raw_output()):
            # the condition is needed in case files is emtpy and
            # diff-index lists those already reported
            if not fn in reported_files:
                cache_files.append((t, fn))
                reported_files.add(fn)
        files_left = [f for f in files if f not in reported_files]

    # files in the index but changed on (or removed from) disk. Only
    # execute this code if no files were specified when calling the
    # function (i.e. report all files) or files were specified but
    # already found in the previous step
    if not files or files_left:
        args = []
        if files_left:
            args += ['--'] + files_left
        for t, fn in parse_git_ls(GRun('diff-files', '-z', *args).raw_output()):
            # the condition is needed in case files is empty and
            # diff-files lists those already reported
            if not fn in reported_files:
                cache_files.append((t, fn))
                reported_files.add(fn)

    if verbose:
        out.done()

    return cache_files

def local_changes(verbose = True):
    """Return true if there are local changes in the tree
    """
    return len(tree_status(verbose = verbose)) != 0

def get_heads():
    heads = []
    hr = re.compile(r'^[0-9a-f]{40} refs/heads/(.+)$')
    for line in GRun('show-ref', '--heads').output_lines():
        m = hr.match(line)
        heads.append(m.group(1))
    return heads

# HEAD value cached
__head = None

def get_head():
    """Verifies the HEAD and returns the SHA1 id that represents it
    """
    global __head

    if not __head:
        __head = rev_parse('HEAD')
    return __head

class DetachedHeadException(GitException):
    def __init__(self):
        GitException.__init__(self, 'Not on any branch')

def get_head_file():
    """Return the name of the file pointed to by the HEAD symref.
    Throw an exception if HEAD is detached."""
    try:
        return strip_prefix(
            'refs/heads/', GRun('symbolic-ref', '-q', 'HEAD'
                                ).output_one_line())
    except GitRunException:
        raise DetachedHeadException()

def set_head_file(ref):
    """Resets HEAD to point to a new ref
    """
    # head cache flushing is needed since we might have a different value
    # in the new head
    __clear_head_cache()
    try:
        GRun('symbolic-ref', 'HEAD', 'refs/heads/%s' % ref).run()
    except GitRunException:
        raise GitException, 'Could not set head to "%s"' % ref

def set_ref(ref, val):
    """Point ref at a new commit object."""
    try:
        GRun('update-ref', ref, val).run()
    except GitRunException:
        raise GitException, 'Could not update %s to "%s".' % (ref, val)

def set_branch(branch, val):
    set_ref('refs/heads/%s' % branch, val)

def __set_head(val):
    """Sets the HEAD value
    """
    global __head

    if not __head or __head != val:
        set_ref('HEAD', val)
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
    GRun('update-index', '-q', '--unmerged', '--refresh').run()

def rev_parse(git_id):
    """Parse the string and return a verified SHA1 id
    """
    try:
        return GRun('rev-parse', '--verify', git_id
                    ).discard_stderr().output_one_line()
    except GitRunException:
        raise GitException, 'Unknown revision: %s' % git_id

def ref_exists(ref):
    try:
        rev_parse(ref)
        return True
    except GitException:
        return False

def branch_exists(branch):
    return ref_exists('refs/heads/%s' % branch)

def create_branch(new_branch, tree_id = None):
    """Create a new branch in the git repository
    """
    if branch_exists(new_branch):
        raise GitException, 'Branch "%s" already exists' % new_branch

    current_head_file = get_head_file()
    current_head = get_head()
    set_head_file(new_branch)
    __set_head(current_head)

    # a checkout isn't needed if new branch points to the current head
    if tree_id:
        try:
            switch(tree_id)
        except GitException:
            # Tree switching failed. Revert the head file
            set_head_file(current_head_file)
            delete_branch(new_branch)
            raise

    if os.path.isfile(os.path.join(basedir.get(), 'MERGE_HEAD')):
        os.remove(os.path.join(basedir.get(), 'MERGE_HEAD'))

def switch_branch(new_branch):
    """Switch to a git branch
    """
    global __head

    if not branch_exists(new_branch):
        raise GitException, 'Branch "%s" does not exist' % new_branch

    tree_id = rev_parse('refs/heads/%s^{commit}' % new_branch)
    if tree_id != get_head():
        refresh_index()
        try:
            GRun('read-tree', '-u', '-m', get_head(), tree_id).run()
        except GitRunException:
            raise GitException, 'read-tree failed (local changes maybe?)'
        __head = tree_id
    set_head_file(new_branch)

    if os.path.isfile(os.path.join(basedir.get(), 'MERGE_HEAD')):
        os.remove(os.path.join(basedir.get(), 'MERGE_HEAD'))

def delete_ref(ref):
    if not ref_exists(ref):
        raise GitException, '%s does not exist' % ref
    sha1 = GRun('show-ref', '-s', ref).output_one_line()
    try:
        GRun('update-ref', '-d', ref, sha1).run()
    except GitRunException:
        raise GitException, 'Failed to delete ref %s' % ref

def delete_branch(name):
    delete_ref('refs/heads/%s' % name)

def rename_ref(from_ref, to_ref):
    if not ref_exists(from_ref):
        raise GitException, '"%s" does not exist' % from_ref
    if ref_exists(to_ref):
        raise GitException, '"%s" already exists' % to_ref

    sha1 = GRun('show-ref', '-s', from_ref).output_one_line()
    try:
        GRun('update-ref', to_ref, sha1, '0'*40).run()
    except GitRunException:
        raise GitException, 'Failed to create new ref %s' % to_ref
    try:
        GRun('update-ref', '-d', from_ref, sha1).run()
    except GitRunException:
        raise GitException, 'Failed to delete ref %s' % from_ref

def rename_branch(from_name, to_name):
    """Rename a git branch."""
    rename_ref('refs/heads/%s' % from_name, 'refs/heads/%s' % to_name)
    try:
        if get_head_file() == from_name:
            set_head_file(to_name)
    except DetachedHeadException:
        pass # detached HEAD, so the renamee can't be the current branch
    reflog_dir = os.path.join(basedir.get(), 'logs', 'refs', 'heads')
    if os.path.exists(reflog_dir) \
           and os.path.exists(os.path.join(reflog_dir, from_name)):
        rename(reflog_dir, from_name, to_name)

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

    GRun('update-index', '--add', '--').xargs(add_files)
    GRun('update-index', '--force-remove', '--').xargs(rm_files)
    GRun('update-index', '--').xargs(m_files)

    return True

def commit(message, files = None, parents = None, allowempty = False,
           cache_update = True, tree_id = None, set_head = False,
           author_name = None, author_email = None, author_date = None,
           committer_name = None, committer_email = None):
    """Commit the current tree to repository
    """
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
        tree_id = GRun('write-tree').output_one_line()
        set_head = True

    # the commit
    env = {}
    if author_name:
        env['GIT_AUTHOR_NAME'] = author_name
    if author_email:
        env['GIT_AUTHOR_EMAIL'] = author_email
    if author_date:
        env['GIT_AUTHOR_DATE'] = author_date
    if committer_name:
        env['GIT_COMMITTER_NAME'] = committer_name
    if committer_email:
        env['GIT_COMMITTER_EMAIL'] = committer_email
    commit_id = GRun('commit-tree', tree_id,
                     *sum([['-p', p] for p in parents], [])
                     ).env(env).raw_input(message).output_one_line()
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
            GRun('apply', *index_opt).raw_input(
                diff_str).discard_stderr().no_output()
        except GitRunException:
            return False

    return True

stages_re = re.compile('^([0-7]+) ([0-9a-f]{40}) ([1-3])\t(.*)$', re.S)

def merge_recursive(base, head1, head2):
    """Perform a 3-way merge between base, head1 and head2 into the
    local tree
    """
    refresh_index()
    p = GRun('merge-recursive', base, '--', head1, head2).env(
        { 'GITHEAD_%s' % base: 'ancestor',
          'GITHEAD_%s' % head1: 'current',
          'GITHEAD_%s' % head2: 'patched'}).returns([0, 1])
    output = p.output_lines()
    if p.exitcode:
        # There were conflicts
        if config.get('stgit.autoimerge') == 'yes':
            mergetool()
        else:
            conflicts = [l for l in output if l.startswith('CONFLICT')]
            out.info(*conflicts)
            raise GitException, "%d conflict(s)" % len(conflicts)

def mergetool(files = ()):
    """Invoke 'git mergetool' to resolve any outstanding conflicts. If 'not
    files', all the files in an unmerged state will be processed."""
    GRun('mergetool', *list(files)).returns([0, 1]).run()
    # check for unmerged entries (prepend 'CONFLICT ' for consistency with
    # merge_recursive())
    conflicts = ['CONFLICT ' + f for f in get_conflicts()]
    if conflicts:
        out.info(*conflicts)
        raise GitException, "%d conflict(s)" % len(conflicts)

def diff(files = None, rev1 = 'HEAD', rev2 = None, diff_flags = [],
         binary = True):
    """Show the diff between rev1 and rev2
    """
    if not files:
        files = []
    if binary and '--binary' not in diff_flags:
        diff_flags = diff_flags + ['--binary']

    if rev1 and rev2:
        return GRun('diff-tree', '-p',
                    *(diff_flags + [rev1, rev2, '--'] + files)).raw_output()
    elif rev1 or rev2:
        refresh_index()
        if rev2:
            return GRun('diff-index', '-p', '-R',
                        *(diff_flags + [rev2, '--'] + files)).raw_output()
        else:
            return GRun('diff-index', '-p',
                        *(diff_flags + [rev1, '--'] + files)).raw_output()
    else:
        return ''

def files(rev1, rev2, diff_flags = []):
    """Return the files modified between rev1 and rev2
    """

    result = []
    for line in GRun('diff-tree', *(diff_flags + ['-r', rev1, rev2])
                     ).output_lines():
        result.append('%s %s' % tuple(line.split(' ', 4)[-1].split('\t', 1)))

    return '\n'.join(result)

def barefiles(rev1, rev2):
    """Return the files modified between rev1 and rev2, without status info
    """

    result = []
    for line in GRun('diff-tree', '-r', rev1, rev2).output_lines():
        result.append(line.split(' ', 4)[-1].split('\t', 1)[-1])

    return '\n'.join(result)

def pretty_commit(commit_id = 'HEAD', flags = []):
    """Return a given commit (log + diff)
    """
    return GRun('show', *(flags + [commit_id])).raw_output()

def checkout(files = None, tree_id = None, force = False):
    """Check out the given or all files
    """
    if tree_id:
        try:
            GRun('read-tree', '--reset', tree_id).run()
        except GitRunException:
            raise GitException, 'Failed "git read-tree" --reset %s' % tree_id

    cmd = ['checkout-index', '-q', '-u']
    if force:
        cmd.append('-f')
    if files:
        GRun(*(cmd + ['--'])).xargs(files)
    else:
        GRun(*(cmd + ['-a'])).run()

def switch(tree_id, keep = False):
    """Switch the tree to the given id
    """
    if keep:
        # only update the index while keeping the local changes
        GRun('read-tree', tree_id).run()
    else:
        refresh_index()
        try:
            GRun('read-tree', '-u', '-m', get_head(), tree_id).run()
        except GitRunException:
            raise GitException, 'read-tree failed (local changes maybe?)'

    __set_head(tree_id)

def reset(files = None, tree_id = None, check_out = True):
    """Revert the tree changes relative to the given tree_id. It removes
    any local changes
    """
    if not tree_id:
        tree_id = get_head()

    if check_out:
        checkout(files, tree_id, True)

    # if the reset refers to the whole tree, switch the HEAD as well
    if not files:
        __set_head(tree_id)

def resolved(filenames, reset = None):
    if reset:
        stage = {'ancestor': 1, 'current': 2, 'patched': 3}[reset]
        GRun('checkout-index', '--no-create', '--stage=%d' % stage,
             '--stdin', '-z').input_nulterm(filenames).no_output()
    GRun('update-index', '--add', '--').xargs(filenames)
    for filename in filenames:
        # update the access and modificatied times
        os.utime(filename, None)

def fetch(repository = 'origin', refspec = None):
    """Fetches changes from the remote repository, using 'git fetch'
    by default.
    """
    # we update the HEAD
    __clear_head_cache()

    args = [repository]
    if refspec:
        args.append(refspec)

    command = config.get('branch.%s.stgit.fetchcmd' % get_head_file()) or \
              config.get('stgit.fetchcmd')
    Run(*(command.split() + args)).run()

def pull(repository = 'origin', refspec = None):
    """Fetches changes from the remote repository, using 'git pull'
    by default.
    """
    # we update the HEAD
    __clear_head_cache()

    args = [repository]
    if refspec:
        args.append(refspec)

    command = config.get('branch.%s.stgit.pullcmd' % get_head_file()) or \
              config.get('stgit.pullcmd')
    Run(*(command.split() + args)).run()

def rebase(tree_id = None):
    """Rebase the current tree to the give tree_id. The tree_id
    argument may be something other than a GIT id if an external
    command is invoked.
    """
    command = config.get('branch.%s.stgit.rebasecmd' % get_head_file()) \
                or config.get('stgit.rebasecmd')
    if tree_id:
        args = [tree_id]
    elif command:
        args = []
    else:
        raise GitException, 'Default rebasing requires a commit id'
    if command:
        # clear the HEAD cache as the custom rebase command will update it
        __clear_head_cache()
        Run(*(command.split() + args)).run()
    else:
        # default rebasing
        reset(tree_id = tree_id)

def repack():
    """Repack all objects into a single pack
    """
    GRun('repack', '-a', '-d', '-f').run()

def apply_patch(filename = None, diff = None, base = None,
                reject = False, strip = None):
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

    cmd = ['apply', '--index']
    if reject:
        cmd += ['--reject']
    if strip != None:
        cmd += ['-p%s' % (strip,)]
    try:
        GRun(*cmd).raw_input(diff).no_output()
    except GitRunException:
        if base:
            switch(orig_head)
        raise GitException('Diff does not apply cleanly')

    if base:
        top = commit(message = 'temporary commit used for applying a patch',
                     parents = [base])
        switch(orig_head)
        merge_recursive(base, orig_head, top)

def clone(repository, local_dir):
    """Clone a remote repository. At the moment, just use the
    'git clone' script
    """
    GRun('clone', repository, local_dir).run()

def modifying_revs(files, base_rev, head_rev):
    """Return the revisions from the list modifying the given files."""
    return GRun('rev-list', '%s..%s' % (base_rev, head_rev), '--', *files
                ).output_lines()

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
        return []

def remotes_list():
    """Return the list of remotes in the repository
    """
    return (set(__remotes_from_config())
            | set(__remotes_from_dir('remotes'))
            | set(__remotes_from_dir('branches')))

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
                raise GitException, 'StGit does not support multiple FETCH_HEAD'
            else:
                fetch_head=m.group(1)
    stream.close()

    if not fetch_head:
        out.warn('No for-merge remote head found in FETCH_HEAD')

    # here we are sure to have a single fetch_head
    return fetch_head

def all_refs():
    """Return a list of all refs in the current repository.
    """

    return [line.split()[1] for line in GRun('show-ref').output_lines()]
