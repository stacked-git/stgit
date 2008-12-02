"""A Python class hierarchy wrapping the StGit on-disk metadata."""

import os.path
from stgit import exception, utils
from stgit.lib import git, stackupgrade
from stgit.config import config

class StackException(exception.StgException):
    """Exception raised by L{stack} objects."""

class Patch(object):
    """Represents an StGit patch. This class is mainly concerned with
    reading and writing the on-disk representation of a patch."""
    def __init__(self, stack, name):
        self.__stack = stack
        self.__name = name
    name = property(lambda self: self.__name)
    @property
    def __ref(self):
        return 'refs/patches/%s/%s' % (self.__stack.name, self.__name)
    @property
    def __log_ref(self):
        return self.__ref + '.log'
    @property
    def commit(self):
        return self.__stack.repository.refs.get(self.__ref)
    @property
    def old_commit(self):
        """Return the previous commit for this patch."""
        fn = os.path.join(self.__compat_dir, 'top.old')
        if not os.path.isfile(fn):
            return None
        return self.__stack.repository.get_commit(utils.read_string(fn))
    @property
    def __compat_dir(self):
        return os.path.join(self.__stack.directory, 'patches', self.__name)
    def __write_compat_files(self, new_commit, msg):
        """Write files used by the old infrastructure."""
        def write(name, val, multiline = False):
            fn = os.path.join(self.__compat_dir, name)
            if val:
                utils.write_string(fn, val, multiline)
            elif os.path.isfile(fn):
                os.remove(fn)
        def write_patchlog():
            try:
                old_log = [self.__stack.repository.refs.get(self.__log_ref)]
            except KeyError:
                old_log = []
            cd = git.CommitData(tree = new_commit.data.tree, parents = old_log,
                                message = '%s\t%s' % (msg, new_commit.sha1))
            c = self.__stack.repository.commit(cd)
            self.__stack.repository.refs.set(self.__log_ref, c, msg)
            return c
        d = new_commit.data
        write('authname', d.author.name)
        write('authemail', d.author.email)
        write('authdate', d.author.date)
        write('commname', d.committer.name)
        write('commemail', d.committer.email)
        write('description', d.message, multiline = True)
        write('log', write_patchlog().sha1)
        write('top', new_commit.sha1)
        write('bottom', d.parent.sha1)
        try:
            old_top_sha1 = self.commit.sha1
            old_bottom_sha1 = self.commit.data.parent.sha1
        except KeyError:
            old_top_sha1 = None
            old_bottom_sha1 = None
        write('top.old', old_top_sha1)
        write('bottom.old', old_bottom_sha1)
    def __delete_compat_files(self):
        if os.path.isdir(self.__compat_dir):
            for f in os.listdir(self.__compat_dir):
                os.remove(os.path.join(self.__compat_dir, f))
            os.rmdir(self.__compat_dir)
        try:
            # this compatibility log ref might not exist
            self.__stack.repository.refs.delete(self.__log_ref)
        except KeyError:
            pass
    def set_commit(self, commit, msg):
        self.__write_compat_files(commit, msg)
        self.__stack.repository.refs.set(self.__ref, commit, msg)
    def delete(self):
        self.__delete_compat_files()
        self.__stack.repository.refs.delete(self.__ref)
    def is_applied(self):
        return self.name in self.__stack.patchorder.applied
    def is_empty(self):
        return self.commit.data.is_nochange()
    def files(self):
        """Return the set of files this patch touches."""
        fs = set()
        for (_, _, _, _, _, oldname, newname
             ) in self.__stack.repository.diff_tree_files(
            self.commit.data.tree, self.commit.data.parent.data.tree):
            fs.add(oldname)
            fs.add(newname)
        return fs

class PatchOrder(object):
    """Keeps track of patch order, and which patches are applied.
    Works with patch names, not actual patches."""
    def __init__(self, stack):
        self.__stack = stack
        self.__lists = {}
    def __read_file(self, fn):
        return tuple(utils.read_strings(
            os.path.join(self.__stack.directory, fn)))
    def __write_file(self, fn, val):
        utils.write_strings(os.path.join(self.__stack.directory, fn), val)
    def __get_list(self, name):
        if not name in self.__lists:
            self.__lists[name] = self.__read_file(name)
        return self.__lists[name]
    def __set_list(self, name, val):
        val = tuple(val)
        if val != self.__lists.get(name, None):
            self.__lists[name] = val
            self.__write_file(name, val)
    applied = property(lambda self: self.__get_list('applied'),
                       lambda self, val: self.__set_list('applied', val))
    unapplied = property(lambda self: self.__get_list('unapplied'),
                         lambda self, val: self.__set_list('unapplied', val))
    hidden = property(lambda self: self.__get_list('hidden'),
                      lambda self, val: self.__set_list('hidden', val))
    all = property(lambda self: self.applied + self.unapplied + self.hidden)
    all_visible = property(lambda self: self.applied + self.unapplied)

    @staticmethod
    def create(stackdir):
        """Create the PatchOrder specific files
        """
        utils.create_empty_file(os.path.join(stackdir, 'applied'))
        utils.create_empty_file(os.path.join(stackdir, 'unapplied'))
        utils.create_empty_file(os.path.join(stackdir, 'hidden'))

class Patches(object):
    """Creates L{Patch} objects. Makes sure there is only one such object
    per patch."""
    def __init__(self, stack):
        self.__stack = stack
        def create_patch(name):
            p = Patch(self.__stack, name)
            p.commit # raise exception if the patch doesn't exist
            return p
        self.__patches = git.ObjectCache(create_patch) # name -> Patch
    def exists(self, name):
        try:
            self.get(name)
            return True
        except KeyError:
            return False
    def get(self, name):
        return self.__patches[name]
    def new(self, name, commit, msg):
        assert not name in self.__patches
        p = Patch(self.__stack, name)
        p.set_commit(commit, msg)
        self.__patches[name] = p
        return p

class Stack(git.Branch):
    """Represents an StGit stack (that is, a git branch with some extra
    metadata)."""
    __repo_subdir = 'patches'

    def __init__(self, repository, name):
        git.Branch.__init__(self, repository, name)
        self.__patchorder = PatchOrder(self)
        self.__patches = Patches(self)
        if not stackupgrade.update_to_current_format_version(repository, name):
            raise StackException('%s: branch not initialized' % name)
    patchorder = property(lambda self: self.__patchorder)
    patches = property(lambda self: self.__patches)
    @property
    def directory(self):
        return os.path.join(self.repository.directory, self.__repo_subdir, self.name)
    @property
    def base(self):
        if self.patchorder.applied:
            return self.patches.get(self.patchorder.applied[0]
                                    ).commit.data.parent
        else:
            return self.head
    @property
    def top(self):
        """Commit of the topmost patch, or the stack base if no patches are
        applied."""
        if self.patchorder.applied:
            return self.patches.get(self.patchorder.applied[-1]).commit
        else:
            # When no patches are applied, base == head.
            return self.head
    def head_top_equal(self):
        if not self.patchorder.applied:
            return True
        return self.head == self.patches.get(self.patchorder.applied[-1]).commit

    def set_parents(self, remote, branch):
        if remote:
            self.set_parent_remote(remote)
        if branch:
            self.set_parent_branch(branch)

    @classmethod
    def initialise(cls, repository, name = None):
        """Initialise a Git branch to handle patch series.

        @param repository: The L{Repository} where the L{Stack} will be created
        @param name: The name of the L{Stack}
        """
        if not name:
            name = repository.current_branch_name
        # make sure that the corresponding Git branch exists
        git.Branch(repository, name)

        dir = os.path.join(repository.directory, cls.__repo_subdir, name)
        compat_dir = os.path.join(dir, 'patches')
        if os.path.exists(dir):
            raise StackException('%s: branch already initialized' % name)

        # create the stack directory and files
        utils.create_dirs(dir)
        utils.create_dirs(compat_dir)
        PatchOrder.create(dir)
        config.set(stackupgrade.format_version_key(name),
                   str(stackupgrade.FORMAT_VERSION))

        return repository.get_stack(name)

    @classmethod
    def create(cls, repository, name,
               create_at = None, parent_remote = None, parent_branch = None):
        """Create and initialise a Git branch returning the L{Stack} object.

        @param repository: The L{Repository} where the L{Stack} will be created
        @param name: The name of the L{Stack}
        @param create_at: The Git id used as the base for the newly created
            Git branch
        @param parent_remote: The name of the remote Git branch
        @param parent_branch: The name of the parent Git branch
        """
        git.Branch.create(repository, name, create_at = create_at)
        stack = cls.initialise(repository, name)
        stack.set_parents(parent_remote, parent_branch)
        return stack

class Repository(git.Repository):
    """A git L{Repository<git.Repository>} with some added StGit-specific
    operations."""
    def __init__(self, *args, **kwargs):
        git.Repository.__init__(self, *args, **kwargs)
        self.__stacks = {} # name -> Stack
    @property
    def current_stack(self):
        return self.get_stack()
    def get_stack(self, name = None):
        if not name:
            name = self.current_branch_name
        if not name in self.__stacks:
            self.__stacks[name] = Stack(self, name)
        return self.__stacks[name]
