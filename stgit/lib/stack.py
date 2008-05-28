import os.path
from stgit import exception, utils
from stgit.lib import git, stackupgrade

class Patch(object):
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
        write('description', d.message)
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
        self.__stack.repository.refs.delete(self.__log_ref)
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
    # don't return the hidden patches, these have to be returned explicitly
    all = property(lambda self: self.applied + self.unapplied)

class Patches(object):
    """Creates Patch objects."""
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

class Stack(object):
    def __init__(self, repository, name):
        self.__repository = repository
        self.__name = name
        try:
            self.head
        except KeyError:
            raise exception.StgException('%s: no such branch' % name)
        self.__patchorder = PatchOrder(self)
        self.__patches = Patches(self)
        if not stackupgrade.update_to_current_format_version(repository, name):
            raise exception.StgException('%s: branch not initialized' % name)
    name = property(lambda self: self.__name)
    repository = property(lambda self: self.__repository)
    patchorder = property(lambda self: self.__patchorder)
    patches = property(lambda self: self.__patches)
    @property
    def directory(self):
        return os.path.join(self.__repository.directory, 'patches', self.__name)
    def __ref(self):
        return 'refs/heads/%s' % self.__name
    @property
    def head(self):
        return self.__repository.refs.get(self.__ref())
    def set_head(self, commit, msg):
        self.__repository.refs.set(self.__ref(), commit, msg)
    @property
    def base(self):
        if self.patchorder.applied:
            return self.patches.get(self.patchorder.applied[0]
                                    ).commit.data.parent
        else:
            return self.head
    def head_top_equal(self):
        if not self.patchorder.applied:
            return True
        return self.head == self.patches.get(self.patchorder.applied[-1]).commit

class Repository(git.Repository):
    def __init__(self, *args, **kwargs):
        git.Repository.__init__(self, *args, **kwargs)
        self.__stacks = {} # name -> Stack
    @property
    def current_branch(self):
        return utils.strip_leading('refs/heads/', self.head)
    @property
    def current_stack(self):
        return self.get_stack(self.current_branch)
    def get_stack(self, name):
        if not name in self.__stacks:
            self.__stacks[name] = Stack(self, name)
        return self.__stacks[name]
