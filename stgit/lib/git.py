"""A Python class hierarchy wrapping a git repository and its
contents."""

import atexit, os, os.path, re, signal
from datetime import datetime, timedelta, tzinfo

from stgit import exception, run, utils
from stgit.config import config

class Immutable(object):
    """I{Immutable} objects cannot be modified once created. Any
    modification methods will return a new object, leaving the
    original object as it was.

    The reason for this is that we want to be able to represent git
    objects, which are immutable, and want to be able to create new
    git objects that are just slight modifications of other git
    objects. (Such as, for example, modifying the commit message of a
    commit object while leaving the rest of it intact. This involves
    creating a whole new commit object that's exactly like the old one
    except for the commit message.)

    The L{Immutable} class doesn't actually enforce immutability --
    that is up to the individual immutable subclasses. It just serves
    as documentation."""

class RepositoryException(exception.StgException):
    """Base class for all exceptions due to failed L{Repository}
    operations."""

class BranchException(exception.StgException):
    """Exception raised by failed L{Branch} operations."""

class DateException(exception.StgException):
    """Exception raised when a date+time string could not be parsed."""
    def __init__(self, string, type):
        exception.StgException.__init__(
            self, '"%s" is not a valid %s' % (string, type))

class DetachedHeadException(RepositoryException):
    """Exception raised when HEAD is detached (that is, there is no
    current branch)."""
    def __init__(self):
        RepositoryException.__init__(self, 'Not on any branch')

class Repr(object):
    """Utility class that defines C{__reps__} in terms of C{__str__}."""
    def __repr__(self):
        return str(self)

class NoValue(object):
    """A handy default value that is guaranteed to be distinct from any
    real argument value."""
    pass

def make_defaults(defaults):
    def d(val, attr, default_fun = lambda: None):
        if val != NoValue:
            return val
        elif defaults != NoValue:
            return getattr(defaults, attr)
        else:
            return default_fun()
    return d

class TimeZone(tzinfo, Repr):
    """A simple time zone class for static offsets from UTC. (We have to
    define our own since Python's standard library doesn't define any
    time zone classes.)"""
    def __init__(self, tzstring):
        m = re.match(r'^([+-])(\d{2}):?(\d{2})$', tzstring)
        if not m:
            raise DateException(tzstring, 'time zone')
        sign = int(m.group(1) + '1')
        try:
            self.__offset = timedelta(hours = sign*int(m.group(2)),
                                      minutes = sign*int(m.group(3)))
        except OverflowError:
            raise DateException(tzstring, 'time zone')
        self.__name = tzstring
    def utcoffset(self, dt):
        return self.__offset
    def tzname(self, dt):
        return self.__name
    def dst(self, dt):
        return timedelta(0)
    def __str__(self):
        return self.__name

def system_date(datestring):
    m = re.match(r"^(.+)([+-]\d\d:?\d\d)$", datestring)
    if m:
        # Time zone included; we parse it ourselves, since "date"
        # would convert it to the local time zone.
        (ds, z) = m.groups()
        try:
            t = run.Run("date", "+%Y-%m-%d-%H-%M-%S", "-d", ds
                        ).output_one_line()
        except run.RunException:
            return None
    else:
        # Time zone not included; we ask "date" to provide it for us.
        try:
            d = run.Run("date", "+%Y-%m-%d-%H-%M-%S_%z", "-d", datestring
                        ).output_one_line()
        except run.RunException:
            return None
        (t, z) = d.split("_")
    try:
        return datetime(*[int(x) for x in t.split("-")], tzinfo=TimeZone(z))
    except ValueError:
        raise DateException(datestring, "date")

class Date(Immutable, Repr):
    """Represents a timestamp used in git commits."""
    def __init__(self, datestring):
        # Try git-formatted date.
        m = re.match(r'^(\d+)\s+([+-]\d\d:?\d\d)$', datestring)
        if m:
            try:
                self.__time = datetime.fromtimestamp(int(m.group(1)),
                                                     TimeZone(m.group(2)))
            except ValueError:
                raise DateException(datestring, 'date')
            return

        # Try iso-formatted date.
        m = re.match(r'^(\d{4})-(\d{2})-(\d{2})\s+(\d{2}):(\d{2}):(\d{2})\s+'
                     + r'([+-]\d\d:?\d\d)$', datestring)
        if m:
            try:
                self.__time = datetime(
                    *[int(m.group(i + 1)) for i in xrange(6)],
                    **{'tzinfo': TimeZone(m.group(7))})
            except ValueError:
                raise DateException(datestring, 'date')
            return

        # Try parsing with the system's "date" command.
        sd = system_date(datestring)
        if sd:
            self.__time = sd
            return

        raise DateException(datestring, 'date')
    def __str__(self):
        return self.isoformat()
    def isoformat(self):
        """Human-friendly ISO 8601 format."""
        return '%s %s' % (self.__time.replace(tzinfo = None).isoformat(' '),
                          self.__time.tzinfo)
    @classmethod
    def maybe(cls, datestring):
        """Return a new object initialized with the argument if it contains a
        value (otherwise, just return the argument)."""
        if datestring in [None, NoValue]:
            return datestring
        return cls(datestring)

class Person(Immutable, Repr):
    """Represents an author or committer in a git commit object. Contains
    name, email and timestamp."""
    def __init__(self, name = NoValue, email = NoValue,
                 date = NoValue, defaults = NoValue):
        d = make_defaults(defaults)
        self.__name = d(name, 'name')
        self.__email = d(email, 'email')
        self.__date = d(date, 'date')
        assert isinstance(self.__date, Date) or self.__date in [None, NoValue]
    name = property(lambda self: self.__name)
    email = property(lambda self: self.__email)
    name_email = property(lambda self: '%s <%s>' % (self.name, self.email))
    date = property(lambda self: self.__date)
    def set_name(self, name):
        return type(self)(name = name, defaults = self)
    def set_email(self, email):
        return type(self)(email = email, defaults = self)
    def set_date(self, date):
        return type(self)(date = date, defaults = self)
    def __str__(self):
        return '%s %s' % (self.name_email, self.date)
    @classmethod
    def parse(cls, s):
        m = re.match(r'^([^<]*)<([^>]*)>\s+(\d+\s+[+-]\d{4})$', s)
        assert m
        name = m.group(1).strip()
        email = m.group(2)
        date = Date(m.group(3))
        return cls(name, email, date)
    @classmethod
    def user(cls):
        if not hasattr(cls, '__user'):
            cls.__user = cls(name = config.get('user.name'),
                             email = config.get('user.email'))
        return cls.__user
    @classmethod
    def author(cls):
        if not hasattr(cls, '__author'):
            cls.__author = cls(
                name = os.environ.get('GIT_AUTHOR_NAME', NoValue),
                email = os.environ.get('GIT_AUTHOR_EMAIL', NoValue),
                date = Date.maybe(os.environ.get('GIT_AUTHOR_DATE', NoValue)),
                defaults = cls.user())
        return cls.__author
    @classmethod
    def committer(cls):
        if not hasattr(cls, '__committer'):
            cls.__committer = cls(
                name = os.environ.get('GIT_COMMITTER_NAME', NoValue),
                email = os.environ.get('GIT_COMMITTER_EMAIL', NoValue),
                date = Date.maybe(
                    os.environ.get('GIT_COMMITTER_DATE', NoValue)),
                defaults = cls.user())
        return cls.__committer

class GitObject(Immutable, Repr):
    """Base class for all git objects. One git object is represented by at
    most one C{GitObject}, which makes it possible to compare them
    using normal Python object comparison; it also ensures we don't
    waste more memory than necessary."""

class BlobData(Immutable, Repr):
    """Represents the data contents of a git blob object."""
    def __init__(self, string):
        self.__string = str(string)
    str = property(lambda self: self.__string)
    def commit(self, repository):
        """Commit the blob.
        @return: The committed blob
        @rtype: L{Blob}"""
        sha1 = repository.run(['git', 'hash-object', '-w', '--stdin']
                              ).raw_input(self.str).output_one_line()
        return repository.get_blob(sha1)

class Blob(GitObject):
    """Represents a git blob object. All the actual data contents of the
    blob object is stored in the L{data} member, which is a
    L{BlobData} object."""
    typename = 'blob'
    default_perm = '100644'
    def __init__(self, repository, sha1):
        self.__repository = repository
        self.__sha1 = sha1
    sha1 = property(lambda self: self.__sha1)
    def __str__(self):
        return 'Blob<%s>' % self.sha1
    @property
    def data(self):
        return BlobData(self.__repository.cat_object(self.sha1))

class ImmutableDict(dict):
    """A dictionary that cannot be modified once it's been created."""
    def error(*args, **kwargs):
        raise TypeError('Cannot modify immutable dict')
    __delitem__ = error
    __setitem__ = error
    clear = error
    pop = error
    popitem = error
    setdefault = error
    update = error

class TreeData(Immutable, Repr):
    """Represents the data contents of a git tree object."""
    @staticmethod
    def __x(po):
        if isinstance(po, GitObject):
            perm, object = po.default_perm, po
        else:
            perm, object = po
        return perm, object
    def __init__(self, entries):
        """Create a new L{TreeData} object from the given mapping from names
        (strings) to either (I{permission}, I{object}) tuples or just
        objects."""
        self.__entries = ImmutableDict((name, self.__x(po))
                                       for (name, po) in entries.iteritems())
    entries = property(lambda self: self.__entries)
    """Map from name to (I{permission}, I{object}) tuple."""
    def set_entry(self, name, po):
        """Create a new L{TreeData} object identical to this one, except that
        it maps C{name} to C{po}.

        @param name: Name of the changed mapping
        @type name: C{str}
        @param po: Value of the changed mapping
        @type po: L{Blob} or L{Tree} or (C{str}, L{Blob} or L{Tree})
        @return: The new L{TreeData} object
        @rtype: L{TreeData}"""
        e = dict(self.entries)
        e[name] = self.__x(po)
        return type(self)(e)
    def del_entry(self, name):
        """Create a new L{TreeData} object identical to this one, except that
        it doesn't map C{name} to anything.

        @param name: Name of the deleted mapping
        @type name: C{str}
        @return: The new L{TreeData} object
        @rtype: L{TreeData}"""
        e = dict(self.entries)
        del e[name]
        return type(self)(e)
    def commit(self, repository):
        """Commit the tree.
        @return: The committed tree
        @rtype: L{Tree}"""
        listing = ''.join(
            '%s %s %s\t%s\0' % (mode, obj.typename, obj.sha1, name)
            for (name, (mode, obj)) in self.entries.iteritems())
        sha1 = repository.run(['git', 'mktree', '-z']
                              ).raw_input(listing).output_one_line()
        return repository.get_tree(sha1)
    @classmethod
    def parse(cls, repository, s):
        """Parse a raw git tree description.

        @return: A new L{TreeData} object
        @rtype: L{TreeData}"""
        entries = {}
        for line in s.split('\0')[:-1]:
            m = re.match(r'^([0-7]{6}) ([a-z]+) ([0-9a-f]{40})\t(.*)$', line)
            assert m
            perm, type, sha1, name = m.groups()
            entries[name] = (perm, repository.get_object(type, sha1))
        return cls(entries)

class Tree(GitObject):
    """Represents a git tree object. All the actual data contents of the
    tree object is stored in the L{data} member, which is a
    L{TreeData} object."""
    typename = 'tree'
    default_perm = '040000'
    def __init__(self, repository, sha1):
        self.__sha1 = sha1
        self.__repository = repository
        self.__data = None
    sha1 = property(lambda self: self.__sha1)
    @property
    def data(self):
        if self.__data == None:
            self.__data = TreeData.parse(
                self.__repository,
                self.__repository.run(['git', 'ls-tree', '-z', self.sha1]
                                      ).raw_output())
        return self.__data
    def __str__(self):
        return 'Tree<sha1: %s>' % self.sha1

class CommitData(Immutable, Repr):
    """Represents the data contents of a git commit object."""
    def __init__(self, tree = NoValue, parents = NoValue, author = NoValue,
                 committer = NoValue, message = NoValue, defaults = NoValue):
        d = make_defaults(defaults)
        self.__tree = d(tree, 'tree')
        self.__parents = d(parents, 'parents')
        self.__author = d(author, 'author', Person.author)
        self.__committer = d(committer, 'committer', Person.committer)
        self.__message = d(message, 'message')
    tree = property(lambda self: self.__tree)
    parents = property(lambda self: self.__parents)
    @property
    def parent(self):
        assert len(self.__parents) == 1
        return self.__parents[0]
    author = property(lambda self: self.__author)
    committer = property(lambda self: self.__committer)
    message = property(lambda self: self.__message)
    def set_tree(self, tree):
        return type(self)(tree = tree, defaults = self)
    def set_parents(self, parents):
        return type(self)(parents = parents, defaults = self)
    def add_parent(self, parent):
        return type(self)(parents = list(self.parents or []) + [parent],
                          defaults = self)
    def set_parent(self, parent):
        return self.set_parents([parent])
    def set_author(self, author):
        return type(self)(author = author, defaults = self)
    def set_committer(self, committer):
        return type(self)(committer = committer, defaults = self)
    def set_message(self, message):
        return type(self)(message = message, defaults = self)
    def is_nochange(self):
        return len(self.parents) == 1 and self.tree == self.parent.data.tree
    def __str__(self):
        if self.tree == None:
            tree = None
        else:
            tree = self.tree.sha1
        if self.parents == None:
            parents = None
        else:
            parents = [p.sha1 for p in self.parents]
        return ('CommitData<tree: %s, parents: %s, author: %s,'
                ' committer: %s, message: "%s">'
                ) % (tree, parents, self.author, self.committer, self.message)
    def commit(self, repository):
        """Commit the commit.
        @return: The committed commit
        @rtype: L{Commit}"""
        c = ['git', 'commit-tree', self.tree.sha1]
        for p in self.parents:
            c.append('-p')
            c.append(p.sha1)
        env = {}
        for p, v1 in ((self.author, 'AUTHOR'),
                       (self.committer, 'COMMITTER')):
            if p != None:
                for attr, v2 in (('name', 'NAME'), ('email', 'EMAIL'),
                                 ('date', 'DATE')):
                    if getattr(p, attr) != None:
                        env['GIT_%s_%s' % (v1, v2)] = str(getattr(p, attr))
        sha1 = repository.run(c, env = env).raw_input(self.message
                                                      ).output_one_line()
        return repository.get_commit(sha1)
    @classmethod
    def parse(cls, repository, s):
        """Parse a raw git commit description.
        @return: A new L{CommitData} object
        @rtype: L{CommitData}"""
        cd = cls(parents = [])
        lines = []
        raw_lines = s.split('\n')
        # Collapse multi-line header lines
        for i, line in enumerate(raw_lines):
            if not line:
                cd = cd.set_message('\n'.join(raw_lines[i+1:]))
                break
            if line.startswith(' '):
                # continuation line
                lines[-1] += '\n' + line[1:]
            else:
                lines.append(line)
        for line in lines:
            if ' ' in line:
                key, value = line.split(' ', 1)
                if key == 'tree':
                    cd = cd.set_tree(repository.get_tree(value))
                elif key == 'parent':
                    cd = cd.add_parent(repository.get_commit(value))
                elif key == 'author':
                    cd = cd.set_author(Person.parse(value))
                elif key == 'committer':
                    cd = cd.set_committer(Person.parse(value))
        return cd


class Commit(GitObject):
    """Represents a git commit object. All the actual data contents of the
    commit object is stored in the L{data} member, which is a
    L{CommitData} object."""
    typename = 'commit'
    def __init__(self, repository, sha1):
        self.__sha1 = sha1
        self.__repository = repository
        self.__data = None
    sha1 = property(lambda self: self.__sha1)
    @property
    def data(self):
        if self.__data == None:
            self.__data = CommitData.parse(
                self.__repository,
                self.__repository.cat_object(self.sha1))
        return self.__data
    def __str__(self):
        return 'Commit<sha1: %s, data: %s>' % (self.sha1, self.__data)

class Refs(object):
    """Accessor for the refs stored in a git repository. Will
    transparently cache the values of all refs."""
    def __init__(self, repository):
        self.__repository = repository
        self.__refs = None
    def __cache_refs(self):
        """(Re-)Build the cache of all refs in the repository."""
        self.__refs = {}
        runner = self.__repository.run(['git', 'show-ref'])
        try:
            lines = runner.output_lines()
        except run.RunException:
            # as this happens both in non-git trees and empty git
            # trees, we silently ignore this error
            return
        for line in lines:
            m = re.match(r'^([0-9a-f]{40})\s+(\S+)$', line)
            sha1, ref = m.groups()
            self.__refs[ref] = sha1
    def get(self, ref):
        """Get the Commit the given ref points to. Throws KeyError if ref
        doesn't exist."""
        if self.__refs == None:
            self.__cache_refs()
        return self.__repository.get_commit(self.__refs[ref])
    def exists(self, ref):
        """Check if the given ref exists."""
        try:
            self.get(ref)
        except KeyError:
            return False
        else:
            return True
    def set(self, ref, commit, msg):
        """Write the sha1 of the given Commit to the ref. The ref may or may
        not already exist."""
        if self.__refs == None:
            self.__cache_refs()
        old_sha1 = self.__refs.get(ref, '0'*40)
        new_sha1 = commit.sha1
        if old_sha1 != new_sha1:
            self.__repository.run(['git', 'update-ref', '-m', msg,
                                   ref, new_sha1, old_sha1]).no_output()
            self.__refs[ref] = new_sha1
    def delete(self, ref):
        """Delete the given ref. Throws KeyError if ref doesn't exist."""
        if self.__refs == None:
            self.__cache_refs()
        self.__repository.run(['git', 'update-ref',
                               '-d', ref, self.__refs[ref]]).no_output()
        del self.__refs[ref]

class ObjectCache(object):
    """Cache for Python objects, for making sure that we create only one
    Python object per git object. This reduces memory consumption and
    makes object comparison very cheap."""
    def __init__(self, create):
        self.__objects = {}
        self.__create = create
    def __getitem__(self, name):
        if not name in self.__objects:
            self.__objects[name] = self.__create(name)
        return self.__objects[name]
    def __contains__(self, name):
        return name in self.__objects
    def __setitem__(self, name, val):
        assert not name in self.__objects
        self.__objects[name] = val

class RunWithEnv(object):
    def run(self, args, env = {}):
        """Run the given command with an environment given by self.env.

        @type args: list of strings
        @param args: Command and argument vector
        @type env: dict
        @param env: Extra environment"""
        return run.Run(*args).env(utils.add_dict(self.env, env))

class RunWithEnvCwd(RunWithEnv):
    def run(self, args, env = {}):
        """Run the given command with an environment given by self.env, and
        current working directory given by self.cwd.

        @type args: list of strings
        @param args: Command and argument vector
        @type env: dict
        @param env: Extra environment"""
        return RunWithEnv.run(self, args, env).cwd(self.cwd)
    def run_in_cwd(self, args):
        """Run the given command with an environment given by self.env and
        self.env_in_cwd, without changing the current working
        directory.

        @type args: list of strings
        @param args: Command and argument vector"""
        return RunWithEnv.run(self, args, self.env_in_cwd)

class CatFileProcess(object):
    def __init__(self, repo):
        self.__repo = repo
        self.__proc = None
        atexit.register(self.__shutdown)
    def __get_process(self):
        if not self.__proc:
            self.__proc = self.__repo.run(['git', 'cat-file', '--batch']
                                          ).run_background()
        return self.__proc
    def __shutdown(self):
        p = self.__proc
        if p:
            p.stdin.close()
            os.kill(p.pid(), signal.SIGTERM)
            p.wait()
    def cat_file(self, sha1):
        p = self.__get_process()
        p.stdin.write('%s\n' % sha1)
        p.stdin.flush()

        # Read until we have the entire status line.
        s = ''
        while not '\n' in s:
            s += os.read(p.stdout.fileno(), 4096)
        h, b = s.split('\n', 1)
        if h == '%s missing' % sha1:
            raise SomeException()
        hash, type, length = h.split()
        assert hash == sha1
        length = int(length)

        # Read until we have the entire object plus the trailing
        # newline.
        while len(b) < length + 1:
            b += os.read(p.stdout.fileno(), 4096)
        return type, b[:-1]

class DiffTreeProcesses(object):
    def __init__(self, repo):
        self.__repo = repo
        self.__procs = {}
        atexit.register(self.__shutdown)
    def __get_process(self, args):
        args = tuple(args)
        if not args in self.__procs:
            self.__procs[args] = self.__repo.run(
                ['git', 'diff-tree', '--stdin'] + list(args)).run_background()
        return self.__procs[args]
    def __shutdown(self):
        for p in self.__procs.values():
            os.kill(p.pid(), signal.SIGTERM)
            p.wait()
    def diff_trees(self, args, sha1a, sha1b):
        p = self.__get_process(args)
        query = '%s %s\n' % (sha1a, sha1b)
        end = 'EOF\n' # arbitrary string that's not a 40-digit hex number
        p.stdin.write(query + end)
        p.stdin.flush()
        s = ''
        while not (s.endswith('\n' + end) or s.endswith('\0' + end)):
            s += os.read(p.stdout.fileno(), 4096)
        assert s.startswith(query)
        assert s.endswith(end)
        return s[len(query):-len(end)]

class Repository(RunWithEnv):
    """Represents a git repository."""
    def __init__(self, directory):
        self.__git_dir = directory
        self.__refs = Refs(self)
        self.__blobs = ObjectCache(lambda sha1: Blob(self, sha1))
        self.__trees = ObjectCache(lambda sha1: Tree(self, sha1))
        self.__commits = ObjectCache(lambda sha1: Commit(self, sha1))
        self.__default_index = None
        self.__default_worktree = None
        self.__default_iw = None
        self.__catfile = CatFileProcess(self)
        self.__difftree = DiffTreeProcesses(self)
    env = property(lambda self: { 'GIT_DIR': self.__git_dir })
    @classmethod
    def default(cls):
        """Return the default repository."""
        try:
            return cls(run.Run('git', 'rev-parse', '--git-dir'
                               ).output_one_line())
        except run.RunException:
            raise RepositoryException('Cannot find git repository')
    @property
    def current_branch_name(self):
        """Return the name of the current branch."""
        return utils.strip_prefix('refs/heads/', self.head_ref)
    @property
    def default_index(self):
        """An L{Index} object representing the default index file for the
        repository."""
        if self.__default_index == None:
            self.__default_index = Index(
                self, (os.environ.get('GIT_INDEX_FILE', None)
                       or os.path.join(self.__git_dir, 'index')))
        return self.__default_index
    def temp_index(self):
        """Return an L{Index} object representing a new temporary index file
        for the repository."""
        return Index(self, self.__git_dir)
    @property
    def default_worktree(self):
        """A L{Worktree} object representing the default work tree."""
        if self.__default_worktree == None:
            path = os.environ.get('GIT_WORK_TREE', None)
            if not path:
                o = run.Run('git', 'rev-parse', '--show-cdup').output_lines()
                o = o or ['.']
                assert len(o) == 1
                path = o[0]
            self.__default_worktree = Worktree(path)
        return self.__default_worktree
    @property
    def default_iw(self):
        """An L{IndexAndWorktree} object representing the default index and
        work tree for this repository."""
        if self.__default_iw == None:
            self.__default_iw = IndexAndWorktree(self.default_index,
                                                 self.default_worktree)
        return self.__default_iw
    directory = property(lambda self: self.__git_dir)
    refs = property(lambda self: self.__refs)
    def cat_object(self, sha1):
        return self.__catfile.cat_file(sha1)[1]
    def rev_parse(self, rev, discard_stderr = False, object_type = 'commit'):
        assert object_type in ('commit', 'tree', 'blob')
        getter = getattr(self, 'get_' + object_type)
        try:
            return getter(self.run(
                    ['git', 'rev-parse', '%s^{%s}' % (rev, object_type)]
                    ).discard_stderr(discard_stderr).output_one_line())
        except run.RunException:
            raise RepositoryException('%s: No such %s' % (rev, object_type))
    def get_blob(self, sha1):
        return self.__blobs[sha1]
    def get_tree(self, sha1):
        return self.__trees[sha1]
    def get_commit(self, sha1):
        return self.__commits[sha1]
    def get_object(self, type, sha1):
        return { Blob.typename: self.get_blob,
                 Tree.typename: self.get_tree,
                 Commit.typename: self.get_commit }[type](sha1)
    def commit(self, objectdata):
        return objectdata.commit(self)
    @property
    def head_ref(self):
        try:
            return self.run(['git', 'symbolic-ref', '-q', 'HEAD']
                            ).output_one_line()
        except run.RunException:
            raise DetachedHeadException()
    def set_head_ref(self, ref, msg):
        self.run(['git', 'symbolic-ref', '-m', msg, 'HEAD', ref]).no_output()
    def get_merge_bases(self, commit1, commit2):
        """Return a set of merge bases of two commits."""
        sha1_list = self.run(['git', 'merge-base', '--all',
                              commit1.sha1, commit2.sha1]).output_lines()
        return [self.get_commit(sha1) for sha1 in sha1_list]
    def describe(self, commit):
        """Use git describe --all on the given commit."""
        return self.run(['git', 'describe', '--all', commit.sha1]
                       ).discard_stderr().discard_exitcode().raw_output()
    def simple_merge(self, base, ours, theirs):
        index = self.temp_index()
        try:
            result, index_tree = index.merge(base, ours, theirs)
        finally:
            index.delete()
        return result
    def apply(self, tree, patch_text, quiet):
        """Given a L{Tree} and a patch, will either return the new L{Tree}
        that results when the patch is applied, or None if the patch
        couldn't be applied."""
        assert isinstance(tree, Tree)
        if not patch_text:
            return tree
        index = self.temp_index()
        try:
            index.read_tree(tree)
            try:
                index.apply(patch_text, quiet)
                return index.write_tree()
            except MergeException:
                return None
        finally:
            index.delete()
    def diff_tree(self, t1, t2, diff_opts, binary = True):
        """Given two L{Tree}s C{t1} and C{t2}, return the patch that takes
        C{t1} to C{t2}.

        @type diff_opts: list of strings
        @param diff_opts: Extra diff options
        @rtype: String
        @return: Patch text"""
        assert isinstance(t1, Tree)
        assert isinstance(t2, Tree)
        diff_opts = list(diff_opts)
        if binary and not '--binary' in diff_opts:
            diff_opts.append('--binary')
        return self.__difftree.diff_trees(['-p'] + diff_opts,
                                          t1.sha1, t2.sha1)
    def diff_tree_files(self, t1, t2):
        """Given two L{Tree}s C{t1} and C{t2}, iterate over all files for
        which they differ. For each file, yield a tuple with the old
        file mode, the new file mode, the old blob, the new blob, the
        status, the old filename, and the new filename. Except in case
        of a copy or a rename, the old and new filenames are
        identical."""
        assert isinstance(t1, Tree)
        assert isinstance(t2, Tree)
        i = iter(self.__difftree.diff_trees(
                ['-r', '-z'], t1.sha1, t2.sha1).split('\0'))
        try:
            while True:
                x = i.next()
                if not x:
                    continue
                omode, nmode, osha1, nsha1, status = x[1:].split(' ')
                fn1 = i.next()
                if status[0] in ['C', 'R']:
                    fn2 = i.next()
                else:
                    fn2 = fn1
                yield (omode, nmode, self.get_blob(osha1),
                       self.get_blob(nsha1), status, fn1, fn2)
        except StopIteration:
            pass

class MergeException(exception.StgException):
    """Exception raised when a merge fails for some reason."""

class MergeConflictException(MergeException):
    """Exception raised when a merge fails due to conflicts."""
    def __init__(self, conflicts):
        MergeException.__init__(self)
        self.conflicts = conflicts

class Index(RunWithEnv):
    """Represents a git index file."""
    def __init__(self, repository, filename):
        self.__repository = repository
        if os.path.isdir(filename):
            # Create a temp index in the given directory.
            self.__filename = os.path.join(
                filename, 'index.temp-%d-%x' % (os.getpid(), id(self)))
            self.delete()
        else:
            self.__filename = filename
    env = property(lambda self: utils.add_dict(
            self.__repository.env, { 'GIT_INDEX_FILE': self.__filename }))
    def read_tree(self, tree):
        self.run(['git', 'read-tree', tree.sha1]).no_output()
    def write_tree(self):
        """Write the index contents to the repository.
        @return: The resulting L{Tree}
        @rtype: L{Tree}"""
        try:
            return self.__repository.get_tree(
                self.run(['git', 'write-tree']).discard_stderr(
                    ).output_one_line())
        except run.RunException:
            raise MergeException('Conflicting merge')
    def is_clean(self, tree):
        """Check whether the index is clean relative to the given treeish."""
        try:
            self.run(['git', 'diff-index', '--quiet', '--cached', tree.sha1]
                    ).discard_output()
        except run.RunException:
            return False
        else:
            return True
    def apply(self, patch_text, quiet):
        """In-index patch application, no worktree involved."""
        try:
            r = self.run(['git', 'apply', '--cached']).raw_input(patch_text)
            if quiet:
                r = r.discard_stderr()
            r.no_output()
        except run.RunException:
            raise MergeException('Patch does not apply cleanly')
    def apply_treediff(self, tree1, tree2, quiet):
        """Apply the diff from C{tree1} to C{tree2} to the index."""
        # Passing --full-index here is necessary to support binary
        # files. It is also sufficient, since the repository already
        # contains all involved objects; in other words, we don't have
        # to use --binary.
        self.apply(self.__repository.diff_tree(tree1, tree2, ['--full-index']),
                   quiet)
    def merge(self, base, ours, theirs, current = None):
        """Use the index (and only the index) to do a 3-way merge of the
        L{Tree}s C{base}, C{ours} and C{theirs}. The merge will either
        succeed (in which case the first half of the return value is
        the resulting tree) or fail cleanly (in which case the first
        half of the return value is C{None}).

        If C{current} is given (and not C{None}), it is assumed to be
        the L{Tree} currently stored in the index; this information is
        used to avoid having to read the right tree (either of C{ours}
        and C{theirs}) into the index if it's already there. The
        second half of the return value is the tree now stored in the
        index, or C{None} if unknown. If the merge succeeded, this is
        often the merge result."""
        assert isinstance(base, Tree)
        assert isinstance(ours, Tree)
        assert isinstance(theirs, Tree)
        assert current == None or isinstance(current, Tree)

        # Take care of the really trivial cases.
        if base == ours:
            return (theirs, current)
        if base == theirs:
            return (ours, current)
        if ours == theirs:
            return (ours, current)

        if current == theirs:
            # Swap the trees. It doesn't matter since merging is
            # symmetric, and will allow us to avoid the read_tree()
            # call below.
            ours, theirs = theirs, ours
        if current != ours:
            self.read_tree(ours)
        try:
            self.apply_treediff(base, theirs, quiet = True)
            result = self.write_tree()
            return (result, result)
        except MergeException:
            return (None, ours)
    def delete(self):
        if os.path.isfile(self.__filename):
            os.remove(self.__filename)
    def conflicts(self):
        """The set of conflicting paths."""
        paths = set()
        for line in self.run(['git', 'ls-files', '-z', '--unmerged']
                             ).raw_output().split('\0')[:-1]:
            stat, path = line.split('\t', 1)
            paths.add(path)
        return paths

class Worktree(object):
    """Represents a git worktree (that is, a checked-out file tree)."""
    def __init__(self, directory):
        self.__directory = directory
    env = property(lambda self: { 'GIT_WORK_TREE': '.' })
    env_in_cwd = property(lambda self: { 'GIT_WORK_TREE': self.directory })
    directory = property(lambda self: self.__directory)

class CheckoutException(exception.StgException):
    """Exception raised when a checkout fails."""

class IndexAndWorktree(RunWithEnvCwd):
    """Represents a git index and a worktree. Anything that an index or
    worktree can do on their own are handled by the L{Index} and
    L{Worktree} classes; this class concerns itself with the
    operations that require both."""
    def __init__(self, index, worktree):
        self.__index = index
        self.__worktree = worktree
    index = property(lambda self: self.__index)
    env = property(lambda self: utils.add_dict(self.__index.env,
                                               self.__worktree.env))
    env_in_cwd = property(lambda self: self.__worktree.env_in_cwd)
    cwd = property(lambda self: self.__worktree.directory)
    def checkout_hard(self, tree):
        assert isinstance(tree, Tree)
        self.run(['git', 'read-tree', '--reset', '-u', tree.sha1]
                 ).discard_output()
    def checkout(self, old_tree, new_tree):
        # TODO: Optionally do a 3-way instead of doing nothing when we
        # have a problem. Or maybe we should stash changes in a patch?
        assert isinstance(old_tree, Tree)
        assert isinstance(new_tree, Tree)
        try:
            self.run(['git', 'read-tree', '-u', '-m',
                      '--exclude-per-directory=.gitignore',
                      old_tree.sha1, new_tree.sha1]
                     ).discard_output()
        except run.RunException:
            raise CheckoutException('Index/workdir dirty')
    def merge(self, base, ours, theirs, interactive = False):
        assert isinstance(base, Tree)
        assert isinstance(ours, Tree)
        assert isinstance(theirs, Tree)
        try:
            r = self.run(['git', 'merge-recursive', base.sha1, '--', ours.sha1,
                          theirs.sha1],
                         env = { 'GITHEAD_%s' % base.sha1: 'ancestor',
                                 'GITHEAD_%s' % ours.sha1: 'current',
                                 'GITHEAD_%s' % theirs.sha1: 'patched'})
            r.returns([0, 1])
            output = r.output_lines()
            if r.exitcode:
                # There were conflicts
                if interactive:
                    self.mergetool()
                else:
                    conflicts = [l for l in output if l.startswith('CONFLICT')]
                    raise MergeConflictException(conflicts)
        except run.RunException, e:
            raise MergeException('Index/worktree dirty')
    def mergetool(self, files = ()):
        """Invoke 'git mergetool' on the current IndexAndWorktree to resolve
        any outstanding conflicts. If 'not files', all the files in an
        unmerged state will be processed."""
        self.run(['git', 'mergetool'] + list(files)).returns([0, 1]).run()
        # check for unmerged entries (prepend 'CONFLICT ' for consistency with
        # merge())
        conflicts = ['CONFLICT ' + f for f in self.index.conflicts()]
        if conflicts:
            raise MergeConflictException(conflicts)
    def changed_files(self, tree, pathlimits = []):
        """Return the set of files in the worktree that have changed with
        respect to C{tree}. The listing is optionally restricted to
        those files that match any of the path limiters given.

        The path limiters are relative to the current working
        directory; the returned file names are relative to the
        repository root."""
        assert isinstance(tree, Tree)
        return set(self.run_in_cwd(
                ['git', 'diff-index', tree.sha1, '--name-only', '-z', '--']
                + list(pathlimits)).raw_output().split('\0')[:-1])
    def update_index(self, paths):
        """Update the index with files from the worktree. C{paths} is an
        iterable of paths relative to the root of the repository."""
        cmd = ['git', 'update-index', '--remove']
        self.run(cmd + ['-z', '--stdin']
                 ).input_nulterm(paths).discard_output()
    def worktree_clean(self):
        """Check whether the worktree is clean relative to index."""
        try:
            self.run(['git', 'update-index', '--refresh']).discard_output()
        except run.RunException:
            return False
        else:
            return True

class Branch(object):
    """Represents a Git branch."""
    def __init__(self, repository, name):
        self.__repository = repository
        self.__name = name
        try:
            self.head
        except KeyError:
            raise BranchException('%s: no such branch' % name)

    name = property(lambda self: self.__name)
    repository = property(lambda self: self.__repository)

    def __ref(self):
        return 'refs/heads/%s' % self.__name
    @property
    def head(self):
        return self.__repository.refs.get(self.__ref())
    def set_head(self, commit, msg):
        self.__repository.refs.set(self.__ref(), commit, msg)

    def set_parent_remote(self, name):
        value = config.set('branch.%s.remote' % self.__name, name)
    def set_parent_branch(self, name):
        if config.get('branch.%s.remote' % self.__name):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.__name, name)

    @classmethod
    def create(cls, repository, name, create_at = None):
        """Create a new Git branch and return the corresponding
        L{Branch} object."""
        try:
            branch = cls(repository, name)
        except BranchException:
            branch = None
        if branch:
            raise BranchException('%s: branch already exists' % name)

        cmd = ['git', 'branch']
        if create_at:
            cmd.append(create_at.sha1)
        repository.run(['git', 'branch', create_at.sha1]).discard_output()

        return cls(repository, name)

def diffstat(diff):
    """Return the diffstat of the supplied diff."""
    return run.Run('git', 'apply', '--stat', '--summary'
                   ).raw_input(diff).raw_output()

def clone(remote, local):
    """Clone a remote repository using 'git clone'."""
    run.Run('git', 'clone', remote, local).run()
