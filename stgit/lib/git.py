import os, os.path, re
from stgit import exception, run, utils

class RepositoryException(exception.StgException):
    pass

class DetachedHeadException(RepositoryException):
    def __init__(self):
        RepositoryException.__init__(self, 'Not on any branch')

class Repr(object):
    def __repr__(self):
        return str(self)

class NoValue(object):
    pass

def make_defaults(defaults):
    def d(val, attr):
        if val != NoValue:
            return val
        elif defaults != NoValue:
            return getattr(defaults, attr)
        else:
            return None
    return d

class Person(Repr):
    """Immutable."""
    def __init__(self, name = NoValue, email = NoValue,
                 date = NoValue, defaults = NoValue):
        d = make_defaults(defaults)
        self.__name = d(name, 'name')
        self.__email = d(email, 'email')
        self.__date = d(date, 'date')
    name = property(lambda self: self.__name)
    email = property(lambda self: self.__email)
    date = property(lambda self: self.__date)
    def set_name(self, name):
        return type(self)(name = name, defaults = self)
    def set_email(self, email):
        return type(self)(email = email, defaults = self)
    def set_date(self, date):
        return type(self)(date = date, defaults = self)
    def __str__(self):
        return '%s <%s> %s' % (self.name, self.email, self.date)
    @classmethod
    def parse(cls, s):
        m = re.match(r'^([^<]*)<([^>]*)>\s+(\d+\s+[+-]\d{4})$', s)
        assert m
        name = m.group(1).strip()
        email = m.group(2)
        date = m.group(3)
        return cls(name, email, date)

class Tree(Repr):
    """Immutable."""
    def __init__(self, sha1):
        self.__sha1 = sha1
    sha1 = property(lambda self: self.__sha1)
    def __str__(self):
        return 'Tree<%s>' % self.sha1

class Commitdata(Repr):
    """Immutable."""
    def __init__(self, tree = NoValue, parents = NoValue, author = NoValue,
                 committer = NoValue, message = NoValue, defaults = NoValue):
        d = make_defaults(defaults)
        self.__tree = d(tree, 'tree')
        self.__parents = d(parents, 'parents')
        self.__author = d(author, 'author')
        self.__committer = d(committer, 'committer')
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
    def __str__(self):
        if self.tree == None:
            tree = None
        else:
            tree = self.tree.sha1
        if self.parents == None:
            parents = None
        else:
            parents = [p.sha1 for p in self.parents]
        return ('Commitdata<tree: %s, parents: %s, author: %s,'
                ' committer: %s, message: "%s">'
                ) % (tree, parents, self.author, self.committer, self.message)
    @classmethod
    def parse(cls, repository, s):
        cd = cls()
        lines = list(s.splitlines(True))
        for i in xrange(len(lines)):
            line = lines[i].strip()
            if not line:
                return cd.set_message(''.join(lines[i+1:]))
            key, value = line.split(None, 1)
            if key == 'tree':
                cd = cd.set_tree(repository.get_tree(value))
            elif key == 'parent':
                cd = cd.add_parent(repository.get_commit(value))
            elif key == 'author':
                cd = cd.set_author(Person.parse(value))
            elif key == 'committer':
                cd = cd.set_committer(Person.parse(value))
            else:
                assert False
        assert False

class Commit(Repr):
    """Immutable."""
    def __init__(self, repository, sha1):
        self.__sha1 = sha1
        self.__repository = repository
        self.__data = None
    sha1 = property(lambda self: self.__sha1)
    @property
    def data(self):
        if self.__data == None:
            self.__data = Commitdata.parse(
                self.__repository,
                self.__repository.cat_object(self.sha1))
        return self.__data
    def __str__(self):
        return 'Commit<sha1: %s, data: %s>' % (self.sha1, self.__data)

class Refs(object):
    def __init__(self, repository):
        self.__repository = repository
        self.__refs = None
    def __cache_refs(self):
        self.__refs = {}
        for line in self.__repository.run(['git', 'show-ref']).output_lines():
            m = re.match(r'^([0-9a-f]{40})\s+(\S+)$', line)
            sha1, ref = m.groups()
            self.__refs[ref] = sha1
    def get(self, ref):
        """Throws KeyError if ref doesn't exist."""
        if self.__refs == None:
            self.__cache_refs()
        return self.__repository.get_commit(self.__refs[ref])
    def set(self, ref, commit, msg):
        if self.__refs == None:
            self.__cache_refs()
        old_sha1 = self.__refs.get(ref, '0'*40)
        new_sha1 = commit.sha1
        if old_sha1 != new_sha1:
            self.__repository.run(['git', 'update-ref', '-m', msg,
                                   ref, new_sha1, old_sha1]).no_output()
            self.__refs[ref] = new_sha1
    def delete(self, ref):
        if self.__refs == None:
            self.__cache_refs()
        self.__repository.run(['git', 'update-ref',
                               '-d', ref, self.__refs[ref]]).no_output()
        del self.__refs[ref]

class ObjectCache(object):
    """Cache for Python objects, for making sure that we create only one
    Python object per git object."""
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
        return run.Run(*args).env(utils.add_dict(self.env, env))

class Repository(RunWithEnv):
    def __init__(self, directory):
        self.__git_dir = directory
        self.__refs = Refs(self)
        self.__trees = ObjectCache(lambda sha1: Tree(sha1))
        self.__commits = ObjectCache(lambda sha1: Commit(self, sha1))
    env = property(lambda self: { 'GIT_DIR': self.__git_dir })
    @classmethod
    def default(cls):
        """Return the default repository."""
        try:
            return cls(run.Run('git', 'rev-parse', '--git-dir'
                               ).output_one_line())
        except run.RunException:
            raise RepositoryException('Cannot find git repository')
    directory = property(lambda self: self.__git_dir)
    refs = property(lambda self: self.__refs)
    def cat_object(self, sha1):
        return self.run(['git', 'cat-file', '-p', sha1]).raw_output()
    def rev_parse(self, rev):
        try:
            return self.get_commit(self.run(
                    ['git', 'rev-parse', '%s^{commit}' % rev]
                    ).output_one_line())
        except run.RunException:
            raise RepositoryException('%s: No such revision' % rev)
    def get_tree(self, sha1):
        return self.__trees[sha1]
    def get_commit(self, sha1):
        return self.__commits[sha1]
    def commit(self, commitdata):
        c = ['git', 'commit-tree', commitdata.tree.sha1]
        for p in commitdata.parents:
            c.append('-p')
            c.append(p.sha1)
        env = {}
        for p, v1 in ((commitdata.author, 'AUTHOR'),
                       (commitdata.committer, 'COMMITTER')):
            if p != None:
                for attr, v2 in (('name', 'NAME'), ('email', 'EMAIL'),
                                 ('date', 'DATE')):
                    if getattr(p, attr) != None:
                        env['GIT_%s_%s' % (v1, v2)] = getattr(p, attr)
        sha1 = self.run(c, env = env).raw_input(commitdata.message
                                                ).output_one_line()
        return self.get_commit(sha1)
    @property
    def head(self):
        try:
            return self.run(['git', 'symbolic-ref', '-q', 'HEAD']
                            ).output_one_line()
        except run.RunException:
            raise DetachedHeadException()
    def set_head(self, ref, msg):
        self.run(['git', 'symbolic-ref', '-m', msg, 'HEAD', ref]).no_output()
