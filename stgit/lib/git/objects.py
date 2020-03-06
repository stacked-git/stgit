import re

from stgit.config import config

from .base import Immutable
from .person import Person


class GitObject(Immutable):
    """Base class for all git objects. One git object is represented by at
    most one C{GitObject}, which makes it possible to compare them
    using normal Python object comparison; it also ensures we don't
    waste more memory than necessary."""


class BlobData(Immutable):
    """Represents the data contents of a git blob object."""

    def __init__(self, data):
        assert isinstance(data, bytes)
        self.bytes = data

    def commit(self, repository):
        """Commit the blob.
        @return: The committed blob
        @rtype: L{Blob}"""
        sha1 = (
            repository.run(['git', 'hash-object', '-w', '--stdin'])
            .encoding(None)
            .raw_input(self.bytes)
            .output_one_line()
        )
        return repository.get_blob(sha1)


class Blob(GitObject):
    """Represents a git blob object. All the actual data contents of the
    blob object is stored in the L{data} member, which is a
    L{BlobData} object."""

    typename = 'blob'
    default_perm = '100644'

    def __init__(self, repository, sha1):
        self._repository = repository
        self.sha1 = sha1

    def __repr__(self):  # pragma: no cover
        return 'Blob<%s>' % self.sha1

    @property
    def data(self):
        type_, content = self._repository.cat_object(self.sha1)
        assert type_ == 'blob', 'expected "blob", got "%s" for %s' % (type_, self.sha1)
        return BlobData(content)


class TreeData(Immutable):
    """Represents the data contents of a git tree object."""

    def __init__(self, entries):
        """Create a new L{TreeData} object from the given mapping from names
        (strings) to either (I{permission}, I{object}) tuples or just
        objects."""
        self._entries = {}
        for name, po in entries.items():
            assert '/' not in name, 'tree entry name contains slash: %s' % name
            if isinstance(po, GitObject):
                perm, obj = po.default_perm, po
            else:
                perm, obj = po
            self._entries[name] = (perm, obj)

    def __getitem__(self, key):
        return self._entries[key]

    def __iter__(self):
        for name, (perm, obj) in self._entries.items():
            yield name, (perm, obj)

    def commit(self, repository):
        """Commit the tree.
        @return: The committed tree
        @rtype: L{Tree}"""
        listing = [
            '%s %s %s\t%s' % (perm, obj.typename, obj.sha1, name)
            for name, (perm, obj) in self
        ]
        sha1 = (
            repository.run(['git', 'mktree', '-z'])
            .input_nulterm(listing)
            .output_one_line()
        )
        return repository.get_tree(sha1)

    @classmethod
    def parse(cls, repository, lines):
        """Parse a raw git tree description.

        @return: A new L{TreeData} object
        @rtype: L{TreeData}"""
        entries = {}
        for line in lines:
            m = re.match(r'^([0-7]{6}) ([a-z]+) ([0-9a-f]{40})\t(.*)$', line)
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
        self.sha1 = sha1
        self._repository = repository
        self._data = None

    @property
    def data(self):
        if self._data is None:
            self._data = TreeData.parse(
                self._repository,
                self._repository.run(['git', 'ls-tree', '-z', self.sha1]).output_lines(
                    '\0'
                ),
            )
        return self._data

    def __repr__(self):  # pragma: no cover
        return 'Tree<sha1: %s>' % self.sha1


class CommitData(Immutable):
    """Represents the data contents of a git commit object."""

    def __init__(
        self, tree, parents, message, encoding=None, author=None, committer=None
    ):
        self.tree = tree
        self.parents = parents
        self.encoding = (
            encoding if encoding is not None else config.get('i18n.commitencoding')
        )
        if isinstance(message, bytes):
            self.message = message
        else:
            self.message = message.encode(self.encoding)

        if author is None:
            self._author = Person.author()
        else:
            assert isinstance(author, (Person, bytes))
            self._author = author

        if committer is None:
            self._committer = Person.committer()
        else:
            assert isinstance(committer, (Person, bytes))
            self._committer = committer

    @property
    def env(self):
        env = {}
        for p, v1 in [(self.author, 'AUTHOR'), (self.committer, 'COMMITTER')]:
            if p is not None:
                for attr, v2 in [
                    ('name', 'NAME'),
                    ('email', 'EMAIL'),
                    ('date', 'DATE'),
                ]:
                    if getattr(p, attr) is not None:
                        env['GIT_%s_%s' % (v1, v2)] = str(getattr(p, attr))
        return env

    @property
    def message_str(self):
        return self.message.decode(self.encoding)

    @property
    def parent(self):
        assert len(self.parents) == 1
        return self.parents[0]

    @property
    def author(self):
        if isinstance(self._author, bytes):
            self._author = Person.parse(self._author.decode(self.encoding))
        return self._author

    @property
    def committer(self):
        if isinstance(self._committer, bytes):
            self._committer = Person.parse(self._committer.decode(self.encoding))
        return self._committer

    def set_tree(self, tree):
        return self._replace(tree=tree)

    def set_parent(self, parent):
        return self._replace(parents=[parent])

    def set_author(self, author):
        assert isinstance(author, Person) or author is None
        return self._replace(author=author)

    def set_committer(self, committer):
        assert isinstance(committer, Person) or committer is None
        return self._replace(committer=committer)

    def set_message(self, message):
        commit_encoding = config.get('i18n.commitencoding')
        if isinstance(message, bytes):
            message.decode(commit_encoding)
        else:
            message = message.encode(commit_encoding)
        return self._replace(message=message, encoding=commit_encoding)

    def _replace(self, **kws):
        return type(self)(
            tree=kws.get('tree', self.tree),
            parents=kws.get('parents', self.parents),
            message=kws.get('message', self.message),
            encoding=kws.get('encoding', self.encoding),
            author=kws.get('author', self.author),
            committer=kws.get('committer', self.committer),
        )

    def is_nochange(self):
        return len(self.parents) == 1 and self.tree == self.parent.data.tree

    def __repr__(self):  # pragma: no cover
        return (
            'CommitData<tree: %s, parents: %s, author: %s, committer: %s, '
            'message: %s>'
        ) % (
            self.tree.sha1,
            [p.sha1 for p in self.parents],
            self._author,
            self._committer,
            self.message.split(b'\n', 1)[0],
        )

    def commit(self, repository):
        """Commit the commit.
        @return: The committed commit
        @rtype: L{Commit}"""
        c = ['git', 'commit-tree', self.tree.sha1]
        for p in self.parents:
            c.append('-p')
            c.append(p.sha1)
        sha1 = (
            repository.run(c, env=self.env)
            .encoding(None)
            .raw_input(self.message)
            .output_one_line()
        )
        return repository.get_commit(sha1)

    @classmethod
    def parse(cls, repository, content):
        """Parse a raw git commit description.
        @return: A new L{CommitData} object
        @rtype: L{CommitData}"""
        required_keys = set(['tree', 'author', 'committer'])
        parents = []
        encoding = None

        while True:
            line, content = content.split(b'\n', 1)
            if line:
                while content.startswith(b' '):
                    extended, content = content.split(b'\n', 1)
                    line += extended[1:]

                key_b, value_b = line.split(b' ', 1)
                key = key_b.decode('utf-8')
                if key == 'tree':
                    tree = repository.get_tree(value_b.decode('utf-8'))
                    required_keys.remove(key)
                elif key == 'parent':
                    parents.append(repository.get_commit(value_b.decode('utf-8')))
                elif key == 'author':
                    author = value_b
                    required_keys.remove(key)
                elif key == 'committer':
                    committer = value_b
                    required_keys.remove(key)
                elif key == 'encoding':
                    encoding = value_b.decode('utf-8')
                else:
                    # Any other keys are meant to be explicitly ignored
                    pass
            else:
                break
        assert not required_keys, 'commit data missing keys %s' % required_keys
        return cls(tree, parents, content, encoding, author, committer)


class Commit(GitObject):
    """Represents a git commit object. All the actual data contents of the
    commit object is stored in the L{data} member, which is a
    L{CommitData} object."""

    typename = 'commit'

    def __init__(self, repository, sha1):
        self.sha1 = sha1
        self._repository = repository
        self._data = None

    @property
    def data(self):
        if self._data is None:
            type_, content = self._repository.cat_object(self.sha1)
            assert type_ == 'commit', 'expected "commit", got "%s" for %s' % (
                type_,
                self.sha1,
            )
            self._data = CommitData.parse(self._repository, content)
        return self._data

    def __repr__(self):  # pragma: no cover
        return 'Commit<sha1: %s, data: %s>' % (self.sha1, self._data)
