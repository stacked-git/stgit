# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.config import config
from stgit.exception import StgException


class BranchException(StgException):
    """Exception raised by failed L{Branch} operations."""


class Branch(object):
    """Represents a Git branch."""

    def __init__(self, repository, name):
        self.repository = repository
        self.name = name
        try:
            self.head
        except KeyError:
            raise BranchException('%s: no such branch' % name)

    @property
    def _ref(self):
        return 'refs/heads/%s' % self.name

    @property
    def _remote_key(self):
        return 'branch.%s.remote' % self.name

    @property
    def head(self):
        return self.repository.refs.get(self._ref)

    def set_head(self, commit, msg):
        self.repository.refs.set(self._ref, commit, msg)

    @property
    def parent_remote(self):
        remote = config.get(self._remote_key)
        if remote is None:
            raise BranchException(
                '%s: no parent remote; consider configuring "%s"' % (
                    self.name, self._remote_key
                )
            )
        else:
            return remote

    def set_parent_remote(self, name):
        config.set('branch.%s.remote' % self.name, name)

    def set_parent_branch(self, name):
        if config.get(self._remote_key):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.name, name)

    @classmethod
    def create(cls, repository, name, create_at=None):
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
