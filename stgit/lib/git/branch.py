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
        self.__repository = repository
        self.__name = name
        try:
            self.head
        except KeyError:
            raise BranchException('%s: no such branch' % name)

    @property
    def name(self):
        return self.__name

    @property
    def repository(self):
        return self.__repository

    def __ref(self):
        return 'refs/heads/%s' % self.__name

    @property
    def head(self):
        return self.__repository.refs.get(self.__ref())

    def set_head(self, commit, msg):
        self.__repository.refs.set(self.__ref(), commit, msg)

    def set_parent_remote(self, name):
        config.set('branch.%s.remote' % self.__name, name)

    def set_parent_branch(self, name):
        if config.get('branch.%s.remote' % self.__name):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.__name, name)

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
