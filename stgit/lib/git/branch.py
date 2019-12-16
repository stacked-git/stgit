# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.config import config
from stgit.exception import StgException
from stgit.run import RunException


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
        return config.get(self._remote_key)

    def set_parent_remote(self, name):
        config.set(self._remote_key, name)

    def set_parent_branch(self, name):
        if config.get(self._remote_key):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.name, name)

    def get_description(self):
        return config.get('branch.%s.description' % self.name)

    def set_description(self, description):
        if description:
            config.set('branch.%s.description' % self.name, description)
        else:
            config.unset('branch.%s.description' % self.name)

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

        cmd = ['git', 'branch', name]
        if create_at:
            cmd.append(create_at.sha1)
        repository.run(cmd).discard_output()
        repository.refs.reset_cache()

        return cls(repository, name)

    def switch_to(self):
        try:
            self.repository.run(
                ['git', 'checkout', self.name]
            ).discard_output()
        except RunException:
            raise BranchException('%s: failed to switch to branch' % self.name)
        self.repository.refs.reset_cache()

    def delete(self):
        self.repository.run(
            ['git', 'branch', '--delete', '--force', self.name]
        ).discard_output()

    def rename(self, name):
        new_ref = 'refs/heads/%s' % name
        if self.repository.refs.exists(new_ref):
            raise BranchException('branch "%s" already exists' % name)

        # Moves the branch ref, moves the reflog, updates HEAD, and renames
        # 'branch.<name>' config section.
        self.repository.run(
            ['git', 'branch', '--move', self.name, name]
        ).discard_output()
        self.repository.refs.reset_cache()
        self.name = name
