# -*- coding: utf-8 -*-
"""A Python class hierarchy wrapping the StGit on-disk metadata."""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import os
import shutil

from stgit import utils
from stgit.compat import fsencode_utf8, text
from stgit.config import config
from stgit.exception import StackException
from stgit.lib import stackupgrade
from stgit.lib.git import CommitData, Repository
from stgit.lib.git.branch import Branch, BranchException
from stgit.lib.objcache import ObjectCache


class Patch(object):
    """Represents an StGit patch. This class is mainly concerned with
    reading and writing the on-disk representation of a patch."""

    def __init__(self, stack, name):
        self._stack = stack
        self.name = name

    @property
    def _ref(self):
        return 'refs/patches/%s/%s' % (self._stack.name, self.name)

    @property
    def _log_ref(self):
        return self._ref + '.log'

    @property
    def commit(self):
        return self._stack.repository.refs.get(self._ref)

    @property
    def _compat_dir(self):
        return os.path.join(self._stack.directory, 'patches', self.name)

    def _write_compat_files(self, new_commit, msg):
        """Write files used by the old infrastructure."""
        def write(name, val, multiline=False):
            fn = os.path.join(self._compat_dir, name)
            fn = fsencode_utf8(fn)
            if val:
                utils.write_string(fn, val, multiline)
            elif os.path.isfile(fn):
                os.remove(fn)

        def write_patchlog():
            try:
                old_log = [self._stack.repository.refs.get(self._log_ref)]
            except KeyError:
                old_log = []
            cd = CommitData(
                tree=new_commit.data.tree,
                parents=old_log,
                message='%s\t%s' % (msg, new_commit.sha1),
            )
            c = self._stack.repository.commit(cd)
            self._stack.repository.refs.set(self._log_ref, c, msg)
            return c

        d = new_commit.data
        write('authname', d.author.name)
        write('authemail', d.author.email)
        write('authdate', d.author.date)
        write('commname', d.committer.name)
        write('commemail', d.committer.email)
        write('description', d.message_str, multiline=True)
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

    def _delete_compat_files(self):
        if os.path.isdir(self._compat_dir):
            for f in os.listdir(self._compat_dir):
                os.remove(os.path.join(self._compat_dir, f))
            os.rmdir(self._compat_dir)
        try:
            # this compatibility log ref might not exist
            self._stack.repository.refs.delete(self._log_ref)
        except KeyError:
            pass

    def set_commit(self, commit, msg):
        try:
            old_sha1 = self.commit.sha1
        except KeyError:
            old_sha1 = None
        self._write_compat_files(commit, msg)
        self._stack.repository.refs.set(self._ref, commit, msg)
        if old_sha1 and old_sha1 != commit.sha1:
            self._stack.repository.copy_notes(old_sha1, commit.sha1)

    def set_name(self, name, msg):
        commit = self.commit
        self.delete()
        self.name = name
        self._write_compat_files(commit, msg)
        self._stack.repository.refs.set(self._ref, commit, msg)

    def delete(self):
        self._delete_compat_files()
        self._stack.repository.refs.delete(self._ref)

    def is_empty(self):
        return self.commit.data.is_nochange()

    def files(self):
        """Return the set of files this patch touches."""
        fs = set()
        for dt in self._stack.repository.diff_tree_files(
            self.commit.data.parent.data.tree, self.commit.data.tree,
        ):
            _, _, _, _, _, oldname, newname = dt
            fs.add(oldname)
            fs.add(newname)
        return fs


class PatchOrder(object):
    """Keeps track of patch order, and which patches are applied.
    Works with patch names, not actual patches."""

    def __init__(self, stack):
        self._stack = stack
        self._lists = {}

    def _read_file(self, fn):
        return tuple(utils.read_strings(
            os.path.join(self._stack.directory, fn))
        )

    def _write_file(self, fn, val):
        utils.write_strings(os.path.join(self._stack.directory, fn), val)

    def _get_list(self, name):
        if name not in self._lists:
            self._lists[name] = self._read_file(name)
        return self._lists[name]

    def _set_list(self, name, val):
        val = tuple(val)
        if val != self._lists.get(name, None):
            self._lists[name] = val
            self._write_file(name, val)

    @property
    def applied(self):
        return self._get_list('applied')

    @property
    def unapplied(self):
        return self._get_list('unapplied')

    @property
    def hidden(self):
        return self._get_list('hidden')

    @property
    def all(self):
        return self.applied + self.unapplied + self.hidden

    @property
    def all_visible(self):
        return self.applied + self.unapplied

    def set_order(self, applied, unapplied, hidden):
        self._set_list('applied', applied)
        self._set_list('unapplied', unapplied)
        self._set_list('hidden', hidden)

    def rename_patch(self, old_name, new_name):
        for list_name in ['applied', 'unapplied', 'hidden']:
            patch_list = list(self._get_list(list_name))
            try:
                index = patch_list.index(old_name)
            except ValueError:
                continue
            else:
                patch_list[index] = new_name
                self._set_list(list_name, patch_list)
                break
        else:
            raise AssertionError('"%s" not found in patchorder' % old_name)

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
        self._stack = stack

        def create_patch(name):
            p = Patch(self._stack, name)
            p.commit  # raise exception if the patch doesn't exist
            return p

        self._patches = ObjectCache(create_patch)  # name -> Patch

    def exists(self, name):
        try:
            self.get(name)
            return True
        except KeyError:
            return False

    def get(self, name):
        return self._patches[name]

    def is_name_valid(self, name):
        if '/' in name:
            # TODO slashes in patch names could be made to be okay
            return False
        ref_name = 'refs/patches/%s/%s' % (self._stack.name, name)
        p = self._stack.repository.run(['git', 'check-ref-format', ref_name])
        p.returns([0, 1]).discard_stderr().discard_output()
        return p.exitcode == 0

    def new(self, name, commit, msg):
        assert name not in self._patches
        assert self.is_name_valid(name)
        p = Patch(self._stack, name)
        p.set_commit(commit, msg)
        self._patches[name] = p
        return p


class Stack(Branch):
    """Represents an StGit stack (that is, a git branch with some extra
    metadata)."""

    _repo_subdir = 'patches'

    def __init__(self, repository, name):
        Branch.__init__(self, repository, name)
        self.patchorder = PatchOrder(self)
        self.patches = Patches(self)
        if not stackupgrade.update_to_current_format_version(repository, name):
            raise StackException('%s: branch not initialized' % name)

    @property
    def directory(self):
        return os.path.join(
            self.repository.directory, self._repo_subdir, self.name
        )

    @property
    def base(self):
        if self.patchorder.applied:
            return self.patches.get(
                self.patchorder.applied[0]
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
        top = self.patches.get(self.patchorder.applied[-1]).commit
        return self.head == top

    def set_parents(self, remote, branch):
        if remote:
            self.set_parent_remote(remote)
        if branch:
            self.set_parent_branch(branch)
            config.set('branch.%s.stgit.parentbranch' % self.name, branch)

    @property
    def protected(self):
        return config.getbool('branch.%s.stgit.protect' % self.name)

    @protected.setter
    def protected(self, protect):
        protect_key = 'branch.%s.stgit.protect' % self.name
        if protect:
            config.set(protect_key, 'true')
        elif self.protected:
            config.unset(protect_key)

    def cleanup(self):
        assert not self.protected, 'attempt to delete protected stack'
        for pn in self.patchorder.all:
            patch = self.patches.get(pn)
            patch.delete()
        shutil.rmtree(self.directory)
        config.remove_section('branch.%s.stgit' % self.name)

    def rename(self, name):
        old_name = self.name
        patch_names = self.patchorder.all
        super(Stack, self).rename(name)
        old_ref_root = 'refs/patches/%s' % old_name
        new_ref_root = 'refs/patches/%s' % name
        empty_id = '0' * 40
        ref_updates = ''
        for pn in patch_names:
            old_ref = '%s/%s' % (old_ref_root, pn)
            new_ref = '%s/%s' % (new_ref_root, pn)
            old_log_ref = old_ref + '.log'
            new_log_ref = new_ref + '.log'
            patch_commit_id = self.repository.refs.get(old_ref).sha1
            log_commit_id = self.repository.refs.get(old_log_ref).sha1

            ref_updates += 'update %s %s %s\n' % (
                new_ref, patch_commit_id, empty_id
            )
            ref_updates += 'update %s %s %s\n' % (
                new_log_ref, log_commit_id, empty_id
            )
            ref_updates += 'delete %s %s\n' % (old_ref, patch_commit_id)
            ref_updates += 'delete %s %s\n' % (old_log_ref, log_commit_id)
        self.repository.run(
            ['git', 'update-ref', '--stdin']
        ).raw_input(ref_updates).discard_output()

        config.rename_section(
            'branch.%s.stgit' % old_name, 'branch.%s.stgit' % name
        )

        utils.rename(
            os.path.join(self.repository.directory, self._repo_subdir),
            old_name,
            name,
        )

    def rename_patch(self, old_name, new_name, msg='rename'):
        if new_name == old_name:
            raise StackException('New patch name same as old: "%s"' % new_name)
        elif self.patches.exists(new_name):
            raise StackException('Patch already exists: "%s"' % new_name)
        elif not self.patches.is_name_valid(new_name):
            raise StackException('Invalid patch name: "%s"' % new_name)
        elif not self.patches.exists(old_name):
            raise StackException('Unknown patch name: "%s"' % old_name)
        self.patchorder.rename_patch(old_name, new_name)
        self.patches.get(old_name).set_name(new_name, msg)

    @classmethod
    def initialise(cls, repository, name=None, switch_to=False):
        """Initialise a Git branch to handle patch series.

        @param repository: The L{Repository} where the L{Stack} will be created
        @param name: The name of the L{Stack}
        """
        if not name:
            name = repository.current_branch_name
        # make sure that the corresponding Git branch exists
        branch = Branch(repository, name)

        dir = os.path.join(repository.directory, cls._repo_subdir, name)
        if os.path.exists(dir):
            raise StackException('%s: branch already initialized' % name)

        if switch_to:
            branch.switch_to()

        # create the stack directory and files
        utils.create_dirs(dir)
        compat_dir = os.path.join(dir, 'patches')
        utils.create_dirs(compat_dir)
        PatchOrder.create(dir)
        config.set(stackupgrade.format_version_key(name),
                   text(stackupgrade.FORMAT_VERSION))

        return repository.get_stack(name)

    @classmethod
    def create(
        cls,
        repository,
        name,
        create_at=None,
        parent_remote=None,
        parent_branch=None,
        switch_to=False,
    ):
        """Create and initialise a Git branch returning the L{Stack} object.

        @param repository: The L{Repository} where the L{Stack} will be created
        @param name: The name of the L{Stack}
        @param create_at: The Git id used as the base for the newly created
            Git branch
        @param parent_remote: The name of the remote Git branch
        @param parent_branch: The name of the parent Git branch
        """
        branch = Branch.create(repository, name, create_at=create_at)
        try:
            stack = cls.initialise(repository, name, switch_to=switch_to)
        except (BranchException, StackException):
            branch.delete()
            raise
        stack.set_parents(parent_remote, parent_branch)
        return stack


class StackRepository(Repository):
    """A git L{Repository<Repository>} with some added StGit-specific
    operations."""

    def __init__(self, *args, **kwargs):
        Repository.__init__(self, *args, **kwargs)
        self._stacks = {}  # name -> Stack

    @property
    def current_stack(self):
        return self.get_stack()

    def get_stack(self, name=None):
        if not name:
            name = self.current_branch_name
        if name not in self._stacks:
            self._stacks[name] = Stack(self, name)
        return self._stacks[name]
