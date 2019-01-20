# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import os

from stgit.exception import StgException
from stgit.run import Run, RunException
from stgit.utils import add_dict

from .objects import Tree


class MergeException(StgException):
    """Exception raised when a merge fails for some reason."""


class MergeConflictException(MergeException):
    """Exception raised when a merge fails due to conflicts."""

    def __init__(self, conflicts):
        MergeException.__init__(self)
        self.conflicts = conflicts


class CheckoutException(StgException):
    """Exception raised when a checkout fails."""


class Index(object):
    """Represents a git index file."""

    def __init__(self, repository, filename):
        self.__repository = repository
        if os.path.isdir(filename):
            # Create a temp index in the given directory.
            self.__filename = os.path.join(
                filename, 'index.temp-%d-%x' % (os.getpid(), id(self))
            )
            self.delete()
        else:
            self.__filename = filename

    @property
    def env(self):
        return add_dict(
            self.__repository.env, {'GIT_INDEX_FILE': self.__filename}
        )

    def run(self, args, env=()):
        return Run(*args).env(add_dict(self.env, env))

    def read_tree(self, tree):
        self.run(['git', 'read-tree', tree.sha1]).no_output()

    def write_tree(self):
        """Write the index contents to the repository.
        @return: The resulting L{Tree}
        @rtype: L{Tree}"""
        try:
            return self.__repository.get_tree(
                self.run(['git', 'write-tree'])
                .discard_stderr()
                .output_one_line()
            )
        except RunException:
            raise MergeException('Conflicting merge')

    def is_clean(self, tree):
        """Check whether the index is clean relative to the given treeish."""
        try:
            self.run(
                ['git', 'diff-index', '--quiet', '--cached', tree.sha1]
            ).discard_output()
        except RunException:
            return False
        else:
            return True

    def apply(self, patch_bytes, quiet):
        """In-index patch application, no worktree involved."""
        try:
            r = self.run(['git', 'apply', '--cached'])
            r.encoding(None).raw_input(patch_bytes)
            if quiet:
                r = r.discard_stderr()
            r.no_output()
        except RunException:
            raise MergeException('Patch does not apply cleanly')

    def apply_treediff(self, tree1, tree2, quiet):
        """Apply the diff from C{tree1} to C{tree2} to the index."""
        # Passing --full-index here is necessary to support binary
        # files. It is also sufficient, since the repository already
        # contains all involved objects; in other words, we don't have
        # to use --binary.
        self.apply(
            self.__repository.diff_tree(tree1, tree2, ['--full-index']), quiet
        )

    def merge(self, base, ours, theirs, current=None):
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
        assert current is None or isinstance(current, Tree)

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
            self.apply_treediff(base, theirs, quiet=True)
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
        for line in self.run(
            ['git', 'ls-files', '-z', '--unmerged']
        ).output_lines('\0'):
            stat, path = line.split('\t', 1)
            paths.add(path)
        return paths


class Worktree(object):
    """Represents a git worktree (that is, a checked-out file tree)."""

    def __init__(self, directory):
        self.__directory = directory

    @property
    def env(self):
        return {'GIT_WORK_TREE': '.'}

    @property
    def env_in_cwd(self):
        return {'GIT_WORK_TREE': self.directory}

    @property
    def directory(self):
        return self.__directory


class IndexAndWorktree(object):
    """Represents a git index and a worktree. Anything that an index or
    worktree can do on their own are handled by the L{Index} and
    L{Worktree} classes; this class concerns itself with the
    operations that require both."""

    def __init__(self, index, worktree):
        self.__index = index
        self.__worktree = worktree

    @property
    def index(self):
        return self.__index

    @property
    def env(self):
        return add_dict(self.__index.env, self.__worktree.env)

    @property
    def env_in_cwd(self):
        return self.__worktree.env_in_cwd

    @property
    def cwd(self):
        return self.__worktree.directory

    def run(self, args, env=()):
        return Run(*args).env(add_dict(self.env, env)).cwd(self.cwd)

    def run_in_cwd(self, args):
        return Run(*args).env(add_dict(self.env, self.env_in_cwd))

    def checkout_hard(self, tree):
        assert isinstance(tree, Tree)
        self.run(
            ['git', 'read-tree', '--reset', '-u', tree.sha1]
        ).discard_output()

    def checkout(self, old_tree, new_tree):
        # TODO: Optionally do a 3-way instead of doing nothing when we
        # have a problem. Or maybe we should stash changes in a patch?
        assert isinstance(old_tree, Tree)
        assert isinstance(new_tree, Tree)
        try:
            self.run(
                [
                    'git',
                    'read-tree',
                    '-u',
                    '-m',
                    '--exclude-per-directory=.gitignore',
                    old_tree.sha1,
                    new_tree.sha1,
                ]
            ).discard_output()
        except RunException:
            raise CheckoutException('Index/workdir dirty')

    def merge(self, base, ours, theirs, interactive=False):
        assert isinstance(base, Tree)
        assert isinstance(ours, Tree)
        assert isinstance(theirs, Tree)
        try:
            r = self.run(
                [
                    'git',
                    'merge-recursive',
                    base.sha1,
                    '--',
                    ours.sha1,
                    theirs.sha1,
                ],
                env={
                    'GITHEAD_%s' % base.sha1: 'ancestor',
                    'GITHEAD_%s' % ours.sha1: 'current',
                    'GITHEAD_%s' % theirs.sha1: 'patched',
                },
            )
            r.returns([0, 1])
            output = r.output_lines()
            if r.exitcode:
                # There were conflicts
                if interactive:
                    self.mergetool()
                else:
                    conflicts = [l for l in output if l.startswith('CONFLICT')]
                    raise MergeConflictException(conflicts)
        except RunException:
            raise MergeException('Index/worktree dirty')

    def mergetool(self, files=()):
        """Invoke 'git mergetool' on the current IndexAndWorktree to resolve
        any outstanding conflicts. If 'not files', all the files in an
        unmerged state will be processed."""
        self.run(['git', 'mergetool'] + list(files)).returns([0, 1]).run()
        # check for unmerged entries (prepend 'CONFLICT ' for consistency with
        # merge())
        conflicts = ['CONFLICT ' + f for f in self.index.conflicts()]
        if conflicts:
            raise MergeConflictException(conflicts)

    def ls_files(self, tree, pathlimits):
        """Given a sequence of file paths, return files known to git.

        The input path limits are specified relative to the current directory.
        The output files are relative to the repository root. A RunException
        will be raised if any of the path limits are unknown to git.
        """
        if not pathlimits:
            return set()
        cmd = [
            'git',
            'ls-files',
            '-z',
            '--with-tree=' + tree.sha1,
            '--error-unmatch',
            '--full-name',
            '--',
        ]
        cmd.extend(pathlimits)
        return set(self.run_in_cwd(cmd).output_lines('\0'))

    def diff(self, tree, diff_opts=(), pathlimits=(), binary=True, stat=False):
        self.run(
            ['git', 'update-index', '-q', '--unmerged', '--refresh']
        ).discard_output()
        cmd = ['git', 'diff-index']
        if stat:
            cmd.extend(['--stat', '--summary'])
            cmd.extend(o for o in diff_opts if o != '--binary')
        else:
            cmd.append('--patch')
            if binary and '--binary' not in diff_opts:
                cmd.append('--binary')
            cmd.extend(diff_opts)
        cmd.extend([tree.sha1, '--'])
        cmd.extend(pathlimits)
        return self.run(cmd).decoding(None).raw_output()

    def changed_files(self, tree, pathlimits=[]):
        """Return the set of files in the worktree that have changed with
        respect to C{tree}. The listing is optionally restricted to
        those files that match any of the path limiters given.

        The path limiters are relative to the current working
        directory; the returned file names are relative to the
        repository root."""
        assert isinstance(tree, Tree)
        return set(
            self.run_in_cwd(
                ['git', 'diff-index', tree.sha1, '--name-only', '-z', '--']
                + list(pathlimits)
            ).output_lines('\0')
        )

    def update_index(self, paths):
        """Update the index with files from the worktree. C{paths} is an
        iterable of paths relative to the root of the repository."""
        cmd = ['git', 'update-index', '--remove']
        self.run(cmd + ['-z', '--stdin']).input_nulterm(paths).discard_output()

    def worktree_clean(self):
        """Check whether the worktree is clean relative to index."""
        try:
            self.run(
                ['git', 'update-index', '--ignore-submodules', '--refresh']
            ).discard_output()
        except RunException:
            return False
        else:
            return True
