import os
import time

from stgit.compat import environ_get
from stgit.exception import StgException
from stgit.run import Run, RunException
from stgit.utils import add_dict

from .objects import Commit, Tree


class MergeException(StgException):
    """Exception raised when a merge fails for some reason."""


class MergeConflictException(MergeException):
    """Exception raised when a merge fails due to conflicts."""

    def __init__(self, conflicts):
        super().__init__()
        self.conflicts = conflicts


class CheckoutException(StgException):
    """Exception raised when a checkout fails."""


class Index:
    """Represents a Git index file."""

    def __init__(self, repository, index_path):
        self._repository = repository
        self._path = index_path

    @classmethod
    def default(cls, repository):
        index_path = environ_get(
            'GIT_INDEX_FILE',
            os.path.join(repository.directory, 'index'),
        )
        return cls(repository, index_path)

    @property
    def env(self):
        return add_dict(self._repository.env, {'GIT_INDEX_FILE': self._path})

    def run(self, args, env=()):
        return Run(*args).env(add_dict(self.env, env))

    def read_tree(self, tree):
        self.run(['git', 'read-tree', tree.sha1]).no_output()

    def write_tree(self):
        """Write the index contents to the repository.

        :return: The resulting :class:`Tree`.

        """
        try:
            return self._repository.get_tree(
                self.run(['git', 'write-tree']).discard_stderr().output_one_line()
            )
        except RunException:
            raise MergeException('Conflicting merge')

    def is_clean(self, tree):
        """Check whether the index is clean relative to the given tree-ish."""
        try:
            self.run(
                ['git', 'diff-index', '--quiet', '--cached', tree.sha1]
            ).discard_output()
        except RunException:
            return False
        else:
            return True

    def apply(self, patch_bytes, quiet):
        """Apply patch to index, no worktree involved."""
        try:
            r = self.run(['git', 'apply', '--cached'])
            r.encoding(None).raw_input(patch_bytes)
            if quiet:
                r = r.discard_stderr()
            r.no_output()
        except RunException:
            raise MergeException('Patch does not apply cleanly')

    def apply_treediff(self, tree1, tree2, quiet):
        """Apply the diff from two :class:`Tree`s to the index."""
        # Passing --full-index here is necessary to support binary
        # files. It is also sufficient, since the repository already
        # contains all involved objects; in other words, we don't have
        # to use --binary.
        self.apply(self._repository.diff_tree(tree1, tree2, ['--full-index']), quiet)

    def merge(self, base, ours, theirs, current=None):
        """Perform three-way merge.

        The index (and only the index) is used to do a 3-way merge of the :class:`Tree`s
        ``base``, ``ours``, and ``theirs``. The merge will either succeed (in which case
        the first half of the return value is the resulting tree) or fail cleanly (in
        which case the first half of the return value is None).

        If ``current`` is given (not None), it is assumed to be the :class:`Tree`
        currently stored in the index; this information is used to avoid having to read
        the right tree (either of ``ours`` and ``theirs``) into the index if it is
        already there. The second half of the return value is the tree now stored in the
        index, or None if unknown. If the merge succeeded, this is often the merge
        result.

        """
        assert isinstance(base, Tree)
        assert isinstance(ours, Tree)
        assert isinstance(theirs, Tree)
        assert isinstance(current, Tree) or current is None

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
        if os.path.isfile(self._path):
            os.remove(self._path)

    def conflicts(self):
        """The set of conflicting paths."""
        paths = set()
        for line in self.run(['git', 'ls-files', '-z', '--unmerged']).output_lines(
            '\0'
        ):
            stat, path = line.split('\t', 1)
            paths.add(path)
        return paths


class TemporaryIndex(Index):
    def __init__(self, repository):
        index_filename = 'index.temp-%d-%x' % (os.getpid(), int(time.monotonic() * 1e9))
        index_path = os.path.join(repository.directory, index_filename)
        if os.path.isfile(index_path):
            os.remove(index_path)
        super().__init__(repository, index_path)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.delete()
        return None


class Worktree:
    """Represents a git worktree (that is, a checked-out file tree)."""

    def __init__(self, directory):
        self.directory = directory

    @classmethod
    def default(cls):
        path = environ_get('GIT_WORK_TREE', None)
        if not path:
            path = Run('git', 'rev-parse', '--show-toplevel').output_one_line()
        return cls(path)

    @property
    def env(self):
        return {'GIT_WORK_TREE': self.directory}


class IndexAndWorktree:
    """Represents a git index and a worktree.

    Anything that an index or worktree can do on their own are handled by :class:`Index`
    and :class:`Worktree`. This class concerns itself with the operations involving
    both.

    """

    def __init__(self, index, worktree):
        self.index = index
        self._worktree = worktree

    @property
    def env(self):
        return add_dict(self.index.env, self._worktree.env)

    def run(self, args, env=()):
        return Run(*args).env(add_dict(self.env, env)).cwd(self._worktree.directory)

    def run_in_cwd(self, args):
        return Run(*args).env(self.env)

    def checkout_hard(self, tree):
        assert isinstance(tree, (Commit, Tree))
        self.run(['git', 'read-tree', '--reset', '-u', tree.sha1]).discard_output()

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
                ['git', 'merge-recursive', base.sha1, '--', ours.sha1, theirs.sha1],
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
                    conflicts = [line for line in output if line.startswith('CONFLICT')]
                    raise MergeConflictException(conflicts)
        except RunException:
            raise MergeException('Index/worktree dirty')

    def mergetool(self):
        """Resolve outstanding conflicts with `git mergetool`."""
        self.run(['git', 'mergetool']).returns([0, 1]).run()
        # check for unmerged entries (prepend 'CONFLICT ' for consistency with merge())
        conflicts = ['CONFLICT ' + f for f in self.index.conflicts()]
        if conflicts:
            raise MergeConflictException(conflicts)

    def ls_files(self, tree, pathlimits):
        """Given a sequence of file paths, return files known to Git.

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
        self.refresh_index()
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

    def apply(self, patch_bytes, quiet, reject=False, strip=None):
        """Apply patch to worktree."""
        cmd = ['git', 'apply', '--index']
        if reject:
            cmd.append('--reject')
        if strip is not None:
            cmd.append('-p%s' % (strip,))
        try:
            r = self.run(cmd).encoding(None).raw_input(patch_bytes)
            if quiet:
                r.discard_stderr()
            r.no_output()
        except RunException:
            raise MergeException('Patch does not apply cleanly')

    def diffstat(self, diff):
        return (
            self.run(['git', 'apply', '--stat', '--summary'])
            .encoding(None)
            .raw_input(diff)
            .decoding('utf-8')
            .raw_output()
        )

    def changed_files(self, tree, pathlimits=()):
        """Find files changed in worktree with respect to a tree.

        The listing is optionally restricted to those files that match any of the given
        path limits.

        The path limits are relative to the current working directory; the returned file
        names are relative to the repository root.

        """
        assert isinstance(tree, Tree), tree
        return set(
            self.run_in_cwd(
                ['git', 'diff-index', tree.sha1, '--name-only', '-z', '--']
                + list(pathlimits)
            ).output_lines('\0')
        )

    def refresh_index(self):
        """Refresh index with stat() information from the working directory."""
        self.run(
            ['git', 'update-index', '-q', '--unmerged', '--refresh']
        ).discard_output()

    def update_index(self, paths):
        """Update the index with files from the worktree.

        :param paths: iterable of paths relative to the root of the repository

        """
        self.run(
            ['git', 'update-index', '--remove', '--add', '-z', '--stdin']
        ).input_nulterm(paths).discard_output()

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
