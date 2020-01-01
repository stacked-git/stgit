# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import atexit
import os
import re
import signal

from stgit import utils
from stgit.compat import environ_get
from stgit.exception import StgException
from stgit.lib.objcache import ObjectCache
from stgit.run import Run, RunException
from stgit.utils import add_dict

from .iw import Index, IndexAndWorktree, MergeException, Worktree
from .objects import Blob, Commit, Tree


class RepositoryException(StgException):
    """Base class for all exceptions due to failed L{Repository} operations."""


class DetachedHeadException(RepositoryException):
    """Exception raised when HEAD is detached (that is, there is no
    current branch)."""

    def __init__(self):
        RepositoryException.__init__(self, 'Not on any branch')


class Refs(object):
    """Accessor for the refs stored in a git repository. Will
    transparently cache the values of all refs."""

    def __init__(self, repository):
        self._repository = repository
        self._refs = None

    def _ensure_refs_cache(self):
        """(Re-)Build the cache of all refs in the repository."""
        if self._refs is not None:
            return
        self._refs = {}
        runner = self._repository.run(['git', 'show-ref'])
        try:
            lines = runner.output_lines()
        except RunException:
            # as this happens both in non-git trees and empty git
            # trees, we silently ignore this error
            return
        for line in lines:
            m = re.match(r'^([0-9a-f]{40})\s+(\S+)$', line)
            sha1, ref = m.groups()
            self._refs[ref] = sha1

    def __iter__(self):
        self._ensure_refs_cache()
        return iter(self._refs)

    def reset_cache(self):
        """Reset cached refs such that cache is rebuilt on next access.

        Useful if refs are known to have changed due to an external command
        such as `git pull`.

        """
        self._refs = None

    def get(self, ref):
        """Get the Commit the given ref points to. Throws KeyError if ref
        doesn't exist."""
        self._ensure_refs_cache()
        return self._repository.get_commit(self._refs[ref])

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
        self._ensure_refs_cache()
        old_sha1 = self._refs.get(ref, '0' * 40)
        new_sha1 = commit.sha1
        if old_sha1 != new_sha1:
            self._repository.run(
                ['git', 'update-ref', '-m', msg, ref, new_sha1, old_sha1]
            ).no_output()
            self._refs[ref] = new_sha1

    def delete(self, ref):
        """Delete the given ref. Throws KeyError if ref doesn't exist."""
        self._ensure_refs_cache()
        self._repository.run(
            ['git', 'update-ref', '-d', ref, self._refs[ref]]
        ).no_output()
        del self._refs[ref]


class CatFileProcess(object):
    def __init__(self, repo):
        self._repository = repo
        self._proc = None
        atexit.register(self._shutdown)

    def _get_process(self):
        if not self._proc:
            self._proc = self._repository.run(
                ['git', 'cat-file', '--batch']
            ).run_background()
        return self._proc

    def _shutdown(self):
        if self._proc:
            self._proc.stdin.close()
            os.kill(self._proc.pid(), signal.SIGTERM)
            self._proc.wait()

    def cat_file(self, sha1):
        p = self._get_process()
        p.stdin.write('%s\n' % sha1)
        p.stdin.flush()

        # Read until we have the entire status line.
        s = b''
        while b'\n' not in s:
            s += os.read(p.stdout.fileno(), 4096)
        h, b = s.split(b'\n', 1)
        header = h.decode('utf-8')
        if header == '%s missing' % sha1:
            raise RepositoryException('Cannot cat %s' % sha1)
        name, type_, size = header.split()
        assert name == sha1
        size = int(size)

        # Read until we have the entire object plus the trailing
        # newline.
        while len(b) < size + 1:
            b += os.read(p.stdout.fileno(), 4096)
        content = b[:size]
        return type_, content


class DiffTreeProcesses(object):
    def __init__(self, repo):
        self._repository = repo
        self._procs = {}
        atexit.register(self._shutdown)

    def _get_process(self, args):
        args = tuple(args)
        if args not in self._procs:
            self._procs[args] = self._repository.run(
                ['git', 'diff-tree', '--stdin'] + list(args)
            ).run_background()
        return self._procs[args]

    def _shutdown(self):
        for p in self._procs.values():
            os.kill(p.pid(), signal.SIGTERM)
            p.wait()

    def diff_trees(self, args, sha1a, sha1b):
        p = self._get_process(args)
        query = ('%s %s\n' % (sha1a, sha1b)).encode('utf-8')
        end = b'EOF\n'  # arbitrary string that's not a 40-digit hex number
        os.write(p.stdin.fileno(), query + end)
        p.stdin.flush()
        data = bytes()
        while not (data.endswith(b'\n' + end) or data.endswith(b'\0' + end)):
            data += os.read(p.stdout.fileno(), 4096)
        assert data.startswith(query)
        assert data.endswith(end)
        return data[len(query):-len(end)]


class Repository(object):
    """Represents a git repository."""

    def __init__(self, directory):
        self._git_dir = directory
        self.refs = Refs(self)
        self._blobs = ObjectCache(lambda sha1: Blob(self, sha1))
        self._trees = ObjectCache(lambda sha1: Tree(self, sha1))
        self._commits = ObjectCache(lambda sha1: Commit(self, sha1))
        self._default_index = None
        self._default_worktree = None
        self._default_iw = None
        self._catfile = CatFileProcess(self)
        self._difftree = DiffTreeProcesses(self)

    @property
    def env(self):
        return {'GIT_DIR': self._git_dir}

    @classmethod
    def default(cls):
        """Return the default repository."""
        try:
            return cls(Run('git', 'rev-parse', '--git-dir').output_one_line())
        except RunException:
            raise RepositoryException('Cannot find git repository')

    @property
    def current_branch_name(self):
        """Return the name of the current branch."""
        return utils.strip_prefix('refs/heads/', self.head_ref)

    @property
    def default_index(self):
        """An L{Index} object representing the default index file for the
        repository."""
        if self._default_index is None:
            self._default_index = Index(
                self,
                (
                    environ_get('GIT_INDEX_FILE', None)
                    or os.path.join(self._git_dir, 'index')
                ),
            )
        return self._default_index

    def temp_index(self):
        """Return an L{Index} object representing a new temporary index file
        for the repository."""
        return Index(self, self._git_dir)

    @property
    def default_worktree(self):
        """A L{Worktree} object representing the default work tree."""
        if self._default_worktree is None:
            path = environ_get('GIT_WORK_TREE', None)
            if not path:
                o = Run('git', 'rev-parse', '--show-cdup').output_lines()
                o = o or ['.']
                assert len(o) == 1
                path = o[0]
            self._default_worktree = Worktree(path)
        return self._default_worktree

    @property
    def default_iw(self):
        """An L{IndexAndWorktree} object representing the default index and
        work tree for this repository."""
        if self._default_iw is None:
            self._default_iw = IndexAndWorktree(
                self.default_index, self.default_worktree
            )
        return self._default_iw

    @property
    def directory(self):
        return self._git_dir

    def run(self, args, env=()):
        return Run(*args).env(add_dict(self.env, env))

    def cat_object(self, sha1):
        return self._catfile.cat_file(sha1)

    def rev_parse(self, rev, discard_stderr=False, object_type='commit'):
        assert object_type in ('commit', 'tree', 'blob')
        getter = getattr(self, 'get_' + object_type)
        try:
            return getter(
                self.run(['git', 'rev-parse', '%s^{%s}' % (rev, object_type)])
                .discard_stderr(discard_stderr)
                .output_one_line()
            )
        except RunException:
            raise RepositoryException('%s: No such %s' % (rev, object_type))

    def get_blob(self, sha1):
        return self._blobs[sha1]

    def get_tree(self, sha1):
        return self._trees[sha1]

    def get_commit(self, sha1):
        return self._commits[sha1]

    def get_object(self, type, sha1):
        return {
            Blob.typename: self.get_blob,
            Tree.typename: self.get_tree,
            Commit.typename: self.get_commit,
        }[type](sha1)

    def commit(self, objectdata):
        return objectdata.commit(self)

    @property
    def head_ref(self):
        try:
            return self.run(
                ['git', 'symbolic-ref', '-q', 'HEAD']
            ).output_one_line()
        except RunException:
            raise DetachedHeadException()

    def set_head_ref(self, ref, msg):
        self.run(['git', 'symbolic-ref', '-m', msg, 'HEAD', ref]).no_output()

    def get_merge_bases(self, commit1, commit2):
        """Return a set of merge bases of two commits."""
        sha1_list = self.run(
            ['git', 'merge-base', '--all', commit1.sha1, commit2.sha1]
        ).output_lines()
        return [self.get_commit(sha1) for sha1 in sha1_list]

    def describe(self, commit):
        """Use git describe --all on the given commit."""
        return (
            self.run(['git', 'describe', '--all', commit.sha1])
            .discard_stderr()
            .discard_exitcode()
            .raw_output()
        )

    def simple_merge(self, base, ours, theirs):
        index = self.temp_index()
        try:
            result, index_tree = index.merge(base, ours, theirs)
        finally:
            index.delete()
        return result

    def apply(self, tree, patch_bytes, quiet):
        """Given a L{Tree} and a patch, will either return the new L{Tree}
        that results when the patch is applied, or None if the patch
        couldn't be applied."""
        assert isinstance(tree, Tree)
        if not patch_bytes:
            return tree
        index = self.temp_index()
        try:
            index.read_tree(tree)
            try:
                index.apply(patch_bytes, quiet)
                return index.write_tree()
            except MergeException:
                return None
        finally:
            index.delete()

    def submodules(self, tree):
        """Given a L{Tree}, return list of paths which are submodules."""
        assert isinstance(tree, Tree)
        # A simple regex to match submodule entries
        regex = re.compile(r'160000 commit [0-9a-f]{40}\t(.*)$')
        # First, use ls-tree to get all the trees and links
        files = self.run(
            ['git', 'ls-tree', '-d', '-r', '-z', tree.sha1]
        ).output_lines('\0')
        # Then extract the paths of any submodules
        return set(m.group(1) for m in map(regex.match, files) if m)

    def diff_tree(
        self, t1, t2, diff_opts=(), pathlimits=(), binary=True, stat=False
    ):
        """Given two L{Tree}s C{t1} and C{t2}, return the patch that takes
        C{t1} to C{t2}.

        @type diff_opts: list of strings
        @param diff_opts: Extra diff options
        @rtype: String
        @return: Patch text"""
        assert isinstance(t1, Tree)
        assert isinstance(t2, Tree)
        if stat:
            args = ['--stat', '--summary']
            args.extend(o for o in diff_opts if o != '--binary')
        else:
            args = ['--patch']
            if binary and '--binary' not in diff_opts:
                args.append('--binary')
            args.extend(diff_opts)
        if pathlimits:
            args.append('--')
            args.extend(pathlimits)
        return self._difftree.diff_trees(args, t1.sha1, t2.sha1)

    def diff_tree_files(self, t1, t2):
        """Given two L{Tree}s C{t1} and C{t2}, iterate over all files for
        which they differ. For each file, yield a tuple with the old
        file mode, the new file mode, the old blob, the new blob, the
        status, the old filename, and the new filename. Except in case
        of a copy or a rename, the old and new filenames are
        identical."""
        assert isinstance(t1, Tree)
        assert isinstance(t2, Tree)
        dt = self._difftree.diff_trees(['-r', '-z'], t1.sha1, t2.sha1)
        i = iter(dt.decode('utf-8').split('\0'))
        try:
            while True:
                x = next(i)
                if not x:
                    continue
                omode, nmode, osha1, nsha1, status = x[1:].split(' ')
                fn1 = next(i)
                if status[0] in ['C', 'R']:
                    fn2 = next(i)
                else:
                    fn2 = fn1
                yield (
                    omode,
                    nmode,
                    self.get_blob(osha1),
                    self.get_blob(nsha1),
                    status,
                    fn1,
                    fn2,
                )
        except StopIteration:
            pass

    def repack(self):
        """Repack all objects into a single pack."""
        self.run(['git', 'repack', '-a', '-d', '-f']).run()

    def copy_notes(self, old_sha1, new_sha1):
        """Copy git notes from the old object to the new one."""
        p = self.run(['git', 'notes', 'copy', old_sha1, new_sha1])
        p.discard_exitcode().discard_stderr().discard_output()
