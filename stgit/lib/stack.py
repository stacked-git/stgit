"""A Python class hierarchy wrapping the StGit on-disk metadata."""

from stgit.config import config
from stgit.exception import StackException
from stgit.lib import log, stackupgrade
from stgit.lib.git import Repository
from stgit.lib.git.branch import Branch, BranchException
from stgit.lib.objcache import ObjectCache


def _stack_state_ref(stack_name):
    """Reference to stack state metadata. A.k.a. the stack's "log"."""
    return 'refs/heads/%s.stgit' % (stack_name,)


def _patch_ref(stack_name, patch_name):
    """Reference to a named patch's commit."""
    return 'refs/patches/%s/%s' % (stack_name, patch_name)


def _patches_ref_prefix(stack_name):
    return _patch_ref(stack_name, '')


class Patch:
    """Represents an StGit patch."""

    def __init__(self, stack, name):
        self._stack = stack
        self.name = name

    @property
    def _ref(self):
        return _patch_ref(self._stack.name, self.name)

    @property
    def commit(self):
        return self._stack.repository.refs.get(self._ref)

    def set_commit(self, commit, msg):
        try:
            old_sha1 = self.commit.sha1
        except KeyError:
            old_sha1 = None
        self._stack.repository.refs.set(self._ref, commit, msg)
        if old_sha1 and old_sha1 != commit.sha1:
            self._stack.repository.copy_notes(old_sha1, commit.sha1)

    def set_name(self, name, msg):
        commit = self.commit
        self.delete()
        self.name = name
        self._stack.repository.refs.set(self._ref, commit, msg)

    def delete(self):
        self._stack.repository.refs.delete(self._ref)

    def is_empty(self):
        return self.commit.data.is_nochange()

    def files(self):
        """Return the set of files this patch touches."""
        fs = set()
        for dt in self._stack.repository.diff_tree_files(
            self.commit.data.parent.data.tree,
            self.commit.data.tree,
        ):
            _, _, _, _, _, oldname, newname = dt
            fs.add(oldname)
            fs.add(newname)
        return fs


class PatchOrder:
    """Keeps track of patch order, and which patches are applied.

    Works with patch names, not actual patches.

    """

    def __init__(self, state):
        self._applied = tuple(state.applied)
        self._unapplied = tuple(state.unapplied)
        self._hidden = tuple(state.hidden)

    @property
    def applied(self):
        return self._applied

    @property
    def unapplied(self):
        return self._unapplied

    @property
    def hidden(self):
        return self._hidden

    @property
    def all(self):
        return self.applied + self.unapplied + self.hidden

    @property
    def all_visible(self):
        return self.applied + self.unapplied

    def set_order(self, applied, unapplied, hidden):
        self._applied = tuple(applied)
        self._unapplied = tuple(unapplied)
        self._hidden = tuple(hidden)

    def rename_patch(self, old_name, new_name):
        for attr in ['_applied', '_unapplied', '_hidden']:
            patch_list = list(getattr(self, attr))
            try:
                index = patch_list.index(old_name)
            except ValueError:
                continue
            else:
                patch_list[index] = new_name
                setattr(self, attr, tuple(patch_list))
                break
        else:
            raise AssertionError('"%s" not found in patchorder' % old_name)


class Patches:
    """Manage the set of :class:`Patch` objects.

    Ensures a single :class:`Patch` instance per patch.

    """

    def __init__(self, stack, state):
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
        ref = _patch_ref(self._stack.name, name)
        p = self._stack.repository.run(['git', 'check-ref-format', ref])
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
    """Represents a StGit stack.

    A StGit stack is a Git branch with extra metadata for patch stack state.

    """

    def __init__(self, repository, name):
        super().__init__(repository, name)
        if not stackupgrade.update_to_current_format_version(repository, name):
            raise StackException('%s: branch not initialized' % name)
        state = log.get_stack_state(self.repository, self.state_ref)
        self._ensure_patch_refs(repository, name, state)
        self.patchorder = PatchOrder(state)
        self.patches = Patches(self, state)

    @property
    def base(self):
        if self.patchorder.applied:
            return self.patches.get(self.patchorder.applied[0]).commit.data.parent
        else:
            return self.head

    @property
    def top(self):
        """Commit of the topmost patch, or the stack base if no patches are applied."""
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

    @property
    def state_ref(self):
        return _stack_state_ref(self.name)

    def cleanup(self):
        assert not self.protected, 'attempt to delete protected stack'
        for pn in self.patchorder.all:
            patch = self.patches.get(pn)
            patch.delete()
        self.repository.refs.delete(self.state_ref)
        config.remove_section('branch.%s.stgit' % self.name)

    def clear_log(self, msg='clear log'):
        state_commit = log.StackState.from_stack(
            prev=None, stack=self, message=msg
        ).commit_state()
        self.repository.refs.set(self.state_ref, state_commit, msg=msg)

    def rename(self, new_name):
        old_name = self.name
        patch_names = self.patchorder.all
        super().rename(new_name)
        renames = []
        for pn in patch_names:
            renames.append((_patch_ref(old_name, pn), _patch_ref(new_name, pn)))
        renames.append((_stack_state_ref(old_name), _stack_state_ref(new_name)))

        self.repository.refs.rename('rename %s to %s' % (old_name, new_name), *renames)

        config.rename_section(
            'branch.%s.stgit' % old_name,
            'branch.%s.stgit' % new_name,
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

    def clone(self, clone_name, msg):
        clone = self.create(
            self.repository,
            name=clone_name,
            msg=msg,
            create_at=self.base,
            parent_remote=self.parent_remote,
            parent_branch=self.name,
        )

        for pn in self.patchorder.all_visible:
            patch = self.patches.get(pn)
            clone.patches.new(pn, patch.commit, 'clone from %s' % self.name)

        clone.patchorder.set_order(
            applied=[],
            unapplied=self.patchorder.all_visible,
            hidden=[],
        )

        prefix = 'branch.%s.' % self.name
        clone_prefix = 'branch.%s.' % clone_name
        for k, v in list(config.getstartswith(prefix)):
            clone_key = k.replace(prefix, clone_prefix, 1)
            config.set(clone_key, v)

        self.repository.refs.set(
            clone.state_ref,
            self.repository.refs.get(self.state_ref),
            msg=msg,
        )

        return clone

    @classmethod
    def initialise(cls, repository, name=None, msg='initialise', switch_to=False):
        """Initialise a Git branch to handle patch stack.

        :param repository: :class:`Repository` where the :class:`Stack` will be created
        :param name: the name of the :class:`Stack`

        """
        if not name:
            name = repository.current_branch_name
        # make sure that the corresponding Git branch exists
        branch = Branch(repository, name)

        stack_state_ref = _stack_state_ref(name)
        if repository.refs.exists(stack_state_ref):
            raise StackException('%s: stack already initialized' % name)

        if switch_to:
            branch.switch_to()

        state_commit = log.StackState(
            repository,
            prev=None,
            head=branch.head,
            applied=[],
            unapplied=[],
            hidden=[],
            patches={},
            message=msg,
        ).commit_state()
        repository.refs.set(stack_state_ref, state_commit, msg)

        return repository.get_stack(name)

    @classmethod
    def create(
        cls,
        repository,
        name,
        msg,
        create_at=None,
        parent_remote=None,
        parent_branch=None,
        switch_to=False,
    ):
        """Create and initialise a Git branch returning the :class:`Stack` object.

        :param repository: :class:`Repository` where the :class:`Stack` will be created
        :param name: name of the :class:`Stack`
        :param msg: message to use in newly created log
        :param create_at: Git id used as the base for the newly created Git branch
        :param parent_remote: name of the parent remote Git branch
        :param parent_branch: name of the parent Git branch

        """
        branch = Branch.create(repository, name, create_at=create_at)
        try:
            stack = cls.initialise(repository, name, msg, switch_to=switch_to)
        except (BranchException, StackException):
            branch.delete()
            raise
        stack.set_parents(parent_remote, parent_branch)
        return stack

    @staticmethod
    def _ensure_patch_refs(repository, stack_name, state):
        """Ensure patch refs in repository match those from stack state."""
        patch_ref_prefix = _patches_ref_prefix(stack_name)

        state_patch_ref_map = {
            _patch_ref(stack_name, pn): commit for pn, commit in state.patches.items()
        }

        state_patch_refs = set(state_patch_ref_map)
        repo_patch_refs = {
            ref for ref in repository.refs if ref.startswith(patch_ref_prefix)
        }

        delete_patch_refs = repo_patch_refs - state_patch_refs
        create_patch_refs = state_patch_refs - repo_patch_refs
        update_patch_refs = {
            ref
            for ref in state_patch_refs - create_patch_refs
            if state_patch_ref_map[ref].sha1 != repository.refs.get(ref).sha1
        }

        if create_patch_refs or update_patch_refs or delete_patch_refs:
            repository.refs.batch_update(
                msg='restore from stack state',
                create=[(ref, state_patch_ref_map[ref]) for ref in create_patch_refs],
                update=[(ref, state_patch_ref_map[ref]) for ref in update_patch_refs],
                delete=delete_patch_refs,
            )


class StackRepository(Repository):
    """A Git :class:`Repository` with some added StGit-specific operations."""

    def __init__(self, directory):
        super().__init__(directory)
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
