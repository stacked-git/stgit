"""The L{StackTransaction} class makes it possible to make complex
updates to an StGit stack in a safe and convenient way."""


import atexit
from itertools import takewhile

from stgit import exception, utils
from stgit.config import config
from stgit.lib.git import CheckoutException, MergeConflictException, MergeException
from stgit.lib.log import log_external_mods, log_stack_state
from stgit.out import out


class TransactionException(exception.StgException):
    """Exception raised when something goes wrong with a :class:`StackTransaction`."""


class TransactionAborted(TransactionException):
    """Raised when :class:`StackTransaction` aborts."""

    def __init__(self):
        super().__init__('Command aborted (all changes rolled back)')


class TransactionHalted(TransactionException):
    """Exception raised when a :class:`StackTransaction` stops part-way through.

    Used to make a non-local jump from the transaction setup to the part of the
    transaction code where the transaction is run.

    """


def _print_current_patch(old_applied, new_applied):
    def now_at(pn):
        out.info('Now at patch "%s"' % pn)

    if not old_applied and not new_applied:
        pass
    elif not old_applied:
        now_at(new_applied[-1])
    elif not new_applied:
        out.info('No patch applied')
    elif old_applied[-1] == new_applied[-1]:
        pass
    else:
        now_at(new_applied[-1])


class _TransPatchMap(dict):
    """Maps patch names to :class:`Commit` objects."""

    def __init__(self, stack):
        super().__init__()
        self._stack = stack

    def __getitem__(self, pn):
        try:
            return super().__getitem__(pn)
        except KeyError:
            return self._stack.patches[pn]


class StackTransaction:
    """Atomically perform complex updates to StGit stack state.

    A stack transaction will either succeed or fail cleanly.

    The basic theory of operation is the following:

    1. Create a transaction object.
    2. Inside a::

       try:
         ...
       except TransactionHalted:
         pass

       block, update the transaction with e.g. methods like :meth:`pop_patches` and
       :meth:`push_patch`. This may create new Git objects such as commits, but will not
       write any refs. This means that in case of a fatal error we can just walk away,
       no clean-up required.

       Some operations may need to touch the index and/or working tree, though. But they
       are cleaned up when needed.

    3. After the ``try`` block--wheher or not the setup ran to completion or halted
       part-way through by raising a :exc:`TransactionHalted` exception--call the
       transaction's :meth:`run` method. This will either succeed in writing the updated
       state to refs and index+worktree, or fail without having done anything.

    """

    def __init__(self, stack, discard_changes=False, allow_conflicts=False):
        """Initialize a new :class:`StackTransaction`.

        :param discard_changes: Discard any changes in index+worktree
        :type discard_changes: bool
        :param allow_conflicts: Whether to allow pre-existing conflicts
        :type allow_conflicts: bool or function taking a :class:`StackTransaction`
                               instance as its argument

        """
        self.stack = stack

        self.patches = _TransPatchMap(stack)
        self._applied = list(self.stack.patchorder.applied)
        self._unapplied = list(self.stack.patchorder.unapplied)
        self._hidden = list(self.stack.patchorder.hidden)
        self._updated_head = None
        self._updated_base = None
        self._current_tree = self.stack.head.data.tree
        self._temp_index = None
        self._temp_index_tree = None
        self._error = None
        self._conflicts = None

        self._discard_changes = discard_changes
        if isinstance(allow_conflicts, bool):
            self._allow_conflicts = lambda _: allow_conflicts
        else:
            self._allow_conflicts = allow_conflicts

    @property
    def applied(self):
        return self._applied

    @applied.setter
    def applied(self, value):
        self._applied = list(value)

    @property
    def unapplied(self):
        return self._unapplied

    @unapplied.setter
    def unapplied(self, value):
        self._unapplied = list(value)

    @property
    def hidden(self):
        return self._hidden

    @hidden.setter
    def hidden(self, value):
        self._hidden = list(value)

    @property
    def all_patches(self):
        return self._applied + self._unapplied + self._hidden

    @property
    def base(self):
        if self._updated_base is None:
            return self.stack.base
        else:
            return self._updated_base

    @base.setter
    def base(self, value):
        assert not self._applied or self.patches[self.applied[0]].data.parent == value
        self._updated_base = value

    @property
    def temp_index(self):
        if not self._temp_index:
            self._temp_index = self.stack.repository.temp_index()
            atexit.register(self._temp_index.delete)
        return self._temp_index

    @property
    def top(self):
        if self._applied:
            return self.patches[self._applied[-1]]
        else:
            return self.base

    @property
    def head(self):
        if self._updated_head:
            return self._updated_head
        else:
            return self.top

    @head.setter
    def head(self, value):
        self._updated_head = value

    def _assert_head_top_equal(self):
        if self.stack.head != self.stack.top:
            out.error(
                'HEAD and top are not the same.',
                'This can happen if you modify a branch with git.',
                '"stg repair --help" explains more about what to do next.',
            )
            raise TransactionAborted()

    def _checkout(self, tree, iw, allow_bad_head):
        assert iw is not None
        if not allow_bad_head:
            self._assert_head_top_equal()
        if self._current_tree == tree and not self._discard_changes:
            # No tree change, but we still want to make sure that
            # there are no unresolved conflicts. Conflicts
            # conceptually "belong" to the topmost patch, and just
            # carrying them along to another patch is confusing.
            if self._allow_conflicts(self) or not iw.index.conflicts():
                return
            out.error('Need to resolve conflicts first')
            raise TransactionAborted()
        if self._discard_changes:
            iw.checkout_hard(tree)
        else:
            iw.checkout(self._current_tree, tree)
        self._current_tree = tree

    def _check_consistency(self):
        remaining = set(self.all_patches)
        for pn, commit in self.patches.items():
            if commit is None:
                assert pn in self.stack.patches
            else:
                assert pn in remaining

    def execute(
        self,
        msg,
        iw=None,
        set_head=True,
        allow_bad_head=False,
        print_current_patch=True,
    ):
        """Execute the transaction.

        Will either succeed, or fail (with an exception) and do nothing.

        """
        self._check_consistency()
        log_external_mods(self.stack)

        # Set branch head.
        if set_head:
            if iw:
                try:
                    self._checkout(self.head.data.tree, iw, allow_bad_head)
                except CheckoutException:
                    # We have to abort the transaction.
                    # The only state we need to restore is index+worktree.
                    self._checkout(self.stack.head.data.tree, iw, allow_bad_head=True)
                    raise TransactionAborted()
            self.stack.set_head(self.head, msg)

        if self._error:
            if self._conflicts:
                out.error(*([self._error] + self._conflicts))
            else:
                out.error(self._error)

        old_applied = self.stack.patchorder.applied
        if self._conflicts:
            msg = '%s (CONFLICT)' % (msg,)

        # Write patches.
        for pn, commit in self.patches.items():
            if pn in self.stack.patches:
                if commit is None:
                    self.stack.patches.delete(pn)
                else:
                    self.stack.patches.update(pn, commit, msg)
            else:
                self.stack.patches.new(pn, commit, msg)
        self.stack.patchorder.set_order(self._applied, self._unapplied, self._hidden)
        log_stack_state(self.stack, msg)

        if print_current_patch:
            _print_current_patch(old_applied, self._applied)

        if self._error:
            return utils.STGIT_CONFLICT
        else:
            return utils.STGIT_SUCCESS

    def _halt(self, msg):
        self._error = msg
        raise TransactionHalted(msg)

    @staticmethod
    def _print_popped(popped):
        if len(popped) == 0:
            pass
        elif len(popped) == 1:
            out.info('Popped %s' % popped[0])
        else:
            out.info('Popped %s -- %s' % (popped[-1], popped[0]))

    def pop_patches(self, p):
        """Pop all patches pn for which p(pn) is true.

        Always succeeds.

        :returns: list of other patches that had to be popped to accomplish this.

        """
        popped = []
        for i in range(len(self.applied)):
            if p(self.applied[i]):
                popped = self.applied[i:]
                del self.applied[i:]
                break
        popped1 = [pn for pn in popped if not p(pn)]
        popped2 = [pn for pn in popped if p(pn)]
        self.unapplied = popped1 + popped2 + self.unapplied
        self._print_popped(popped)
        return popped1

    def hide_patches(self, p, quiet=False):
        """Hide all patches pn for which p(pn) is true.

        Always succeeds.

        :returns: list of other patches that had to be popped to accomplish this.

        """
        popped = []
        all_patches = self.applied + self.unapplied + self.hidden
        for i in range(len(self.applied)):
            if p(self.applied[i]):
                popped = self.applied[i:]
                del self.applied[i:]
                break
        popped = [pn for pn in popped if not p(pn)]
        self._print_popped(popped)
        self.unapplied = popped + [pn for pn in self.unapplied if not p(pn)]
        new_hidden = []
        for pn in all_patches:
            if pn not in self.hidden and p(pn):
                new_hidden.append(pn)
                if not quiet:
                    s = ['', ' (empty)'][self.patches[pn].data.is_nochange()]
                    out.info('Hid %s%s' % (pn, s))
        self.hidden = new_hidden + self.hidden
        return popped

    def delete_patches(self, p, quiet=False):
        """Delete all patches pn for which p(pn) is true.

        Always succeeds.

        :returns: list of other patches that had to be popped to accomplish this.

        """
        popped = []
        all_patches = self.applied + self.unapplied + self.hidden
        for i in range(len(self.applied)):
            if p(self.applied[i]):
                popped = self.applied[i:]
                del self.applied[i:]
                break
        popped = [pn for pn in popped if not p(pn)]
        self.unapplied = popped + [pn for pn in self.unapplied if not p(pn)]
        self.hidden = [pn for pn in self.hidden if not p(pn)]
        self._print_popped(popped)
        for pn in all_patches:
            if p(pn):
                s = ['', ' (empty)'][self.patches[pn].data.is_nochange()]
                self.patches[pn] = None
                if not quiet:
                    out.info('Deleted %s%s' % (pn, s))
        return popped

    def push_patch(self, pn, iw=None, allow_interactive=False, already_merged=False):
        """Attempt to push the named patch.

        If this results in conflicts, the transaction is halted. If index+worktree are
        given, spill any conflicts to them.

        """
        out.start('Pushing patch "%s"' % pn)
        orig_cd = self.patches[pn].data
        cd = orig_cd.set_committer(None)
        oldparent = cd.parent
        cd = cd.set_parent(self.top)
        if already_merged:
            # the resulting patch is empty
            tree = cd.parent.data.tree
        else:
            base = oldparent.data.tree
            ours = cd.parent.data.tree
            theirs = cd.tree
            tree, self._temp_index_tree = self.temp_index.merge(
                base, ours, theirs, self._temp_index_tree
            )
        s = ''
        merge_conflict = False
        if not tree:
            if iw is None:
                self._halt('%s does not apply cleanly' % pn)
            try:
                self._checkout(ours, iw, allow_bad_head=False)
            except CheckoutException:
                self._halt('Index/worktree dirty')
            try:
                interactive = allow_interactive and config.getbool('stgit.autoimerge')
                iw.merge(base, ours, theirs, interactive=interactive)
                tree = iw.index.write_tree()
                self._current_tree = tree
                s = 'modified'
            except MergeConflictException as e:
                tree = ours
                merge_conflict = True
                self._conflicts = e.conflicts
                s = 'conflict'
            except MergeException as e:
                self._halt(str(e))
        cd = cd.set_tree(tree)
        if any(
            getattr(cd, a) != getattr(orig_cd, a)
            for a in ['parent', 'tree', 'author', 'message']
        ):
            comm = self.stack.repository.commit(cd)
            if merge_conflict:
                # When we produce a conflict, we'll run the update()
                # function defined below _after_ having done the
                # checkout in run(). To make sure that we check out
                # the real stack top (as it will look after update()
                # has been run), set it hard here.
                self.head = comm
        else:
            comm = None
            s = 'unmodified'
        if already_merged:
            s = 'merged'
        elif not merge_conflict and cd.is_nochange():
            s = 'empty'
        out.done(s)

        if merge_conflict:
            # We've just caused conflicts, so we must allow them in
            # the final checkout.
            self._allow_conflicts = lambda trans: True

        # Update the stack state
        if comm:
            self.patches[pn] = comm
        if pn in self.hidden:
            x = self.hidden
        else:
            x = self.unapplied
        del x[x.index(pn)]
        self.applied.append(pn)

        if merge_conflict:
            self._halt("%d merge conflict(s)" % len(self._conflicts))

    def push_tree(self, pn):
        """Push the named patch without updating its tree."""
        orig_cd = self.patches[pn].data
        cd = orig_cd.set_committer(None).set_parent(self.top)

        s = ''
        if any(
            getattr(cd, a) != getattr(orig_cd, a)
            for a in ['parent', 'tree', 'author', 'message']
        ):
            self.patches[pn] = self.stack.repository.commit(cd)
        else:
            s = ' (unmodified)'
        if cd.is_nochange():
            s = ' (empty)'
        out.info('Pushed %s%s' % (pn, s))

        if pn in self.hidden:
            x = self.hidden
        else:
            x = self.unapplied
        del x[x.index(pn)]
        self.applied.append(pn)

    def reorder_patches(
        self, applied, unapplied, hidden=None, iw=None, allow_interactive=False
    ):
        """Push and pop patches to attain the given ordering."""
        if hidden is None:
            hidden = self.hidden
        common = len(
            list(takewhile(lambda a: a[0] == a[1], zip(self.applied, applied)))
        )
        to_pop = set(self.applied[common:])
        self.pop_patches(lambda pn: pn in to_pop)
        for pn in applied[common:]:
            self.push_patch(pn, iw, allow_interactive=allow_interactive)

        # We only get here if all the pushes succeeded.
        assert self.applied == applied
        assert set(self.unapplied + self.hidden) == set(unapplied + hidden)
        self.unapplied = unapplied
        self.hidden = hidden

    def rename_patch(self, old_name, new_name):
        self.stack.rename_patch(old_name, new_name)
        self.patches[new_name] = self.patches.pop(old_name)
        try:
            index = self._applied.index(old_name)
        except ValueError:
            pass
        else:
            self._applied[index] = new_name
        try:
            index = self._unapplied.index(old_name)
        except ValueError:
            pass
        else:
            self._unapplied[index] = new_name
        try:
            index = self._hidden.index(old_name)
        except ValueError:
            pass
        else:
            self._hidden[index] = new_name

    def check_merged(self, patches, tree=None, quiet=False):
        """Return a subset of patches already merged."""
        if not quiet:
            out.start('Checking for patches merged upstream')
        merged = []
        if tree:
            self.temp_index.read_tree(tree)
            self._temp_index_tree = tree
        elif self._temp_index_tree != self.stack.head.data.tree:
            self.temp_index.read_tree(self.stack.head.data.tree)
            self._temp_index_tree = self.stack.head.data.tree
        for pn in reversed(patches):
            # check whether patch changes can be reversed in the current index
            cd = self.patches[pn].data
            if cd.is_nochange():
                continue
            try:
                self.temp_index.apply_treediff(
                    cd.tree,
                    cd.parent.data.tree,
                    quiet=True,
                )
                merged.append(pn)
                # The self.temp_index was modified by apply_treediff() so
                # force read_tree() the next time merge() is used.
                self._temp_index_tree = None
            except MergeException:
                pass
        if not quiet:
            out.done('%d found' % len(merged))
        return merged
