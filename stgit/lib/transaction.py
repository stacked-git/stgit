from stgit import exception, utils
from stgit.out import *
from stgit.lib import git

class TransactionException(exception.StgException):
    pass

class TransactionHalted(TransactionException):
    pass

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
    def __init__(self, stack):
        dict.__init__(self)
        self.__stack = stack
    def __getitem__(self, pn):
        try:
            return dict.__getitem__(self, pn)
        except KeyError:
            return self.__stack.patches.get(pn).commit

class StackTransaction(object):
    def __init__(self, stack, msg):
        self.__stack = stack
        self.__msg = msg
        self.__patches = _TransPatchMap(stack)
        self.__applied = list(self.__stack.patchorder.applied)
        self.__unapplied = list(self.__stack.patchorder.unapplied)
        self.__error = None
        self.__current_tree = self.__stack.head.data.tree
        self.__base = self.__stack.base
    stack = property(lambda self: self.__stack)
    patches = property(lambda self: self.__patches)
    def __set_applied(self, val):
        self.__applied = list(val)
    applied = property(lambda self: self.__applied, __set_applied)
    def __set_unapplied(self, val):
        self.__unapplied = list(val)
    unapplied = property(lambda self: self.__unapplied, __set_unapplied)
    def __set_base(self, val):
        assert (not self.__applied
                or self.patches[self.applied[0]].data.parent == val)
        self.__base = val
    base = property(lambda self: self.__base, __set_base)
    def __checkout(self, tree, iw):
        if not self.__stack.head_top_equal():
            out.error(
                'HEAD and top are not the same.',
                'This can happen if you modify a branch with git.',
                '"stg repair --help" explains more about what to do next.')
            self.__abort()
        if self.__current_tree != tree:
            assert iw != None
            iw.checkout(self.__current_tree, tree)
            self.__current_tree = tree
    @staticmethod
    def __abort():
        raise TransactionException(
            'Command aborted (all changes rolled back)')
    def __check_consistency(self):
        remaining = set(self.__applied + self.__unapplied)
        for pn, commit in self.__patches.iteritems():
            if commit == None:
                assert self.__stack.patches.exists(pn)
            else:
                assert pn in remaining
    @property
    def __head(self):
        if self.__applied:
            return self.__patches[self.__applied[-1]]
        else:
            return self.__base
    def abort(self, iw = None):
        # The only state we need to restore is index+worktree.
        if iw:
            self.__checkout(self.__stack.head.data.tree, iw)
    def run(self, iw = None):
        self.__check_consistency()
        new_head = self.__head

        # Set branch head.
        try:
            self.__checkout(new_head.data.tree, iw)
        except git.CheckoutException:
            # We have to abort the transaction.
            self.abort(iw)
            self.__abort()
        self.__stack.set_head(new_head, self.__msg)

        if self.__error:
            out.error(self.__error)

        # Write patches.
        for pn, commit in self.__patches.iteritems():
            if self.__stack.patches.exists(pn):
                p = self.__stack.patches.get(pn)
                if commit == None:
                    p.delete()
                else:
                    p.set_commit(commit, self.__msg)
            else:
                self.__stack.patches.new(pn, commit, self.__msg)
        _print_current_patch(self.__stack.patchorder.applied, self.__applied)
        self.__stack.patchorder.applied = self.__applied
        self.__stack.patchorder.unapplied = self.__unapplied

        if self.__error:
            return utils.STGIT_CONFLICT
        else:
            return utils.STGIT_SUCCESS

    def __halt(self, msg):
        self.__error = msg
        raise TransactionHalted(msg)

    @staticmethod
    def __print_popped(popped):
        if len(popped) == 0:
            pass
        elif len(popped) == 1:
            out.info('Popped %s' % popped[0])
        else:
            out.info('Popped %s -- %s' % (popped[-1], popped[0]))

    def pop_patches(self, p):
        """Pop all patches pn for which p(pn) is true. Return the list of
        other patches that had to be popped to accomplish this."""
        popped = []
        for i in xrange(len(self.applied)):
            if p(self.applied[i]):
                popped = self.applied[i:]
                del self.applied[i:]
                break
        popped1 = [pn for pn in popped if not p(pn)]
        popped2 = [pn for pn in popped if p(pn)]
        self.unapplied = popped1 + popped2 + self.unapplied
        self.__print_popped(popped)
        return popped1

    def delete_patches(self, p):
        """Delete all patches pn for which p(pn) is true. Return the list of
        other patches that had to be popped to accomplish this."""
        popped = []
        all_patches = self.applied + self.unapplied
        for i in xrange(len(self.applied)):
            if p(self.applied[i]):
                popped = self.applied[i:]
                del self.applied[i:]
                break
        popped = [pn for pn in popped if not p(pn)]
        self.unapplied = popped + [pn for pn in self.unapplied if not p(pn)]
        self.__print_popped(popped)
        for pn in all_patches:
            if p(pn):
                s = ['', ' (empty)'][self.patches[pn].data.is_nochange()]
                self.patches[pn] = None
                out.info('Deleted %s%s' % (pn, s))
        return popped

    def push_patch(self, pn, iw = None):
        """Attempt to push the named patch. If this results in conflicts,
        halts the transaction. If index+worktree are given, spill any
        conflicts to them."""
        cd = self.patches[pn].data
        cd = cd.set_committer(None)
        s = ['', ' (empty)'][cd.is_nochange()]
        oldparent = cd.parent
        cd = cd.set_parent(self.__head)
        base = oldparent.data.tree
        ours = cd.parent.data.tree
        theirs = cd.tree
        tree = self.__stack.repository.simple_merge(base, ours, theirs)
        merge_conflict = False
        if not tree:
            if iw == None:
                self.__halt('%s does not apply cleanly' % pn)
            try:
                self.__checkout(ours, iw)
            except git.CheckoutException:
                self.__halt('Index/worktree dirty')
            try:
                iw.merge(base, ours, theirs)
                tree = iw.index.write_tree()
                self.__current_tree = tree
                s = ' (modified)'
            except git.MergeException:
                tree = ours
                merge_conflict = True
                s = ' (conflict)'
        cd = cd.set_tree(tree)
        self.patches[pn] = self.__stack.repository.commit(cd)
        del self.unapplied[self.unapplied.index(pn)]
        self.applied.append(pn)
        out.info('Pushed %s%s' % (pn, s))
        if merge_conflict:
            self.__halt('Merge conflict')
