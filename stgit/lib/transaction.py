from stgit import exception
from stgit.out import *

class TransactionException(exception.StgException):
    pass

def print_current_patch(old_applied, new_applied):
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

class StackTransaction(object):
    def __init__(self, stack, msg):
        self.__stack = stack
        self.__msg = msg
        self.__patches = {}
        self.__applied = list(self.__stack.patchorder.applied)
        self.__unapplied = list(self.__stack.patchorder.unapplied)
    def __set_patches(self, val):
        self.__patches = dict(val)
    patches = property(lambda self: self.__patches, __set_patches)
    def __set_applied(self, val):
        self.__applied = list(val)
    applied = property(lambda self: self.__applied, __set_applied)
    def __set_unapplied(self, val):
        self.__unapplied = list(val)
    unapplied = property(lambda self: self.__unapplied, __set_unapplied)
    def __check_consistency(self):
        remaining = set(self.__applied + self.__unapplied)
        for pn, commit in self.__patches.iteritems():
            if commit == None:
                assert self.__stack.patches.exists(pn)
            else:
                assert pn in remaining
    def run(self):
        self.__check_consistency()

        # Get new head commit.
        if self.__applied:
            top_patch = self.__applied[-1]
            try:
                new_head = self.__patches[top_patch]
            except KeyError:
                new_head = self.__stack.patches.get(top_patch).commit
        else:
            new_head = self.__stack.base

        # Set branch head.
        if new_head == self.__stack.head:
            pass # same commit: OK
        elif new_head.data.tree == self.__stack.head.data.tree:
            pass # same tree: OK
        else:
            # We can't handle this case yet.
            raise TransactionException('Error: HEAD tree changed')
        self.__stack.set_head(new_head, self.__msg)

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
        print_current_patch(self.__stack.patchorder.applied, self.__applied)
        self.__stack.patchorder.applied = self.__applied
        self.__stack.patchorder.unapplied = self.__unapplied
