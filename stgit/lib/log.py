"""Functions for managing stack state log.

A stack log is a Git branch. Each commit contains the complete state (metadata) of the
stack at the moment it was written; the most recent commit has the most recent state.

For a branch `foo`, the stack state log is stored in the branch `foo.stgit`.

Each log entry makes sure to have proper references to everything it needs to make it
safe against garbage collection. The stack state log can even be pulled from one
repository to another.

Format version 0
================

Version 0 was an experimental version of the stack log format; it is no longer
supported.

Format version 1
================

Commit message
--------------

The commit message is mostly for human consumption; in most cases it is just a subject
line: the StGit subcommand name and possibly some important command-line flag.

An exception to this is commits for ``undo`` and ``redo``. Their subject line is "undo
`n`" and "redo `n`"; the positive integer `n` indicates how many steps were undone or
redone.

Tree
----

* One blob, ``meta``, that contains the stack state:

   * ``Version: 1``

     Future versions of StGit might change the log format; when this is done, this
     version number will be incremented.

   * ``Previous: <sha1 or None>``

     The commit of the previous log entry, or None if this is the first entry.

   * ``Head: <sha1>``

     The current branch head.

   * ``Applied:``

     Marks the start of the list of applied patches. Each patch is listed in order, one
     per line. First one or more spaces, then the patch name, then a colon, space, then
     the patch's sha1.

   * ``Unapplied:``

     Same as ``Applied:``, but for the unapplied patches.

   * ``Hidden:``

     Same as ``Applied:``, but for the hidden patches.

* One subtree, ``patches``, that contains one blob per patch::

   Bottom: <sha1 of patch's bottom tree>
   Top:    <sha1 of patch's top tree>
   Author: <author name and e-mail>
   Date:   <patch timestamp>

   <commit message>

Parents
-------

* The first parent is the *simplified log*, described below.

* The rest of the parents are just there to make sure that all the commits referred to
  in the log entry--patches, branch head, previous log entry--are ancestors of the log
  commit. This is necessary to make the log safe with regard to garbage collection and
  pulling.

Simplified log
--------------

The simplified log is exactly like the full log, except that its only parent is the
(simplified) previous log entry, if any. It's purpose is mainly ease of visualization
in, for example, ``git log --graph`` or ``gitk``.

Format version 4
================

The metadata in the `<branch>.stgit` branch is the same as format version 1.

Format version 4 indicates that, unlike previous format versions used by older versions
of StGit, the stack log state is *only* contained in the stack log branch and *not* as
files in the .git/patches directory.

"""

import re

from stgit import utils
from stgit.exception import StgException
from stgit.lib.git import BlobData, CommitData, TreeData
from stgit.lib.stackupgrade import FORMAT_VERSION
from stgit.out import out


class LogException(StgException):
    pass


class LogParseException(LogException):
    pass


class StackState:
    _max_parents = 16

    def __init__(
        self,
        prev,
        head,
        applied,
        unapplied,
        hidden,
        patches,
        commit=None,
    ):
        self.prev = prev
        self.head = head
        self.applied = applied
        self.unapplied = unapplied
        self.hidden = hidden
        self.patches = patches
        self.commit = commit

    @property
    def simplified_parent(self):
        return self.commit.data.parents[0]

    def get_prev_state(self, repo):
        if self.prev is None:
            return None
        else:
            return self.from_commit(repo, self.prev)

    @property
    def base(self):
        if self.applied:
            return self.patches[self.applied[0]].data.parent
        else:
            return self.head

    @property
    def top(self):
        if self.applied:
            return self.patches[self.applied[-1]]
        else:
            return self.head

    @property
    def all_patches(self):
        return self.applied + self.unapplied + self.hidden

    @classmethod
    def new_empty(cls, head):
        return cls(
            prev=None,
            head=head,
            applied=[],
            unapplied=[],
            hidden=[],
            patches={},
        )

    @classmethod
    def from_stack(cls, prev, stack):
        return cls(
            prev=prev,
            head=stack.head,
            applied=list(stack.patchorder.applied),
            unapplied=list(stack.patchorder.unapplied),
            hidden=list(stack.patchorder.hidden),
            patches={pn: stack.patches.get(pn).commit for pn in stack.patchorder.all},
        )

    @staticmethod
    def _parse_metadata(repo, metadata):
        """Parse a stack log metadata string."""
        if not metadata.startswith('Version:'):
            raise LogParseException('Malformed log metadata')
        metadata = metadata.splitlines()
        version_str = utils.strip_prefix('Version:', metadata.pop(0)).strip()
        try:
            version = int(version_str)
        except ValueError:
            raise LogParseException('Malformed version number: %r' % version_str)
        if version < FORMAT_VERSION:
            raise LogException('Log is version %d, which is too old' % version)
        if version > FORMAT_VERSION:
            raise LogException('Log is version %d, which is too new' % version)
        parsed = {}
        key = None
        for line in metadata:
            if line.startswith(' '):
                assert key is not None
                parsed[key].append(line.strip())
            else:
                key, val = [x.strip() for x in line.split(':', 1)]
                if val:
                    parsed[key] = val
                else:
                    parsed[key] = []
        prev = parsed['Previous']
        if prev == 'None':
            prev = None
        else:
            prev = repo.get_commit(prev)
        head = repo.get_commit(parsed['Head'])
        lists = {'Applied': [], 'Unapplied': [], 'Hidden': []}
        patches = {}
        for lst in lists:
            for entry in parsed[lst]:
                pn, sha1 = [x.strip() for x in entry.split(':')]
                lists[lst].append(pn)
                patches[pn] = repo.get_commit(sha1)
        return (
            prev,
            head,
            lists['Applied'],
            lists['Unapplied'],
            lists['Hidden'],
            patches,
        )

    @classmethod
    def from_commit(cls, repo, commit):
        """Parse a (full or simplified) stack log commit."""
        try:
            perm, meta_blob = commit.data.tree.data['meta']
        except KeyError:
            raise LogParseException('Not a stack log')

        prev, head, applied, unapplied, hidden, patches = cls._parse_metadata(
            repo, meta_blob.data.bytes.decode('utf-8')
        )

        return cls(prev, head, applied, unapplied, hidden, patches, commit)

    def _parents(self, prev_state):
        """Parents this entry needs to be a descendant of all commits it refers to."""
        xp = {self.head, self.top}
        xp |= {self.patches[pn] for pn in self.unapplied}
        xp |= {self.patches[pn] for pn in self.hidden}
        if prev_state is not None:
            xp.add(prev_state.commit)
            xp -= set(prev_state.patches.values())
        return xp

    def _metadata_blob(self, repo, prev_state):
        lines = ['Version: %d' % FORMAT_VERSION]
        lines.append(
            'Previous: %s' % ('None' if prev_state is None else prev_state.commit.sha1)
        )
        lines.append('Head: %s' % self.head.sha1)
        for patch_list, title in [
            (self.applied, 'Applied'),
            (self.unapplied, 'Unapplied'),
            (self.hidden, 'Hidden'),
        ]:
            lines.append('%s:' % title)
            for pn in patch_list:
                lines.append('  %s: %s' % (pn, self.patches[pn].sha1))
        lines.append('')
        metadata_str = '\n'.join(lines)
        return repo.commit(BlobData(metadata_str.encode('utf-8')))

    def _patch_blob(self, repo, pn, commit, prev_state):
        if prev_state is not None:
            perm, prev_patch_tree = prev_state.commit.data.tree.data['patches']
            if pn in prev_patch_tree.data and commit == prev_state.patches[pn]:
                return prev_patch_tree.data[pn]

        patch_meta = '\n'.join(
            [
                'Bottom: %s' % commit.data.parent.data.tree.sha1,
                'Top:    %s' % commit.data.tree.sha1,
                'Author: %s' % commit.data.author.name_email,
                'Date:   %s' % commit.data.author.date,
                '',
                commit.data.message_str,
            ]
        )
        return repo.commit(BlobData(patch_meta.encode('utf-8')))

    def _patches_tree(self, repo, prev_state):
        return repo.commit(
            TreeData(
                {
                    pn: self._patch_blob(repo, pn, commit, prev_state)
                    for pn, commit in self.patches.items()
                }
            )
        )

    def _tree(self, repo, prev_state):
        return repo.commit(
            TreeData(
                {
                    'meta': self._metadata_blob(repo, prev_state),
                    'patches': self._patches_tree(repo, prev_state),
                }
            )
        )

    def commit_state(self, repo, message):
        """Commit stack state to stack metadata branch."""
        prev_state = self.get_prev_state(repo)
        tree = self._tree(repo, prev_state)
        simplified_parent = repo.commit(
            CommitData(
                tree=tree,
                message=message,
                parents=[] if prev_state is None else [prev_state.simplified_parent],
            )
        )
        parents = list(self._parents(prev_state))
        while len(parents) >= self._max_parents:
            g = repo.commit(
                CommitData(
                    tree=tree,
                    parents=parents[-self._max_parents :],
                    message='Stack log parent grouping',
                )
            )
            parents[-self._max_parents :] = [g]
        self.commit = repo.commit(
            CommitData(
                tree=tree,
                message=message,
                parents=[simplified_parent] + parents,
            )
        )
        return self.commit

    def same_state(self, other):
        """Check whether two stack state entries describe the same stack state."""
        return (
            self.head == other.head
            and self.applied == other.applied
            and self.unapplied == other.unapplied
            and self.hidden == other.hidden
            and self.patches == other.patches
        )


def get_stack_state(repo, ref, commit=None):
    if commit is None:
        commit = repo.refs.get(ref)  # May raise KeyError
    try:
        return StackState.from_commit(repo, commit)
    except LogException as e:
        raise LogException('While reading log from %s: %s' % (ref, e))


def log_stack_state(stack, msg):
    """Write a new metadata entry for the stack."""
    try:
        prev_state = get_stack_state(stack.repository, stack.state_ref)
    except KeyError:
        prev_state = None

    try:
        new_state = StackState.from_stack(prev_state.commit, stack)
    except LogException as e:
        out.warn(str(e), 'No log entry written.')
    else:
        if not prev_state or not new_state.same_state(prev_state):
            state_commit = new_state.commit_state(stack.repository, msg)
            stack.repository.refs.set(stack.state_ref, state_commit, msg)


def reset_stack(trans, iw, state):
    """Reset the stack to a given previous state."""
    for pn in trans.all_patches:
        trans.patches[pn] = None
    for pn in state.all_patches:
        trans.patches[pn] = state.patches[pn]
    trans.applied = state.applied
    trans.unapplied = state.unapplied
    trans.hidden = state.hidden
    trans.base = state.base
    trans.head = state.head


def reset_stack_partially(trans, iw, state, only_patches):
    """Reset the stack to a previous state, but only for the given patches.

    :param only_patches: Touch only these patches
    :type only_patches: iterable

    """
    only_patches = set(only_patches)
    patches_to_reset = set(state.all_patches) & only_patches
    existing_patches = set(trans.all_patches)
    original_applied_order = list(trans.applied)
    to_delete = (existing_patches - patches_to_reset) & only_patches

    # In one go, do all the popping we have to in order to pop the
    # patches we're going to delete or modify.
    def mod(pn):
        if pn not in only_patches:
            return False
        if pn in to_delete:
            return True
        if trans.patches[pn] != state.patches.get(pn, None):
            return True
        return False

    trans.pop_patches(mod)

    # Delete and modify/create patches. We've previously popped all
    # patches that we touch in this step.
    trans.delete_patches(lambda pn: pn in to_delete)
    for pn in patches_to_reset:
        if pn in existing_patches:
            if trans.patches[pn] == state.patches[pn]:
                continue
            else:
                out.info('Resetting %s' % pn)
        else:
            if pn in state.hidden:
                trans.hidden.append(pn)
            else:
                trans.unapplied.append(pn)
            out.info('Resurrecting %s' % pn)
        trans.patches[pn] = state.patches[pn]

    # Push all the patches that we've popped, if they still
    # exist.
    pushable = set(trans.unapplied + trans.hidden)
    for pn in original_applied_order:
        if pn in pushable:
            trans.push_patch(pn, iw)


def undo_state(stack, undo_steps):
    """Find the stack state ``undo_steps`` steps in the past.

    Successive undo operations are supposed to "add up", so if we find other undo
    operations along the way we have to add those undo steps to ``undo_steps``.

    If ``undo_steps`` is negative, redo instead of undo.

    :returns: stack state that is the destination of the undo operation
    :rtype: :class:`StackState`

    """
    try:
        state = get_stack_state(stack.repository, stack.state_ref)
    except KeyError:
        raise LogException('Log is empty')
    while undo_steps != 0:
        msg = state.commit.data.message_str.strip()
        um = re.match(r'^undo\s+(\d+)$', msg)
        if undo_steps > 0:
            if um:
                undo_steps += int(um.group(1))
            else:
                undo_steps -= 1
        else:
            rm = re.match(r'^redo\s+(\d+)$', msg)
            if um:
                undo_steps += 1
            elif rm:
                undo_steps -= int(rm.group(1))
            else:
                raise LogException('No more redo information available')
        prev_state = state.get_prev_state(stack.repository)
        if prev_state is None:
            raise LogException('Not enough undo information available')
        state = prev_state
    return state


def log_external_mods(stack):
    try:
        state = get_stack_state(stack.repository, stack.state_ref)
    except KeyError:
        # No log exists yet.
        log_stack_state(stack, 'start of log')
        return
    except LogException:
        # Something's wrong with the log, so don't bother.
        return
    if state.head == stack.head:
        # No external modifications.
        return
    log_stack_state(
        stack,
        'external modifications\n\n'
        'Modifications by tools other than StGit (e.g. git).\n',
    )
