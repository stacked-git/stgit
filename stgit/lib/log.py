r"""This module contains functions and classes for manipulating
I{patch stack logs} (or just I{stack logs}).

A stack log is a git branch. Each commit contains the complete state
of the stack at the moment it was written; the most recent commit has
the most recent state.

For a branch C{I{foo}}, the stack log is stored in C{I{foo}.stgit}.
Each log entry makes sure to have proper references to everything it
needs, which means that it is safe against garbage collection -- you
can even pull it from one repository to another.

Stack log format (version 0)
============================

Version 0 was an experimental version of the stack log format; it is
no longer supported.

Stack log format (version 1)
============================

Commit message
--------------

The commit message is mostly for human consumption; in most cases it
is just a subject line: the stg subcommand name and possibly some
important command-line flag.

An exception to this is log commits for undo and redo. Their subject
line is "C{undo I{n}}" and "C{redo I{n}}"; the positive integer I{n}
says how many steps were undone or redone.

Tree
----

  - One blob, C{meta}, that contains the log data:

      - C{Version:} I{n}

        where I{n} must be 1. (Future versions of StGit might change
        the log format; when this is done, this version number will be
        incremented.)

      - C{Previous:} I{sha1 or C{None}}

        The commit of the previous log entry, or C{None} if this is
        the first entry.

      - C{Head:} I{sha1}

        The current branch head.

      - C{Applied:}

        Marks the start of the list of applied patches. They are
        listed in order, each on its own line: first one or more
        spaces, then the patch name, then a colon, space, then the
        patch's sha1.

      - C{Unapplied:}

        Same as C{Applied:}, but for the unapplied patches.

      - C{Hidden:}

        Same as C{Applied:}, but for the hidden patches.

  - One subtree, C{patches}, that contains one blob per patch::

      Bottom: <sha1 of patch's bottom tree>
      Top:    <sha1 of patch's top tree>
      Author: <author name and e-mail>
      Date:   <patch timestamp>

      <commit message>

      ---

      <patch diff>

Following the message is a newline, three dashes, and another newline.
Then come, each on its own line,

Parents
-------

  - The first parent is the I{simplified log}, described below.

  - The rest of the parents are just there to make sure that all the
    commits referred to in the log entry -- patches, branch head,
    previous log entry -- are ancestors of the log commit. (This is
    necessary to make the log safe with regard to garbage collection
    and pulling.)

Simplified log
--------------

The simplified log is exactly like the full log, except that its only
parent is the (simplified) previous log entry, if any. It's purpose is
mainly ease of visualization."""

import re
from stgit.lib import git, stack as libstack
from stgit import exception, utils
from stgit.out import out
import StringIO

class LogException(exception.StgException):
    pass

class LogParseException(LogException):
    pass

def patch_file(repo, cd):
    return repo.commit(git.BlobData(''.join(s + '\n' for s in [
                    'Bottom: %s' % cd.parent.data.tree.sha1,
                    'Top:    %s' % cd.tree.sha1,
                    'Author: %s' % cd.author.name_email,
                    'Date:   %s' % cd.author.date,
                    '',
                    cd.message,
                    '',
                    '---',
                    '',
                    repo.diff_tree(cd.parent.data.tree, cd.tree, ['-M']
                                   ).strip()])))

def log_ref(branch):
    return 'refs/heads/%s.stgit' % branch

class LogEntry(object):
    __separator = '\n---\n'
    __max_parents = 16
    def __init__(self, repo, prev, head, applied, unapplied, hidden,
                 patches, message):
        self.__repo = repo
        self.__prev = prev
        self.__simplified = None
        self.head = head
        self.applied = applied
        self.unapplied = unapplied
        self.hidden = hidden
        self.patches = patches
        self.message = message
    @property
    def simplified(self):
        if not self.__simplified:
            self.__simplified = self.commit.data.parents[0]
        return self.__simplified
    @property
    def prev(self):
        if self.__prev != None and not isinstance(self.__prev, LogEntry):
            self.__prev = self.from_commit(self.__repo, self.__prev)
        return self.__prev
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
    all_patches = property(lambda self: (self.applied + self.unapplied
                                         + self.hidden))
    @classmethod
    def from_stack(cls, prev, stack, message):
        return cls(
            repo = stack.repository,
            prev = prev,
            head = stack.head,
            applied = list(stack.patchorder.applied),
            unapplied = list(stack.patchorder.unapplied),
            hidden = list(stack.patchorder.hidden),
            patches = dict((pn, stack.patches.get(pn).commit)
                           for pn in stack.patchorder.all),
            message = message)
    @staticmethod
    def __parse_metadata(repo, metadata):
        """Parse a stack log metadata string."""
        if not metadata.startswith('Version:'):
            raise LogParseException('Malformed log metadata')
        metadata = metadata.splitlines()
        version_str = utils.strip_prefix('Version:', metadata.pop(0)).strip()
        try:
            version = int(version_str)
        except ValueError:
            raise LogParseException(
                'Malformed version number: %r' % version_str)
        if version < 1:
            raise LogException('Log is version %d, which is too old' % version)
        if version > 1:
            raise LogException('Log is version %d, which is too new' % version)
        parsed = {}
        for line in metadata:
            if line.startswith(' '):
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
        lists = { 'Applied': [], 'Unapplied': [], 'Hidden': [] }
        patches = {}
        for lst in lists.keys():
            for entry in parsed[lst]:
                pn, sha1 = [x.strip() for x in entry.split(':')]
                lists[lst].append(pn)
                patches[pn] = repo.get_commit(sha1)
        return (prev, head, lists['Applied'], lists['Unapplied'],
                lists['Hidden'], patches)
    @classmethod
    def from_commit(cls, repo, commit):
        """Parse a (full or simplified) stack log commit."""
        message = commit.data.message
        try:
            perm, meta = commit.data.tree.data.entries['meta']
        except KeyError:
            raise LogParseException('Not a stack log')
        (prev, head, applied, unapplied, hidden, patches
         ) = cls.__parse_metadata(repo, meta.data.str)
        lg = cls(repo, prev, head, applied, unapplied, hidden, patches, message)
        lg.commit = commit
        return lg
    def __metadata_string(self):
        e = StringIO.StringIO()
        e.write('Version: 1\n')
        if self.prev == None:
            e.write('Previous: None\n')
        else:
            e.write('Previous: %s\n' % self.prev.commit.sha1)
        e.write('Head: %s\n' % self.head.sha1)
        for lst, title in [(self.applied, 'Applied'),
                           (self.unapplied, 'Unapplied'),
                           (self.hidden, 'Hidden')]:
            e.write('%s:\n' % title)
            for pn in lst:
                e.write('  %s: %s\n' % (pn, self.patches[pn].sha1))
        return e.getvalue()
    def __parents(self):
        """Return the set of parents this log entry needs in order to be a
        descendant of all the commits it refers to."""
        xp = set([self.head]) | set(self.patches[pn]
                                    for pn in self.unapplied + self.hidden)
        if self.applied:
            xp.add(self.patches[self.applied[-1]])
        if self.prev != None:
            xp.add(self.prev.commit)
            xp -= set(self.prev.patches.values())
        return xp
    def __tree(self, metadata):
        if self.prev == None:
            def pf(c):
                return patch_file(self.__repo, c.data)
        else:
            prev_top_tree = self.prev.commit.data.tree
            perm, prev_patch_tree = prev_top_tree.data.entries['patches']
            # Map from Commit object to patch_file() results taken
            # from the previous log entry.
            c2b = dict((self.prev.patches[pn], pf) for pn, pf
                       in prev_patch_tree.data.entries.iteritems())
            def pf(c):
                r = c2b.get(c, None)
                if not r:
                    r = patch_file(self.__repo, c.data)
                return r
        patches = dict((pn, pf(c)) for pn, c in self.patches.iteritems())
        return self.__repo.commit(git.TreeData({
                    'meta': self.__repo.commit(git.BlobData(metadata)),
                    'patches': self.__repo.commit(git.TreeData(patches)) }))
    def write_commit(self):
        metadata = self.__metadata_string()
        tree = self.__tree(metadata)
        self.__simplified = self.__repo.commit(git.CommitData(
                tree = tree, message = self.message,
                parents = [prev.simplified for prev in [self.prev]
                           if prev != None]))
        parents = list(self.__parents())
        while len(parents) >= self.__max_parents:
            g = self.__repo.commit(git.CommitData(
                    tree = tree, parents = parents[-self.__max_parents:],
                    message = 'Stack log parent grouping'))
            parents[-self.__max_parents:] = [g]
        self.commit = self.__repo.commit(git.CommitData(
                tree = tree, message = self.message,
                parents = [self.simplified] + parents))

def get_log_entry(repo, ref, commit):
    try:
        return LogEntry.from_commit(repo, commit)
    except LogException, e:
        raise LogException('While reading log from %s: %s' % (ref, e))

def same_state(log1, log2):
    """Check whether two log entries describe the same current state."""
    s = [[lg.head, lg.applied, lg.unapplied, lg.hidden, lg.patches]
         for lg in [log1, log2]]
    return s[0] == s[1]

def log_entry(stack, msg):
    """Write a new log entry for the stack."""
    ref = log_ref(stack.name)
    try:
        last_log_commit = stack.repository.refs.get(ref)
    except KeyError:
        last_log_commit = None
    try:
        if last_log_commit:
            last_log = get_log_entry(stack.repository, ref, last_log_commit)
        else:
            last_log = None
        new_log = LogEntry.from_stack(last_log, stack, msg)
    except LogException, e:
        out.warn(str(e), 'No log entry written.')
        return
    if last_log and same_state(last_log, new_log):
        return
    new_log.write_commit()
    stack.repository.refs.set(ref, new_log.commit, msg)

class Fakestack(object):
    """Imitates a real L{Stack<stgit.lib.stack.Stack>}, but with the
    topmost patch popped."""
    def __init__(self, stack):
        appl = list(stack.patchorder.applied)
        unappl = list(stack.patchorder.unapplied)
        hidd = list(stack.patchorder.hidden)
        class patchorder(object):
            applied = appl[:-1]
            unapplied = [appl[-1]] + unappl
            hidden = hidd
            all = appl + unappl + hidd
        self.patchorder = patchorder
        class patches(object):
            @staticmethod
            def get(pn):
                if pn == appl[-1]:
                    class patch(object):
                        commit = stack.patches.get(pn).old_commit
                    return patch
                else:
                    return stack.patches.get(pn)
        self.patches = patches
        self.head = stack.head.data.parent
        self.top = stack.top.data.parent
        self.base = stack.base
        self.name = stack.name
        self.repository = stack.repository
def compat_log_entry(msg):
    """Write a new log entry. (Convenience function intended for use by
    code not yet converted to the new infrastructure.)"""
    try:
        repo = default_repo()
        stack = repo.get_stack(repo.current_branch_name)
    except (libstack.StackException, git.RepositoryException), e:
        out.warn(str(e), 'Could not write to stack log')
    else:
        if repo.default_index.conflicts() and stack.patchorder.applied:
            log_entry(Fakestack(stack), msg)
            log_entry(stack, msg + ' (CONFLICT)')
        else:
            log_entry(stack, msg)

def delete_log(repo, branch):
    ref = log_ref(branch)
    if repo.refs.exists(ref):
        repo.refs.delete(ref)

def rename_log(repo, old_branch, new_branch, msg):
    old_ref = log_ref(old_branch)
    new_ref = log_ref(new_branch)
    if repo.refs.exists(old_ref):
        repo.refs.set(new_ref, repo.refs.get(old_ref), msg)
        repo.refs.delete(old_ref)

def copy_log(repo, src_branch, dst_branch, msg):
    src_ref = log_ref(src_branch)
    dst_ref = log_ref(dst_branch)
    if repo.refs.exists(src_ref):
        repo.refs.set(dst_ref, repo.refs.get(src_ref), msg)

def default_repo():
    return libstack.Repository.default()

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
    """Reset the stack to a given previous state -- but only the given
    patches, not anything else.

    @param only_patches: Touch only these patches
    @type only_patches: iterable"""
    only_patches = set(only_patches)
    patches_to_reset = set(state.all_patches) & only_patches
    existing_patches = set(trans.all_patches)
    original_applied_order = list(trans.applied)
    to_delete = (existing_patches - patches_to_reset) & only_patches

    # In one go, do all the popping we have to in order to pop the
    # patches we're going to delete or modify.
    def mod(pn):
        if not pn in only_patches:
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
    """Find the log entry C{undo_steps} steps in the past. (Successive
    undo operations are supposed to "add up", so if we find other undo
    operations along the way we have to add those undo steps to
    C{undo_steps}.)

    If C{undo_steps} is negative, redo instead of undo.

    @return: The log entry that is the destination of the undo
             operation
    @rtype: L{LogEntry}"""
    ref = log_ref(stack.name)
    try:
        commit = stack.repository.refs.get(ref)
    except KeyError:
        raise LogException('Log is empty')
    log = get_log_entry(stack.repository, ref, commit)
    while undo_steps != 0:
        msg = log.message.strip()
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
        if not log.prev:
            raise LogException('Not enough undo information available')
        log = log.prev
    return log

def log_external_mods(stack):
    ref = log_ref(stack.name)
    try:
        log_commit = stack.repository.refs.get(ref)
    except KeyError:
        # No log exists yet.
        log_entry(stack, 'start of log')
        return
    try:
        log = get_log_entry(stack.repository, ref, log_commit)
    except LogException:
        # Something's wrong with the log, so don't bother.
        return
    if log.head == stack.head:
        # No external modifications.
        return
    log_entry(stack, '\n'.join([
                'external modifications', '',
                'Modifications by tools other than StGit (e.g. git).']))

def compat_log_external_mods():
    try:
        repo = default_repo()
    except git.RepositoryException:
        # No repository, so we couldn't log even if we wanted to.
        return
    try:
        stack = repo.get_stack(repo.current_branch_name)
    except exception.StgException:
        # Stack doesn't exist, so we can't log.
        return
    log_external_mods(stack)
