"""Function/variables common to all the commands"""

import email.utils
import os
import re
import sys

from stgit import templates
from stgit.compat import decode_utf8_with_latin1
from stgit.config import config
from stgit.exception import StgException
from stgit.lib.git import CommitData, MergeException, RepositoryException
from stgit.lib.stack import StackRepository
from stgit.lib.transaction import (
    StackTransaction,
    TransactionException,
    TransactionHalted,
)
from stgit.out import out
from stgit.run import Run, RunException
from stgit.utils import (
    EditorException,
    add_trailer,
    edit_bytes,
    get_hook,
    run_hook_on_bytes,
    strip_prefix,
)

__copyright__ = """
Copyright (C) 2005, Catalin Marinas <catalin.marinas@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see http://www.gnu.org/licenses/.
"""


class CmdException(StgException):
    pass


def parse_rev(rev):
    """Parse a revision specification into its branch:patch parts."""
    try:
        branch, patch = rev.split(':', 1)
    except ValueError:
        branch = None
        patch = rev

    return (branch, patch)


def git_commit(name, repository, branch_name=None):
    """Return the a Commit object if 'name' is a patch name or Git commit.

    The patch names allowed are in the form '<branch>:<patch>' and can be followed by
    standard symbols used by ``git rev-parse``. If <patch> is '{base}', it represents
    the bottom of the stack.

    """
    # Try a [branch:]patch name first
    branch, patch = parse_rev(name)
    if not branch:
        branch = branch_name or repository.current_branch_name

    # The stack base
    if patch.startswith('{base}'):
        base_id = repository.get_stack(branch).base.sha1
        return repository.rev_parse(base_id + strip_prefix('{base}', patch))

    # Other combination of branch and patch
    try:
        return repository.rev_parse(
            'patches/%s/%s' % (branch, patch), discard_stderr=True
        )
    except RepositoryException:
        pass

    # Try a Git commit
    try:
        return repository.rev_parse(name, discard_stderr=True)
    except RepositoryException:
        raise CmdException('%s: Unknown patch or revision name' % name)


def color_diff_flags():
    """Return the Git flags for coloured diff output if allowed."""
    stdout_is_tty = (sys.stdout.isatty() and 'true') or 'false'
    if config.get_colorbool('color.diff', stdout_is_tty) == 'true':
        return ['--color']
    else:
        return []


def check_local_changes(repository):
    out.start('Checking for changes in the working directory')
    iw = repository.default_iw
    iw.refresh_index()
    tree = repository.refs.get(repository.head_ref).data.tree
    local_changes = iw.changed_files(tree)
    out.done()
    if local_changes:
        raise CmdException('local changes in the tree. Use "refresh" or "reset --hard"')


def check_head_top_equal(stack):
    if stack.head != stack.top:
        raise CmdException(
            'HEAD and top are not the same. This can happen if you modify a '
            'branch with git. "stg repair --help" explains more about what to '
            'do next.'
        )


def check_conflicts(iw):
    if iw.index.conflicts():
        raise CmdException(
            'Unsolved conflicts. Please fix the conflicts then use "git add '
            '--update <files>" or revert the changes with "reset --hard".'
        )


def print_current_patch(stack):
    if stack.patchorder.applied:
        out.info('Now at patch "%s"' % stack.patchorder.applied[-1])
    else:
        out.info('No patches applied')


def parse_patches(patch_args, patch_list, boundary=0, ordered=False):
    """Parse patch_args list for patch names in patch_list.

    The names can be individual patches and/or in the 'patch1..patch2' format.

    """
    # in case it receives a tuple
    patch_list = list(patch_list)
    patches = []

    for name in patch_args:
        pair = name.split('..')
        for p in pair:
            if p and p not in patch_list:
                raise CmdException('Unknown patch name: %s' % p)

        if len(pair) == 1:
            # single patch name
            pl = pair
        elif len(pair) == 2:
            # patch range [p1]..[p2]
            # inclusive boundary
            if pair[0]:
                first = patch_list.index(pair[0])
            else:
                first = -1
            # exclusive boundary
            if pair[1]:
                last = patch_list.index(pair[1]) + 1
            else:
                last = -1

            # only cross the boundary if explicitly asked
            if not boundary:
                boundary = len(patch_list)
            if first < 0:
                if last <= boundary:
                    first = 0
                else:
                    first = boundary
            if last < 0:
                if first < boundary:
                    last = boundary
                else:
                    last = len(patch_list)

            if last > first:
                pl = patch_list[first:last]
            else:
                pl = patch_list[(last - 1) : (first + 1)]
                pl.reverse()
        else:
            raise CmdException('Malformed patch name: %s' % name)

        for p in pl:
            if p in patches:
                raise CmdException('Duplicate patch name: %s' % p)

        patches += pl

    if ordered:
        patches = [p for p in patch_list if p in patches]

    return patches


def name_email(address):
    p = email.utils.parseaddr(address)
    if p[1]:
        return p
    else:
        raise CmdException(
            'Incorrect "name <email>"/"email (name)" string: %s' % address
        )


def address_or_alias(addr_pair):
    """Pass-through name/email address or lookup alias from config.

    Returns a name-email tuple if the e-mail address is valid, otherwise looks up the
    alias in the config files.

    """
    addr = addr_pair[1]
    if '@' in addr:
        # it's an e-mail address
        return addr_pair
    alias = config.get('mail.alias.' + addr)
    if alias:
        # it's an alias
        return name_email(alias)
    raise CmdException('unknown e-mail alias: %s' % addr)


def apply_patch(stack, diff, base=None, reject=False, strip=None, context_lines=None):
    iw = stack.repository.default_iw
    iw.refresh_index()
    if base:
        orig_head = stack.head
        iw.checkout(orig_head.data.tree, base.data.tree)
        stack.set_head(base, msg='apply patch')

    try:
        iw.apply(
            diff, quiet=False, reject=reject, strip=strip, context_lines=context_lines
        )
    except MergeException:
        if base:
            iw.checkout_hard(orig_head.data.tree)
        raise

    if base:
        iw.update_index(iw.changed_files(base.data.tree))
        top = stack.repository.commit(
            CommitData(
                tree=stack.repository.default_index.write_tree(),
                message='temporary commit used for applying a patch',
                parents=[base],
            )
        )
        iw.checkout(top.data.tree, orig_head.data.tree)
        stack.set_head(orig_head, msg='post apply')
        iw.merge(base.data.tree, orig_head.data.tree, top.data.tree)


def prepare_rebase(stack, cmd_name):
    # pop all patches
    iw = stack.repository.default_iw
    trans = StackTransaction(stack, '%s (pop)' % cmd_name, check_clean_iw=iw)
    out.start('Popping all applied patches')
    try:
        trans.reorder_patches(
            applied=[],
            unapplied=trans.applied + trans.unapplied,
            iw=iw,
            allow_interactive=True,
        )
    except TransactionException:
        pass
    retval = trans.run(iw, print_current_patch=False)
    if retval:
        out.done('Failed to pop applied patches')
    else:
        out.done()
    return retval


def rebase(stack, iw, target_commit=None):
    command = config.get('branch.%s.stgit.rebasecmd' % stack.name) or config.get(
        'stgit.rebasecmd'
    )
    if not command and not target_commit:
        raise CmdException('Default rebasing requires a commit')
    elif target_commit:
        out.start('Rebasing to "%s"' % target_commit.sha1)
    else:
        out.start('Rebasing to the default target')

    if command:
        command = command.split()
        if target_commit is not None:
            command.append(target_commit.sha1)
        iw.run(command).run()
    else:
        iw.checkout_hard(target_commit)
        stack.set_head(target_commit, 'rebase')
    out.done()


def post_rebase(stack, applied, cmd_name, check_merged):
    iw = stack.repository.default_iw
    trans = StackTransaction(stack, '%s (reapply)' % cmd_name)
    try:
        if check_merged:
            merged = set(trans.check_merged(applied))
        else:
            merged = set()
        for pn in applied:
            trans.push_patch(
                pn, iw, allow_interactive=True, already_merged=pn in merged
            )
    except TransactionHalted:
        pass
    return trans.run(iw)


#
# Patch description/e-mail/diff parsing
#
def __split_descr_diff(string):
    """Return the description and the diff from the given string."""
    m = re.search(
        br'''
        ^
        (?:
               --- \s*
             | --- \s \w
             | diff \s -
             | Index: \s
        )
        ''',
        string,
        re.MULTILINE | re.VERBOSE,
    )

    if m:
        desc = string[: m.start()]
        diff = string[m.start() :]
    else:
        desc = string
        diff = b''

    return desc, diff


def __parse_description(descr):
    """Parse the patch description for author information."""
    subject = ''
    authname = authemail = authdate = None

    descr_lines = [line.rstrip() for line in descr.splitlines()]

    lasthdr = 0
    descr_strip = 0

    # Parse the patch header
    for pos, line in enumerate(descr_lines):
        if not line:
            continue
        # check for a "From|Author:" line
        if re.match(r'\s*(?:from|author):\s+', line, re.I):
            auth = re.findall(r'^.*?:\s+(.*)$', line)[0]
            authname, authemail = name_email(auth)
            lasthdr = pos + 1
            continue
        # check for a "Date:" line
        if re.match(r'\s*date:\s+', line, re.I):
            authdate = re.findall(r'^.*?:\s+(.*)$', line)[0]
            lasthdr = pos + 1
            continue
        if subject:
            break
        # get the subject
        subject = line[descr_strip:]
        if re.match(r'commit [\da-f]{40}$', subject):
            # 'git show' output, look for the real subject
            subject = ''
            descr_strip = 4
        lasthdr = pos + 1

    body = ''.join(line[descr_strip:] + '\n' for line in descr_lines[lasthdr:])
    message = subject + '\n' + body

    return (message, authname, authemail, authdate)


def parse_patch(patch_data, contains_diff):
    """Parse patch data.

    Returns (description, authname, authemail, authdate, diff)

    """
    assert isinstance(patch_data, bytes)
    if contains_diff:
        (descr, diff) = __split_descr_diff(patch_data)
    else:
        descr = patch_data
        diff = None
    (descr, authname, authemail, authdate) = __parse_description(
        decode_utf8_with_latin1(descr)
    )

    # we don't yet have an agreed place for the creation date.
    # Just return None
    return (descr, authname, authemail, authdate, diff)


def run_commit_msg_hook(repo, cd, editor_is_used=True):
    """Run the commit-msg hook (if any) on a commit.

    :param cd: The :class:`stgit.lib.git.CommitData` to run the hook on.

    :returns: the new :class:`stgit.lib.git.CommitData`

    """
    env = dict(cd.env)
    if not editor_is_used:
        env['GIT_EDITOR'] = ':'
    commit_msg_hook = get_hook(repo, 'commit-msg', env)

    if commit_msg_hook:
        try:
            new_msg = run_hook_on_bytes(commit_msg_hook, cd.message)
        except RunException as exc:
            raise EditorException(str(exc))

        return cd.set_message(new_msg)
    else:
        return cd


def update_commit_data(cd, message=None, author=None, sign_str=None, edit=False):
    """Create updated CommitData according to the command line options."""
    # Set the commit message from commandline.
    if message is not None:
        cd = cd.set_message(message)

    # Modify author data.
    if author is not None:
        cd = cd.set_author(author)

    # Add Signed-off-by: or similar.
    if sign_str is None:
        sign_str = config.get("stgit.autosign")
    if sign_str:
        cd = cd.set_message(
            add_trailer(cd.message_str, sign_str, cd.committer.name, cd.committer.email)
        )

    if edit:
        tmpl = templates.get_template('patchdescr.tmpl')
        if tmpl:
            cd = cd.set_message(cd.message_str + tmpl)
        cd = cd.set_message(edit_bytes(cd.message, '.stgit-new.txt'))

    return cd


class DirectoryException(StgException):
    pass


class DirectoryAnywhere:
    def setup(self):
        pass


class DirectoryHasRepository:
    def setup(self):
        # This will throw an exception if we don't have a repository.
        self.repository = StackRepository.default()

    def cd_to_topdir(self):
        worktree_top = (
            Run('git', 'rev-parse', '--show-cdup')
            .discard_stderr()
            .raw_output()
            .rstrip()
        )
        if worktree_top:
            os.chdir(worktree_top)


class DirectoryInWorktree(DirectoryHasRepository):
    def setup(self):
        DirectoryHasRepository.setup(self)
        if not self._is_inside_worktree():
            raise DirectoryException('Not inside a git worktree')

    def _is_inside_worktree(self):
        return (
            Run('git', 'rev-parse', '--is-inside-work-tree').output_one_line() == 'true'
        )


class DirectoryGotoTopLevel(DirectoryInWorktree):
    def setup(self):
        DirectoryInWorktree.setup(self)
        self.cd_to_topdir()
