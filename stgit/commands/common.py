# -*- coding: utf-8 -*-
"""Function/variables common to all the commands"""
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import email.utils
import os
import re
import sys

from stgit import git, stack, templates
from stgit.compat import decode_utf8_with_latin1, text
from stgit.config import config
from stgit.exception import StgException
from stgit.lib.git import CommitData, MergeException, RepositoryException
from stgit.lib.log import compat_log_entry, compat_log_external_mods
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
    add_sign_line,
    edit_string,
    get_hook,
    parse_name_email_date,
    run_hook_on_string,
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


# Command exception class
class CmdException(StgException):
    pass


# Utility functions
def parse_rev(rev):
    """Parse a revision specification into its branch:patch parts.
    """
    try:
        branch, patch = rev.split(':', 1)
    except ValueError:
        branch = None
        patch = rev

    return (branch, patch)


def git_id(crt_series, rev):
    """Return the GIT id
    """
    # TODO: remove this function once all the occurrences were converted
    # to git_commit()
    repository = StackRepository.default()
    return git_commit(rev, repository, crt_series.name).sha1


def get_public_ref(branch_name):
    """Return the public ref of the branch."""
    public_ref = config.get('branch.%s.public' % branch_name)
    if not public_ref:
        public_ref = 'refs/heads/%s.public' % branch_name
    return public_ref


def git_commit(name, repository, branch_name=None):
    """Return the a Commit object if 'name' is a patch name or Git commit.
    The patch names allowed are in the form '<branch>:<patch>' and can
    be followed by standard symbols used by git rev-parse. If <patch>
    is '{base}', it represents the bottom of the stack. If <patch> is
    {public}, it represents the public branch corresponding to the stack as
    described in the 'publish' command.
    """
    # Try a [branch:]patch name first
    branch, patch = parse_rev(name)
    if not branch:
        branch = branch_name or repository.current_branch_name

    # The stack base
    if patch.startswith('{base}'):
        base_id = repository.get_stack(branch).base.sha1
        return repository.rev_parse(base_id +
                                    strip_prefix('{base}', patch))
    elif patch.startswith('{public}'):
        public_ref = get_public_ref(branch)
        return repository.rev_parse(public_ref +
                                    strip_prefix('{public}', patch),
                                    discard_stderr=True)

    # Other combination of branch and patch
    try:
        return repository.rev_parse('patches/%s/%s' % (branch, patch),
                                    discard_stderr=True)
    except RepositoryException:
        pass

    # Try a Git commit
    try:
        return repository.rev_parse(name, discard_stderr=True)
    except RepositoryException:
        raise CmdException('%s: Unknown patch or revision name' % name)


def color_diff_flags():
    """Return the git flags for coloured diff output if the configuration and
    stdout allows."""
    stdout_is_tty = (sys.stdout.isatty() and 'true') or 'false'
    if config.get_colorbool('color.diff', stdout_is_tty) == 'true':
        return ['--color']
    else:
        return []


def check_local_changes(stack=None):
    out.start('Checking for changes in the working directory')
    if stack:
        local_changes = stack.repository.default_iw.changed_files(
            stack.head.data.tree
        )
    else:
        local_changes = git.local_changes()
    out.done()
    if local_changes:
        raise CmdException(
            'local changes in the tree. Use "refresh" or "reset --hard"'
        )


def check_head_top_equal(stack_or_series):
    # N.B. stack_or_series may be either an old-style PatchSet instance (e.g.
    # crt_series) or a new-style Stack instance since both have a
    # head_top_equal() method. TODO: eliminate instances of the former.
    if not stack_or_series.head_top_equal():
        raise CmdException(
            'HEAD and top are not the same. This can happen if you modify a '
            'branch with git. "stg repair --help" explains more about what to '
            'do next.'
        )


def check_conflicts(iw=None):
    if iw:
        conflicts = iw.index.conflicts()
    else:
        conflicts = git.get_conflicts()
    if conflicts:
        raise CmdException(
            'Unsolved conflicts. Please fix the conflicts then use "git add '
            '--update <files>" or revert the changes with "reset --hard".'
        )


def print_current_patch(stack):
    if stack.patchorder.applied:
        out.info('Now at patch "%s"' % stack.patchorder.applied[-1])
    else:
        out.info('No patches applied')


def print_crt_patch(crt_series, branch=None):
    if not branch:
        patch = crt_series.get_current()
    else:
        patch = stack.Series(branch).get_current()

    if patch:
        out.info('Now at patch "%s"' % patch)
    else:
        out.info('No patches applied')


def push_patches(crt_series, patches):
    """Push multiple patches onto the stack. This function is shared
    between the push and pull commands
    """
    forwarded = crt_series.forward_patches(patches)
    if forwarded > 1:
        out.info('Fast-forwarded patches "%s" - "%s"'
                 % (patches[0], patches[forwarded - 1]))
    elif forwarded == 1:
        out.info('Fast-forwarded patch "%s"' % patches[0])

    names = patches[forwarded:]

    for p in names:
        out.start('Pushing patch "%s"' % p)

        modified = crt_series.push_patch(p)

        if crt_series.empty_patch(p):
            out.done('empty patch')
        elif modified:
            out.done('modified')
        else:
            out.done()


def pop_patches(crt_series, patches, keep=False):
    """Pop the patches in the list from the stack. It is assumed that
    the patches are listed in the stack reverse order.
    """
    if len(patches) == 0:
        out.info('Nothing to push/pop')
    else:
        p = patches[-1]
        if len(patches) == 1:
            out.start('Popping patch "%s"' % p)
        else:
            out.start('Popping patches "%s" - "%s"' % (patches[0], p))
        crt_series.pop_patch(p, keep)
        out.done()


def get_patch_from_list(part_name, patch_list):
    candidates = [full for full in patch_list if part_name in full]
    if len(candidates) >= 2:
        out.info('Possible patches:\n  %s' % '\n  '.join(candidates))
        raise CmdException('Ambiguous patch name "%s"' % part_name)
    elif len(candidates) == 1:
        return candidates[0]
    else:
        return None


def parse_patches(patch_args, patch_list, boundary=0, ordered=False):
    """Parse patch_args list for patch names in patch_list and return
    a list. The names can be individual patches and/or in the
    patch1..patch2 format.
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
                pl = patch_list[(last - 1):(first + 1)]
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
        raise CmdException('Incorrect "name <email>"/"email (name)" string: %s'
                           % address)


def name_email_date(address):
    p = parse_name_email_date(address)
    if p:
        return p
    else:
        raise CmdException(
            'Incorrect "name <email> date" string: %s' % address
        )


def address_or_alias(addr_pair):
    """Return a name-email tuple the e-mail address is valid or look up
    the aliases in the config files.
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


def apply_patch(stack, diff, base=None, reject=False, strip=None):
    iw = stack.repository.default_iw
    iw.refresh_index()
    if base:
        orig_head = stack.head
        iw.checkout(orig_head.data.tree, base.data.tree)
        stack.set_head(base, msg='apply patch')

    try:
        iw.apply(diff, quiet=False, reject=reject, strip=strip)
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
    command = (
        config.get('branch.%s.stgit.rebasecmd' % stack.name)
        or config.get('stgit.rebasecmd')
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
def __end_descr(line):
    return (re.match(br'---\s*$', line) or
            re.match(b'diff -', line) or
            re.match(b'Index: ', line) or
            re.match(br'--- \w', line))


def __split_descr_diff(string):
    """Return the description and the diff from the given string
    """
    descr = diff = b''
    top = True

    for line in string.split(b'\n'):
        if top:
            if not __end_descr(line):
                descr += line + b'\n'
                continue
            else:
                top = False
        diff += line + b'\n'

    return (descr.rstrip(), diff)


def __parse_description(descr):
    """Parse the patch description and return the new description and
    author information (if any).
    """
    subject = body = ''
    authname = authemail = authdate = None

    descr_lines = [line.rstrip() for line in descr.split('\n')]
    if not descr_lines:
        raise CmdException("Empty patch description")

    lasthdr = 0
    end = len(descr_lines)
    descr_strip = 0

    # Parse the patch header
    for pos in range(0, end):
        if not descr_lines[pos]:
            continue
        # check for a "From|Author:" line
        if re.match(r'\s*(?:from|author):\s+', descr_lines[pos], re.I):
            auth = re.findall(r'^.*?:\s+(.*)$', descr_lines[pos])[0]
            authname, authemail = name_email(auth)
            lasthdr = pos + 1
            continue
        # check for a "Date:" line
        if re.match(r'\s*date:\s+', descr_lines[pos], re.I):
            authdate = re.findall(r'^.*?:\s+(.*)$', descr_lines[pos])[0]
            lasthdr = pos + 1
            continue
        if subject:
            break
        # get the subject
        subject = descr_lines[pos][descr_strip:]
        if re.match(r'commit [\da-f]{40}$', subject):
            # 'git show' output, look for the real subject
            subject = ''
            descr_strip = 4
        lasthdr = pos + 1

    # get the body
    if lasthdr < end:
        body = '\n' + '\n'.join(l[descr_strip:] for l in descr_lines[lasthdr:])

    return (subject + body, authname, authemail, authdate)


def parse_mail(msg):
    """Parse the message object and return (description, authname,
    authemail, authdate, diff)
    """
    import email.header
    if sys.version_info[0] <= 2:
        # Python 2's decode_header() fails to decode encoded words if they are
        # quoted. This does not match the behavior of Python3 or `git
        # mailinfo`. For example, Python2 does not handle this header
        # correctly:
        #
        #    From: "=?UTF-8?q?Christian=20K=C3=B6nig?=" <name@example.com>
        #
        # By replacing the encoded words regex in the email.header module, we
        # can bless Python2 with the same behavior as Python3.
        email.header.ecre = re.compile(
            (r'=\? (?P<charset>[^?]*?)'
             r' \? (?P<encoding>[QqBb])'
             r' \? (?P<encoded>.*?)'
             r' \?='), re.VERBOSE | re.MULTILINE)

    def __decode_header(header):
        """Decode a qp-encoded e-mail header as per rfc2047"""
        try:
            decoded_words = email.header.decode_header(header)
            return text(email.header.make_header(decoded_words))
        except Exception as ex:
            raise CmdException('header decoding error: %s' % str(ex))

    # parse the headers
    if 'from' in msg:
        authname, authemail = name_email(__decode_header(msg['from']))
    else:
        authname = authemail = None

    # '\n\t' can be found on multi-line headers
    descr = __decode_header(msg['subject'])
    descr = re.sub('\n[ \t]*', ' ', descr)
    authdate = msg['date']

    # remove the '[*PATCH*]' expression in the subject
    if descr:
        descr = re.findall(r'^(\[.*?[Pp][Aa][Tt][Cc][Hh].*?\])?\s*(.*)$',
                           descr)[0][1]
    else:
        raise CmdException('Subject: line not found')

    # the rest of the message
    msg_data = b''
    for part in msg.walk():
        if part.get_content_type() in ['text/plain',
                                       'application/octet-stream']:
            payload = part.get_payload(decode=True)
            msg_data += payload

    rem_descr, diff = __split_descr_diff(msg_data)
    if rem_descr:
        descr += '\n\n' + decode_utf8_with_latin1(rem_descr)

    # parse the description for author information
    (
        descr, descr_authname, descr_authemail, descr_authdate
    ) = __parse_description(descr)
    if descr_authname:
        authname = descr_authname
    if descr_authemail:
        authemail = descr_authemail
    if descr_authdate:
        authdate = descr_authdate

    return (descr, authname, authemail, authdate, diff)


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
        decode_utf8_with_latin1(descr))

    # we don't yet have an agreed place for the creation date.
    # Just return None
    return (descr, authname, authemail, authdate, diff)


def run_commit_msg_hook(repo, cd, editor_is_used=True):
    """Run the commit-msg hook (if any) on a commit.

    @param cd: The L{CommitData<stgit.lib.git.CommitData>} to run the
               hook on.

    Return the new L{CommitData<stgit.lib.git.CommitData>}."""
    env = dict(cd.env)
    if not editor_is_used:
        env['GIT_EDITOR'] = ':'
    commit_msg_hook = get_hook(repo, 'commit-msg', env)

    try:
        new_msg = run_hook_on_string(commit_msg_hook, cd.message)
    except RunException as exc:
        raise EditorException(str(exc))

    return cd.set_message(new_msg)


def update_commit_data(cd, options):
    """Return a new CommitData object updated according to the command line
    options."""
    # Set the commit message from commandline.
    if options.message is not None:
        cd = cd.set_message(options.message)

    # Modify author data.
    cd = cd.set_author(options.author(cd.author))

    # Add Signed-off-by: or similar.
    if options.sign_str is not None:
        sign_str = options.sign_str
    else:
        sign_str = config.get("stgit.autosign")
    if sign_str is not None:
        cd = cd.set_message(
            add_sign_line(cd.message, sign_str,
                          cd.committer.name, cd.committer.email))

    # Let user edit the commit message manually, unless
    # --save-template or --message was specified.
    if not getattr(options, 'save_template', None) and options.message is None:
        tmpl = templates.get_template('patchdescr.tmpl')
        if tmpl:
            cd = cd.set_message(cd.message + tmpl)
        cd = cd.set_message(edit_string(cd.message, '.stgit-new.txt'))

    return cd


class DirectoryException(StgException):
    pass


class _Directory(object):
    def __init__(self, needs_current_series=True, log=True):
        self.needs_current_series = needs_current_series
        self.log = log

    def _get_git_dir(self):
        try:
            return Run(
                'git', 'rev-parse', '--git-dir'
            ).discard_stderr().output_one_line()
        except RunException:
            return None

    def _is_inside_worktree(self):
        return Run(
            'git', 'rev-parse', '--is-inside-work-tree'
        ).output_one_line() == 'true'

    def cd_to_topdir(self):
        worktree_top = Run(
            'git', 'rev-parse', '--show-cdup'
        ).discard_stderr().raw_output().rstrip()
        if worktree_top:
            os.chdir(worktree_top)

    def write_log(self, msg):
        if self.log:
            compat_log_entry(msg)


class DirectoryAnywhere(_Directory):
    def __init__(self):
        super(DirectoryAnywhere, self).__init__(
            needs_current_series=False, log=False
        )

    def setup(self):
        pass


class DirectoryHasRepository(_Directory):
    def setup(self):
        if not self._get_git_dir():
            raise DirectoryException('No git repository found')
        compat_log_external_mods()


class DirectoryInWorktree(DirectoryHasRepository):
    def setup(self):
        DirectoryHasRepository.setup(self)
        if not self._is_inside_worktree():
            raise DirectoryException('Not inside a git worktree')


class DirectoryGotoToplevel(DirectoryInWorktree):
    def setup(self):
        DirectoryInWorktree.setup(self)
        self.cd_to_topdir()


class DirectoryHasRepositoryLib(_Directory):
    """For commands that use the new infrastructure in stgit.lib.*."""

    def __init__(self):
        super(DirectoryHasRepositoryLib, self).__init__(
            needs_current_series=False, log=False
        )

    def setup(self):
        # This will throw an exception if we don't have a repository.
        self.repository = StackRepository.default()


class DirectoryInWorktreeLib(DirectoryHasRepositoryLib):
    def setup(self):
        DirectoryHasRepositoryLib.setup(self)
        if not self._is_inside_worktree():
            raise DirectoryException('Not inside a git worktree')


class DirectoryGotoTopLevelLib(DirectoryInWorktreeLib):
    def setup(self):
        DirectoryInWorktreeLib.setup(self)
        self.cd_to_topdir()
