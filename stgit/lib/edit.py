"""This module contains utility functions for patch editing."""

import io
import re

from stgit import utils
from stgit.commands import common
from stgit.config import config
from stgit.lib import transaction
from stgit.lib.git import Date, Person
from stgit.lib.log import log_stack_state
from stgit.out import out

EDIT_MESSAGE_INSTRUCTIONS = """# Everything here is editable! You can modify the patch name, author,
# date, commit message, and the diff (if --diff was given).
# Lines starting with '#' will be ignored, and an empty message
# aborts the edit.
"""


def update_patch_description(repo, cd, text, contains_diff):
    """Create commit with updated description.

    The given :class:`stgit.lib.git.CommitData` is updated with the
    given description, which may contain author name and time
    stamp in addition to a new commit message. If ``contains_diff`` is
    true, it may also contain a replacement diff.

    :returns: 3-tuple:
        - the new :class:`CommitData<stgit.lib.git.CommitData>`
        - the patch name if given or None otherwise
        - the diff text if it did not apply or None otherwise
    """
    (message, patch_name, authname, authemail, authdate, diff) = common.parse_patch(
        text, contains_diff
    )
    author = cd.author
    if authname is not None:
        author = author.set_name(authname)
    if authemail is not None:
        author = author.set_email(authemail)
    if authdate is not None:
        author = author.set_date(Date(authdate))
    cd = cd.set_message(message).set_author(author)
    failed_diff = None
    if diff and not re.match(br'---\s*\Z', diff, re.MULTILINE):
        tree = repo.apply(cd.parent.data.tree, diff, quiet=False)
        if tree is None:
            failed_diff = diff
        else:
            cd = cd.set_tree(tree)
    return cd, patch_name, failed_diff


def patch_desc(repo, cd, patch_name, append_diff, diff_flags, replacement_diff):
    """Return a description text for the patch.

    The returned description is suitable for editing and/or reimporting with
    :func:`update_patch_description()`.

    :param cd: the :class:`stgit.lib.git.CommitData` to generate a description of
    :param append_diff: whether to append the patch diff to the description
    :type append_diff: bool
    :param diff_flags: extra parameters to pass to `git diff`
    :param replacement_diff: diff text to use; or None if it should be computed from cd
    :type replacement_diff: str or None

    """
    commit_encoding = config.get('i18n.commitencoding')
    desc = '\n'.join(
        [
            'Patch: %s' % patch_name,
            'From: %s' % cd.author.name_email,
            'Date: %s' % cd.author.date.isoformat(),
            '',
            cd.message_str,
            EDIT_MESSAGE_INSTRUCTIONS,
        ]
    ).encode(commit_encoding)
    if append_diff:
        parts = [desc.rstrip(), b'---', b'']
        if replacement_diff:
            parts.append(replacement_diff)
        else:
            diff = repo.diff_tree(cd.parent.data.tree, cd.tree, diff_flags)
            if diff:
                diffstat = repo.default_iw.diffstat(diff).encode(commit_encoding)
                parts.extend([diffstat, diff])
        desc = b'\n'.join(parts)
    return desc


def interactive_edit_patch(repo, cd, patch_name, edit_diff, diff_flags):
    """Edit the patch interactively.

    If ``edit_diff`` is true, edit the diff as well. If ``replacement_diff`` is not
    None, it contains a diff to edit instead of the patch's real diff.

    :returns: 3-tuple:
        - the new :class:`commitdata<stgit.lib.git.commitdata>`
        - the patch name if given or none otherwise
        - the diff text if it did not apply or none otherwise
    """
    cd, patch_name, failed_diff = update_patch_description(
        repo,
        cd,
        utils.edit_bytes(
            patch_desc(
                repo, cd, patch_name, edit_diff, diff_flags, replacement_diff=None
            ),
            '.stgit-edit.' + ['txt', 'patch'][bool(edit_diff)],
        ),
        edit_diff,
    )

    # If we couldn't apply the patch, fail without even trying to
    # effect any of the changes.
    if failed_diff:
        return failed(repo, cd, patch_name, edit_diff, diff_flags, failed_diff)

    return cd, patch_name, failed_diff


def auto_edit_patch(repo, cd, msg, author, sign_str):
    """Edit the patch noninteractively in a couple of ways:

    * If ``msg`` is not None, parse it to find a replacement message, and possibly also
      replacement author and timestamp.

    * ``author`` is a function that takes the original :class:`stgit.lib.git.Person`
      value as argument, and returns the new one.

    * ``sign_str, if not None, is a trailer string to append to the message.

    :returns: 3-tuple:
        - the new :class:`commitdata<stgit.lib.git.commitdata>`
        - the patch name if given or none otherwise
        - the diff text if it did not apply or none otherwise

    """
    if msg is not None:
        cd, _, failed_diff = update_patch_description(
            repo, cd, msg, contains_diff=False
        )
        assert not failed_diff
    a = author(cd.author)
    if a != cd.author:
        cd = cd.set_author(a)
    if sign_str is not None:
        cd = cd.set_message(
            utils.add_trailer(
                cd.message_str,
                sign_str,
                Person.committer().name,
                Person.committer().email,
            )
        )
    return cd


def failed(
    repository,
    cd,
    patch_name,
    edit_diff,
    diff_flags,
    replacement_diff,
    reason='Edited patch did not apply.',
):
    """Call when edit fails. Logs to stderr and saves a patch to filesystem."""
    fn = '.stgit-failed.patch'
    with io.open(fn, 'wb') as f:
        f.write(
            patch_desc(
                repository,
                cd,
                patch_name,
                edit_diff,
                diff_flags,
                replacement_diff=replacement_diff,
            )
        )
    out.error(reason, 'The patch has been saved to "%s".' % fn)
    return utils.STGIT_COMMAND_ERROR


def perform_edit(
    stack,
    cd,
    orig_patchname,
    new_patchname,
    edit_diff,
    diff_flags,
    replacement_diff,
    set_tree=None,
):
    """Given instructions, performs required the edit.

    :returns: 2-tuple:
        - the result of the transaction
        - the new patch name, whether changed or not.
    """
    # Refresh the committer information
    cd = cd.set_committer(None)

    # Rewrite the StGit patch with the given diff (and any patches on top of
    # it).
    iw = stack.repository.default_iw
    trans = transaction.StackTransaction(stack, 'edit', allow_conflicts=True)
    if orig_patchname in trans.applied:
        popped = trans.applied[trans.applied.index(orig_patchname) + 1 :]
        popped_extra = trans.pop_patches(lambda pn: pn in popped)
        assert not popped_extra
    else:
        popped = []
    trans.patches[orig_patchname] = stack.repository.commit(cd)
    if new_patchname == "":
        new_patchname = stack.patches.make_name(cd.message_str, allow=orig_patchname)
    if new_patchname is not None and orig_patchname != new_patchname:
        out.start('Renaming patch "%s" to "%s"' % (orig_patchname, new_patchname))
        trans.rename_patch(orig_patchname, new_patchname)
        out.done()
        log_stack_state(stack, 'rename %s to %s' % (orig_patchname, new_patchname))
    else:
        new_patchname = orig_patchname
    try:
        for pn in popped:
            if set_tree:
                trans.push_tree(pn)
            else:
                trans.push_patch(pn, iw, allow_interactive=True)
    except transaction.TransactionHalted:
        pass
    try:
        # Either a complete success, or a conflict during push. But in
        # either case, we've successfully effected the edits the user
        # asked us for.
        return trans.run(iw), new_patchname
    except transaction.TransactionException:
        # Transaction aborted -- we couldn't check out files due to
        # dirty index/worktree. The edits were not carried out.
        return (
            failed(stack, cd, new_patchname, edit_diff, diff_flags, replacement_diff),
            new_patchname,
        )
