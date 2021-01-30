"""This module contains utility functions for patch editing."""

import re

from stgit import utils
from stgit.commands import common
from stgit.config import config
from stgit.lib.git import Date, Person


def update_patch_description(repo, cd, text, contains_diff):
    """Create commit with updated description.

    The given :class:`stgit.lib.git.CommitData` is updated with the
    given description, which may contain author name and time
    stamp in addition to a new commit message. If ``contains_diff`` is
    true, it may also contain a replacement diff.

    :returns: tuple of the new :class:`CommitData<stgit.lib.git.CommitData>` and the
              diff test if it did not apply or None otherwise

    """
    (message, authname, authemail, authdate, diff) = common.parse_patch(
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
    return cd, failed_diff


def patch_desc(repo, cd, append_diff, diff_flags, replacement_diff):
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
            'From: %s' % cd.author.name_email,
            'Date: %s' % cd.author.date.isoformat(),
            '',
            cd.message_str,
        ]
    ).encode(commit_encoding)
    if append_diff:
        parts = [desc.rstrip(), b'', b'---', b'']
        if replacement_diff:
            parts.append(replacement_diff)
        else:
            diff = repo.diff_tree(cd.parent.data.tree, cd.tree, diff_flags)
            if diff:
                diffstat = repo.default_iw.diffstat(diff).encode(commit_encoding)
                parts.extend([diffstat, diff])
        desc = b'\n'.join(parts)
    return desc


def interactive_edit_patch(repo, cd, edit_diff, diff_flags):
    """Edit the patch interactively.

    If ``edit_diff`` is true, edit the diff as well. If ``replacement_diff`` is not
    None, it contains a diff to edit instead of the patch's real diff.

    :returns: tuple with the new :class:`stgit.lib.git.CommitData` and the diff text if
              it did not apply, or None otherwise

    """
    return update_patch_description(
        repo,
        cd,
        utils.edit_bytes(
            patch_desc(repo, cd, edit_diff, diff_flags, replacement_diff=None),
            '.stgit-edit.' + ['txt', 'patch'][bool(edit_diff)],
        ),
        edit_diff,
    )


def auto_edit_patch(repo, cd, msg, author, sign_str):
    """Edit the patch noninteractively in a couple of ways:

    * If ``msg`` is not None, parse it to find a replacement message, and possibly also
      replacement author and timestamp.

    * ``author`` is a function that takes the original :class:`stgit.lib.git.Person`
      value as argument, and returns the new one.

    * ``sign_str, if not None, is a trailer string to append to the message.

    :returns: tuple with the new :class:`stgit.lib.git.CommitData` and the diff text if
              it did not apply, or None otherwise.

    """
    if msg is not None:
        cd, failed_diff = update_patch_description(repo, cd, msg, contains_diff=False)
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
