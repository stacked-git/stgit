# -*- coding: utf-8 -*-
"""This module contains utility functions for patch editing."""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import re

from stgit import utils
from stgit.commands import common
from stgit.lib.git import Date, Person, diffstat


def update_patch_description(repo, cd, text, contains_diff):
    """Update the given L{CommitData<stgit.lib.git.CommitData>} with the
    given text description, which may contain author name and time
    stamp in addition to a new commit message. If C{contains_diff} is
    true, it may also contain a replacement diff.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    (
        message, authname, authemail, authdate, diff
    ) = common.parse_patch(text, contains_diff)
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
    """Return a description text for the patch, suitable for editing
    and/or reimporting with L{update_patch_description()}.

    @param cd: The L{CommitData<stgit.lib.git.CommitData>} to generate
               a description of
    @param append_diff: Whether to append the patch diff to the
                        description
    @type append_diff: C{bool}
    @param diff_flags: Extra parameters to pass to C{git diff}
    @param replacement_diff: Diff text to use; or C{None} if it should
                             be computed from C{cd}
    @type replacement_diff: C{str} or C{None}"""
    desc = '\n'.join(
        [
            'From: %s' % cd.author.name_email,
            'Date: %s' % cd.author.date.isoformat(),
            '',
            cd.message,
        ]
    ).encode('utf-8')
    if append_diff:
        desc += b'\n---\n'
        if replacement_diff:
            desc += b'\n' + replacement_diff
        else:
            diff = repo.diff_tree(cd.parent.data.tree, cd.tree, diff_flags)
            if diff:
                desc += b'\n'.join([b'', diffstat(diff).encode('utf-8'), diff])
    return desc


def interactive_edit_patch(repo, cd, edit_diff, diff_flags):
    """Edit the patch interactively. If C{edit_diff} is true, edit the
    diff as well. If C{replacement_diff} is not C{None}, it contains a
    diff to edit instead of the patch's real diff.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    return update_patch_description(
        repo,
        cd,
        utils.edit_bytes(
            patch_desc(repo, cd, edit_diff, diff_flags, replacement_diff=None),
            '.stgit-edit.' + ['txt', 'patch'][bool(edit_diff)]
        ),
        edit_diff,
    )


def auto_edit_patch(repo, cd, msg, author, sign_str):
    """Edit the patch noninteractively in a couple of ways:

         - If C{msg} is not C{None}, parse it to find a replacement
           message, and possibly also replacement author and timestamp.

         - C{author} is a function that takes the original
           L{Person<stgit.lib.git.Person>} value as argument, and
           return the new one.

         - C{sign_str}, if not C{None}, is a sign string to append to
           the message.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    if msg is not None:
        cd, failed_diff = update_patch_description(
            repo, cd, msg, contains_diff=False
        )
        assert not failed_diff
    a = author(cd.author)
    if a != cd.author:
        cd = cd.set_author(a)
    if sign_str is not None:
        cd = cd.set_message(
            utils.add_sign_line(
                cd.message,
                sign_str,
                Person.committer().name,
                Person.committer().email,
            )
        )
    return cd
