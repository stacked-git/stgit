"""This module contains utility functions for patch editing."""

from stgit import utils
from stgit.commands import common
from stgit.lib import git

def update_patch_description(repo, cd, text, contains_diff):
    """Update the given L{CommitData<stgit.lib.git.CommitData>} with the
    given text description, which may contain author name and time
    stamp in addition to a new commit message. If C{contains_diff} is
    true, it may also contain a replacement diff.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    (message, authname, authemail, authdate, diff
     ) = common.parse_patch(text, contains_diff)
    a = cd.author
    for val, setter in [(authname, 'set_name'), (authemail, 'set_email'),
                        (git.Date.maybe(authdate), 'set_date')]:
        if val != None:
            a = getattr(a, setter)(val)
    cd = cd.set_message(message).set_author(a)
    failed_diff = None
    if diff:
        tree = repo.apply(cd.parent.data.tree, diff, quiet = False)
        if tree == None:
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
    desc = ['From: %s <%s>' % (cd.author.name, cd.author.email),
            'Date: %s' % cd.author.date.isoformat(),
            '',
            cd.message]
    if append_diff:
        if replacement_diff:
            diff = replacement_diff
        else:
            just_diff = repo.diff_tree(cd.parent.data.tree, cd.tree, diff_flags)
            diff = '\n'.join([git.diffstat(just_diff), just_diff])
        desc += ['---', '', diff]
    return '\n'.join(desc)

def interactive_edit_patch(repo, cd, edit_diff, diff_flags, replacement_diff):
    """Edit the patch interactively. If C{edit_diff} is true, edit the
    diff as well. If C{replacement_diff} is not C{None}, it contains a
    diff to edit instead of the patch's real diff.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    return update_patch_description(
        repo, cd, utils.edit_string(
            patch_desc(repo, cd, edit_diff, diff_flags, replacement_diff),
            '.stgit-edit.' + ['txt', 'patch'][bool(edit_diff)]),
        edit_diff)

def auto_edit_patch(repo, cd, msg, contains_diff, author, committer, sign_str):
    """Edit the patch noninteractively in a couple of ways:

         - If C{msg} is not C{None}, parse it to find a replacement
           message, and possibly also replacement author and
           timestamp. If C{contains_diff} is true, also look for a
           replacement diff.

         - C{author} and C{committer} are two functions that take the
           original L{Person<stgit.lib.git.Person>} value as argument,
           and return the new one.

         - C{sign_str}, if not C{None}, is a sign string to append to
           the message.

    Return a pair: the new L{CommitData<stgit.lib.git.CommitData>};
    and the diff text if it didn't apply, or C{None} otherwise."""
    if msg == None:
        failed_diff = None
    else:
        cd, failed_diff = update_patch_description(repo, cd, msg, contains_diff)
    a, c = author(cd.author), committer(cd.committer)
    if (a, c) != (cd.author, cd.committer):
        cd = cd.set_author(a).set_committer(c)
    if sign_str != None:
        cd = cd.set_message(utils.add_sign_line(
                cd.message, sign_str, git.Person.committer().name,
                git.Person.committer().email))
    return cd, failed_diff
