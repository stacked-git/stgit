"""Patch editing command
"""

__copyright__ = """
Copyright (C) 2007, Catalin Marinas <catalin.marinas@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

from optparse import make_option

from stgit import git, utils
from stgit.commands import common
from stgit.lib import git as gitlib, transaction
from stgit.out import *

help = 'edit a patch description or diff'
usage = """%prog [options] [<patch>]

Edit the description and author information of the given patch (or the
current patch if no patch name was given). With --diff, also edit the
diff.

The editor is invoked with the following contents:

  From: A U Thor <author@example.com>
  Date: creation date

  Patch description

If --diff was specified, the diff appears at the bottom, after a
separator:

  ---

  Diff text

Command-line options can be used to modify specific information
without invoking the editor. (With the --edit option, the editor is
invoked even if such command-line options are given.)

If the patch diff is edited but does not apply, no changes are made to
the patch at all. The edited patch is saved to a file which you can
feed to "stg edit --file", once you have made sure it does apply."""

directory = common.DirectoryHasRepositoryLib()
options = [make_option('-d', '--diff',
                       help = 'edit the patch diff',
                       action = 'store_true'),
           make_option('-e', '--edit', action = 'store_true',
                       help = 'invoke interactive editor'),
           make_option('--author', metavar = '"NAME <EMAIL>"',
                       help = 'replae the author details with "NAME <EMAIL>"'),
           make_option('--authname',
                       help = 'replace the author name with AUTHNAME'),
           make_option('--authemail',
                       help = 'replace the author e-mail with AUTHEMAIL'),
           make_option('--authdate',
                       help = 'replace the author date with AUTHDATE'),
           make_option('--commname',
                       help = 'replace the committer name with COMMNAME'),
           make_option('--commemail',
                       help = 'replace the committer e-mail with COMMEMAIL')
           ] + (utils.make_sign_options() + utils.make_message_options()
                + utils.make_diff_opts_option())

def patch_diff(repository, cd, diff, diff_flags):
    if diff:
        diff = repository.diff_tree(cd.parent.data.tree, cd.tree, diff_flags)
        return '\n'.join([git.diffstat(diff), diff])
    else:
        return None

def patch_description(cd, diff):
    """Generate a string containing the description to edit."""

    desc = ['From: %s <%s>' % (cd.author.name, cd.author.email),
            'Date: %s' % cd.author.date.isoformat(),
            '',
            cd.message]
    if diff:
        desc += ['---',
                 '',
                diff]
    return '\n'.join(desc)

def patch_desc(repository, cd, failed_diff, diff, diff_flags):
    return patch_description(cd, failed_diff or patch_diff(
            repository, cd, diff, diff_flags))

def update_patch_description(repository, cd, text):
    message, authname, authemail, authdate, diff = common.parse_patch(text)
    cd = (cd.set_message(message)
            .set_author(cd.author.set_name(authname)
                                 .set_email(authemail)
                                 .set_date(gitlib.Date.maybe(authdate))))
    failed_diff = None
    if diff:
        tree = repository.apply(cd.parent.data.tree, diff)
        if tree == None:
            failed_diff = diff
        else:
            cd = cd.set_tree(tree)
    return cd, failed_diff

def func(parser, options, args):
    """Edit the given patch or the current one.
    """
    stack = directory.repository.current_stack

    if len(args) == 0:
        if not stack.patchorder.applied:
            raise common.CmdException(
                'Cannot edit top patch, because no patches are applied')
        patchname = stack.patchorder.applied[-1]
    elif len(args) == 1:
        [patchname] = args
        if not stack.patches.exists(patchname):
            raise common.CmdException('%s: no such patch' % patchname)
    else:
        parser.error('Cannot edit more than one patch')

    cd = orig_cd = stack.patches.get(patchname).commit.data

    # Read patch from user-provided description.
    if options.message == None:
        failed_diff = None
    else:
        cd, failed_diff = update_patch_description(stack.repository, cd,
                                                   options.message)

    # Modify author and committer data.
    if options.author != None:
        options.authname, options.authemail = common.name_email(options.author)
    for p, f, val in [('author', 'name', options.authname),
                      ('author', 'email', options.authemail),
                      ('author', 'date', gitlib.Date.maybe(options.authdate)),
                      ('committer', 'name', options.commname),
                      ('committer', 'email', options.commemail)]:
        if val != None:
            cd = getattr(cd, 'set_' + p)(
                getattr(getattr(cd, p), 'set_' + f)(val))

    # Add Signed-off-by: or similar.
    if options.sign_str != None:
        cd = cd.set_message(utils.add_sign_line(
                cd.message, options.sign_str, gitlib.Person.committer().name,
                gitlib.Person.committer().email))

    if options.save_template:
        options.save_template(
            patch_desc(stack.repository, cd, failed_diff,
                       options.diff, options.diff_flags))
        return utils.STGIT_SUCCESS

    # Let user edit the patch manually.
    if cd == orig_cd or options.edit:
        fn = '.stgit-edit.' + ['txt', 'patch'][bool(options.diff)]
        cd, failed_diff = update_patch_description(
            stack.repository, cd, utils.edit_string(
                patch_desc(stack.repository, cd, failed_diff,
                           options.diff, options.diff_flags),
                fn))

    def failed():
        fn = '.stgit-failed.patch'
        f = file(fn, 'w')
        f.write(patch_desc(stack.repository, cd, failed_diff,
                           options.diff, options.diff_flags))
        f.close()
        out.error('Edited patch did not apply.',
                  'It has been saved to "%s".' % fn)
        return utils.STGIT_COMMAND_ERROR

    # If we couldn't apply the patch, fail without even trying to
    # effect any of the changes.
    if failed_diff:
        return failed()

    # The patch applied, so now we have to rewrite the StGit patch
    # (and any patches on top of it).
    iw = stack.repository.default_iw
    trans = transaction.StackTransaction(stack, 'edit')
    if patchname in trans.applied:
        popped = trans.applied[trans.applied.index(patchname)+1:]
        assert not trans.pop_patches(lambda pn: pn in popped)
    else:
        popped = []
    trans.patches[patchname] = stack.repository.commit(cd)
    try:
        for pn in popped:
            trans.push_patch(pn, iw)
    except transaction.TransactionHalted:
        pass
    try:
        # Either a complete success, or a conflict during push. But in
        # either case, we've successfully effected the edits the user
        # asked us for.
        return trans.run(iw)
    except transaction.TransactionException:
        # Transaction aborted -- we couldn't check out files due to
        # dirty index/worktree. The edits were not carried out.
        return failed()
