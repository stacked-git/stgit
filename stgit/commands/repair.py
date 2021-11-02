from stgit.commands.common import CmdException, DirectoryGotoTopLevel
from stgit.lib.transaction import StackTransaction, TransactionHalted
from stgit.out import out

__copyright__ = """
Copyright (C) 2006, Karl Hasselström <kha@treskal.com>

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

help = 'Fix StGit metadata if branch was modified with git commands'
kind = 'stack'
usage = ['']
description = """
If you modify an StGit stack (branch) with some git commands -- such
as commit, pull, merge, and rebase -- you will leave the StGit
metadata in an inconsistent state. In that situation, you have two
options:

  1. Use "stg undo" to undo the effect of the git commands. (If you
     know what you are doing and want more control, "git reset" or
     similar will work too.)

  2. Use "stg repair". This will fix up the StGit metadata to
     accommodate the modifications to the branch. Specifically, it will
     do the following:

       * If you have made regular git commits on top of your stack of
         StGit patches, "stg repair" makes new StGit patches out of
         them, preserving their contents.

       * However, merge commits cannot become patches; if you have
         committed a merge on top of your stack, "repair" will simply
         mark all patches below the merge unapplied, since they are no
         longer reachable. If this is not what you want, use "stg
         undo" to get rid of the merge and run "stg repair" again.

       * The applied patches are supposed to be precisely those that
         are reachable from the branch head. If you have used e.g.
         "git reset" to move the head, some applied patches may no
         longer be reachable, and some unapplied patches may have
         become reachable. "stg repair" will correct the appliedness
         of such patches.

     "stg repair" will fix these inconsistencies reliably, so as long
     as you like what it does, you have no reason to avoid causing
     them in the first place. For example, you might find it
     convenient to make commits with a graphical tool and then have
     "stg repair" make proper patches of the commits.

NOTE: If using git commands on the stack was a mistake, running "stg
repair" is _not_ what you want. In that case, what you want is option
(1) above."""

args = []
options = []

directory = DirectoryGotoTopLevel()


def func(parser, options, args):
    """Repair inconsistencies in StGit metadata."""
    if args:
        parser.error('incorrect number of arguments')

    repository = directory.repository
    stack = repository.get_stack()

    if stack.protected:
        raise CmdException('This branch is protected. Modification is not permitted.')

    patchorder = stack.patchorder
    patches = list(patchorder.all)

    # Find commits that aren't patches, and applied patches.
    patchify = []  # commits to definitely patchify
    maybe_patchify = []  # commits to patchify if we find a patch below them
    applied = []
    c = stack.head
    while len(c.data.parents) == 1:
        for pn in patchorder.all:
            if stack.patches[pn] == c:
                applied.append(pn)
                patchify.extend(maybe_patchify)
                maybe_patchify = []
                break
        else:
            maybe_patchify.append(c)
        c = c.data.parent
    applied.reverse()
    patchify.reverse()

    # Find patches unreachable behind a merge.
    merge = c
    todo = set([c])
    seen = set()
    unreachable = set()
    while todo:
        c = todo.pop()
        seen.add(c)
        todo |= set(c.data.parents) - seen
        if any(stack.patches[pn] == c for pn in patches):
            unreachable.add(c)
    if unreachable:
        out.warn(
            (
                '%d patch%s are hidden below the merge commit'
                % (len(unreachable), ['es', ''][len(unreachable) == 1])
            ),
            '%s,' % merge.sha1,
            'and will be considered unapplied.',
        )

    # Make patches of any linear sequence of commits on top of a patch.
    if applied and patchify:
        out.start(
            'Creating %d new patch%s' % (len(patchify), ['es', ''][len(patchify) == 1])
        )

        for c in patchify:
            pn = stack.patches.make_name(c.data.message_str)
            out.info('Creating patch %s from commit %s' % (pn, c.sha1))
            stack.patches.new(pn, c, 'repair')
            applied.append(pn)
        out.done()

    # Figure out hidden
    hidden = [pn for pn in patches if pn in patchorder.hidden]

    # Write the applied/unapplied files.
    out.start('Checking patch appliedness')
    unapplied = [pn for pn in patches if pn not in applied and pn not in hidden]
    for pn in patchorder.all:
        if pn not in patches:
            out.info('%s is gone' % pn)
    for pn in applied:
        if pn not in patchorder.applied:
            out.info('%s is now applied' % pn)
    for pn in unapplied:
        if pn not in patchorder.unapplied:
            out.info('%s is now unapplied' % pn)
    for pn in hidden:
        if pn not in patchorder.hidden:
            out.info('%s is now hidden' % pn)
    out.done()

    orig_order = {pn: i for i, pn in enumerate(patchorder.all)}

    def patchname_key(p):
        i = orig_order.get(p, len(orig_order))
        return i, p

    trans = StackTransaction(
        stack,
        discard_changes=False,
        allow_conflicts=False,
        allow_bad_head=True,
        check_clean_iw=None,
    )
    try:
        trans.applied = applied
        trans.unapplied = sorted(unapplied, key=patchname_key)
        trans.hidden = sorted(hidden, key=patchname_key)
    except TransactionHalted:
        pass
    return trans.run('repair')
