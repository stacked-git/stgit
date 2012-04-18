# -*- coding: utf-8 -*-

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

import sys, os
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit.run import *
from stgit import stack, git

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

directory = DirectoryGotoToplevel(log = True)

class Commit(object):
    def __init__(self, id):
        self.id = id
        self.parents = set()
        self.children = set()
        self.patch = None
        self.__commit = None
    def __get_commit(self):
        if not self.__commit:
            self.__commit = git.get_commit(self.id)
        return self.__commit
    commit = property(__get_commit)
    def __str__(self):
        if self.patch:
            return '%s (%s)' % (self.id, self.patch)
        else:
            return self.id
    def __repr__(self):
        return '<%s>' % str(self)

def read_commit_dag(branch):
    out.start('Reading commit DAG')
    commits = {}
    patches = set()
    for line in Run('git', 'rev-list', '--parents', '--all').output_lines():
        cs = line.split()
        for id in cs:
            if not id in commits:
                commits[id] = Commit(id)
        for id in cs[1:]:
            commits[cs[0]].parents.add(commits[id])
            commits[id].children.add(commits[cs[0]])
    for line in Run('git', 'show-ref').output_lines():
        id, ref = line.split()
        m = re.match(r'^refs/patches/%s/(.+)$' % re.escape(branch), ref)
        if m and not m.group(1).endswith('.log'):
            c = commits[id]
            c.patch = m.group(1)
            patches.add(c)
    out.done()
    return commits, patches

def func(parser, options, args):
    """Repair inconsistencies in StGit metadata."""

    orig_applied = crt_series.get_applied()
    orig_unapplied = crt_series.get_unapplied()
    orig_hidden = crt_series.get_hidden()

    if crt_series.get_protected():
        raise CmdException(
            'This branch is protected. Modification is not permitted.')

    # Find commits that aren't patches, and applied patches.
    head = git.get_commit(git.get_head()).get_id_hash()
    commits, patches = read_commit_dag(crt_series.get_name())
    c = commits[head]
    patchify = []       # commits to definitely patchify
    maybe_patchify = [] # commits to patchify if we find a patch below them
    applied = []
    while len(c.parents) == 1:
        parent, = c.parents
        if c.patch:
            applied.append(c)
            patchify.extend(maybe_patchify)
            maybe_patchify = []
        else:
            maybe_patchify.append(c)
        c = parent
    applied.reverse()
    patchify.reverse()

    # Find patches hidden behind a merge.
    merge = c
    todo = set([c])
    seen = set()
    hidden = set()
    while todo:
        c = todo.pop()
        seen.add(c)
        todo |= c.parents - seen
        if c.patch:
            hidden.add(c)
    if hidden:
        out.warn(('%d patch%s are hidden below the merge commit'
                  % (len(hidden), ['es', ''][len(hidden) == 1])),
                 '%s,' % merge.id, 'and will be considered unapplied.')

    # Make patches of any linear sequence of commits on top of a patch.
    names = set(p.patch for p in patches)
    def name_taken(name):
        return name in names
    if applied and patchify:
        out.start('Creating %d new patch%s'
                  % (len(patchify), ['es', ''][len(patchify) == 1]))
        for p in patchify:
            name = make_patch_name(p.commit.get_log(), name_taken)
            out.info('Creating patch %s from commit %s' % (name, p.id))
            aname, amail, adate = name_email_date(p.commit.get_author())
            cname, cmail, cdate = name_email_date(p.commit.get_committer())
            parent, = p.parents
            crt_series.new_patch(
                name, can_edit = False, commit = False,
                top = p.id, bottom = parent.id, message = p.commit.get_log(),
                author_name = aname, author_email = amail, author_date = adate,
                committer_name = cname, committer_email = cmail)
            p.patch = name
            applied.append(p)
            names.add(name)
        out.done()

    # Figure out hidden
    orig_patches = orig_applied + orig_unapplied + orig_hidden
    orig_applied_name_set = set(orig_applied)
    orig_unapplied_name_set = set(orig_unapplied)
    orig_hidden_name_set = set(orig_hidden)
    orig_patches_name_set = set(orig_patches)
    hidden = [p for p in patches if p.patch in orig_hidden_name_set]

    # Write the applied/unapplied files.
    out.start('Checking patch appliedness')
    unapplied = patches - set(applied) - set(hidden)
    applied_name_set = set(p.patch for p in applied)
    unapplied_name_set = set(p.patch for p in unapplied)
    hidden_name_set = set(p.patch for p in hidden)
    patches_name_set = set(p.patch for p in patches)
    for name in orig_patches_name_set - patches_name_set:
        out.info('%s is gone' % name)
    for name in applied_name_set - orig_applied_name_set:
        out.info('%s is now applied' % name)
    for name in unapplied_name_set - orig_unapplied_name_set:
        out.info('%s is now unapplied' % name)
    for name in hidden_name_set - orig_hidden_name_set:
        out.info('%s is now hidden' % name)
    orig_order = dict(zip(orig_patches, xrange(len(orig_patches))))
    def patchname_cmp(p1, p2):
        i1 = orig_order.get(p1, len(orig_order))
        i2 = orig_order.get(p2, len(orig_order))
        return cmp((i1, p1), (i2, p2))
    crt_series.set_applied(p.patch for p in applied)
    crt_series.set_unapplied(sorted(unapplied_name_set, cmp = patchname_cmp))
    crt_series.set_hidden(sorted(hidden_name_set, cmp = patchname_cmp))
    out.done()
