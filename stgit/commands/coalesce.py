# -*- coding: utf-8 -*-

__copyright__ = """
Copyright (C) 2007, Karl Hasselström <kha@treskal.com>

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
from stgit.out import *
from stgit import utils
from stgit.commands import common
from stgit.lib import git, transaction

help = 'coalesce two or more patches into one'
usage = """%prog [options] <patches>

Coalesce two or more patches, creating one big patch that contains all
their changes. The patches must all be applied, and must be
consecutive."""

directory = common.DirectoryHasRepositoryLib()
options = [make_option('-n', '--name', help = 'name of coalesced patch'),
           make_option('-m', '--message',
                       help = 'commit message of coalesced patch')]

def _coalesce(stack, name, msg, patches):
    applied = stack.patchorder.applied

    # Make sure the patches are consecutive.
    applied_ix = dict((applied[i], i) for i in xrange(len(applied)))
    ixes = list(sorted(applied_ix[p] for p in patches))
    i0, i1 = ixes[0], ixes[-1]
    if i1 - i0 + 1 != len(patches):
        raise common.CmdException('The patches must be consecutive')

    # Make a commit for the coalesced patch.
    def bad_name(pn):
        return pn not in patches and stack.patches.exists(pn)
    if name and bad_name(name):
        raise common.CmdException('Patch name "%s" already taken')
    ps = [stack.patches.get(pn) for pn in applied[i0:i1+1]]
    if msg == None:
        msg = '\n\n'.join('%s\n\n%s' % (p.name.ljust(70, '-'),
                                        p.commit.data.message)
                          for p in ps)
        msg = utils.edit_string(msg, '.stgit-coalesce.txt').strip()
    if not name:
        name = utils.make_patch_name(msg, bad_name)
    cd = git.Commitdata(tree = ps[-1].commit.data.tree,
                        parents = ps[0].commit.data.parents, message = msg)

    # Rewrite refs.
    trans = transaction.StackTransaction(stack, 'stg coalesce')
    for pn in applied[i0:i1+1]:
        trans.patches[pn] = None
    parent = trans.patches[name] = stack.repository.commit(cd)
    trans.applied = applied[:i0]
    trans.applied.append(name)
    for pn in applied[i1+1:]:
        p = stack.patches.get(pn)
        parent = trans.patches[pn] = stack.repository.commit(
            p.commit.data.set_parent(parent))
        trans.applied.append(pn)
    trans.run()

def func(parser, options, args):
    stack = directory.repository.current_stack
    applied = set(stack.patchorder.applied)
    patches = set(common.parse_patches(args, list(stack.patchorder.applied)))
    if len(patches) < 2:
        raise common.CmdException('Need at least two patches')
    _coalesce(stack, options.name, options.message, patches)
