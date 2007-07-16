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
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git

help = 'StGIT-ify any GIT commits made on top of your StGIT stack'
usage = """%prog [options]

If you have made GIT commits on top of your stack of StGIT patches,
many StGIT commands will refuse to work. This command converts any
such commits to StGIT patches, preserving their contents.

Only GIT commits with exactly one parent can be assimilated; in other
words, if you have committed a merge on top of your stack, this
command cannot help you."""

options = []

def func(parser, options, args):
    """Assimilate a number of patches.
    """

    def nothing_to_do():
        out.info('No commits to assimilate')

    top_patch = crt_series.get_current_patch()
    if not top_patch:
        return nothing_to_do()

    victims = []
    victim = git.get_commit(git.get_head())
    while victim.get_id_hash() != top_patch.get_top():
        victims.append(victim)
        parents = victim.get_parents()
        if not parents:
            raise CmdException, 'Commit %s has no parents, aborting' % victim
        elif len(parents) > 1:
            raise CmdException, 'Commit %s is a merge, aborting' % victim
        victim = git.get_commit(parents[0])

    if not victims:
        return nothing_to_do()

    if crt_series.get_protected():
        raise CmdException(
            'This branch is protected. Modification is not permitted')

    patch2name = {}
    name2patch = {}

    def name_taken(name):
        return name in name2patch or crt_series.patch_exists(name)

    for victim in victims:
        patchname = make_patch_name(victim.get_log(), name_taken)
        patch2name[victim] = patchname
        name2patch[patchname] = victim

    victims.reverse()
    for victim in victims:
        out.info('Creating patch "%s" from commit %s'
                 % (patch2name[victim], victim))
        aname, amail, adate = name_email_date(victim.get_author())
        cname, cmail, cdate = name_email_date(victim.get_committer())
        crt_series.new_patch(
            patch2name[victim],
            can_edit = False, before_existing = False, commit = False,
            top = victim.get_id_hash(), bottom = victim.get_parent(),
            message = victim.get_log(),
            author_name = aname, author_email = amail, author_date = adate,
            committer_name = cname, committer_email = cmail)
