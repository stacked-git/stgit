import re

from stgit import argparse
from stgit.commands.common import CmdException, DirectoryHasRepository
from stgit.lib import transaction
from stgit.out import out

__copyright__ = """
Copyright (C) 2006, Catalin Marinas <catalin.marinas@gmail.com>

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

help = 'Push or pop patches to the given one'
kind = 'stack'
usage = ['[options] [--] <patch-name>']
description = """
Push/pop patches to/from the stack until the one given on the command
line becomes current."""

args = ['other_applied_patches', 'unapplied_patches']
options = argparse.keep_option() + argparse.merged_option()

directory = DirectoryHasRepository()


def func(parser, options, args):
    if len(args) != 1:
        parser.error('incorrect number of arguments')
    name = args[0]

    stack = directory.repository.current_stack
    iw = stack.repository.default_iw
    clean_iw = (not options.keep and iw) or None
    trans = transaction.StackTransaction(
        stack,
        discard_changes=False,
        allow_conflicts=False,
        allow_bad_head=False,
        check_clean_iw=clean_iw,
    )

    if name not in trans.all_patches:
        candidates = [pn for pn in trans.all_patches if name in pn]
        if len(candidates) == 1:
            name = candidates[0]
        elif len(candidates) > 1:
            out.info('Possible patches:\n  %s' % '\n  '.join(candidates))
            raise CmdException('Ambiguous patch name "%s"' % name)
        elif re.match('[0-9A-Fa-f]{4,40}$', name):
            sha1 = name
            name = stack.patches.name_from_sha1(sha1)
            if not name:
                raise CmdException('No patch associated with %s' % sha1)
        else:
            raise CmdException('Patch "%s" does not exist' % name)

    if name in trans.applied:
        to_pop = set(trans.applied[trans.applied.index(name) + 1 :])
        popped_extra = trans.pop_patches(lambda pn: pn in to_pop)
        assert not popped_extra
    elif name in trans.unapplied:
        try:
            to_push = trans.unapplied[: trans.unapplied.index(name) + 1]
            if options.merged:
                merged = set(trans.check_merged(to_push))
            else:
                merged = set()
            for pn in to_push:
                trans.push_patch(
                    pn, iw, allow_interactive=True, already_merged=pn in merged
                )
        except transaction.TransactionHalted:
            pass
    else:
        raise CmdException('Cannot goto a hidden patch')
    return trans.run('goto', iw)
