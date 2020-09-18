from stgit import argparse
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    color_diff_flags,
    parse_rev,
)
from stgit.out import out

__copyright__ = """
Copyright (C) 2005, Catalin Marinas <catalin.marinas@gmail.com>

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

help = 'Show the files modified by a patch (or the current patch)'
kind = 'patch'
usage = ['[options] [--] [[<branch>:]<patch>]']
description = """
List the files modified by the given patch (defaulting to the current
one). Passing the '--stat' option shows the diff statistics for the
given patch. Note that this command doesn't show the files modified in
the working tree and not yet included in the patch by a 'refresh'
command. Use the 'diff' or 'status' commands for these files."""

args = ['applied_patches', 'unapplied_patches', 'hidden_patches']
options = [
    opt('-s', '--stat', action='store_true', short='Show the diffstat'),
    opt('--bare', action='store_true', short='Bare file names (useful for scripting)'),
] + argparse.diff_opts_option()

directory = DirectoryHasRepository()


def func(parser, options, args):
    """Show the files modified by a patch (or the current patch)"""
    if options.bare and options.stat:
        raise CmdException('Cannot specify both --bare and --stat')
    repository = directory.repository
    if len(args) == 0:
        stack = repository.current_stack
        commit = stack.top
    elif len(args) == 1:
        branch, name = parse_rev(args[0])
        stack = repository.get_stack(branch)
        if not stack.patches.exists(name):
            raise CmdException('%s: Unknown patch name' % name)
        commit = stack.patches.get(name).commit
    else:
        parser.error('incorrect number of arguments')

    if options.stat:
        cmd = ['git', 'diff-tree', '--stat', '--summary', '--no-commit-id']
        cmd.extend(options.diff_flags)
        cmd.extend(color_diff_flags())
        cmd.append(commit.sha1)
        out.stdout_bytes(repository.run(cmd).decoding(None).raw_output())
    else:
        used = set()
        for dt in repository.diff_tree_files(
            commit.data.parent.data.tree, commit.data.tree
        ):
            _, _, _, _, status, oldname, newname = dt
            for filename in [oldname, newname]:
                if filename in used:
                    continue
                else:
                    used.add(filename)

                if options.bare:
                    out.stdout(filename)
                else:
                    out.stdout('%s %s' % (status, filename))
