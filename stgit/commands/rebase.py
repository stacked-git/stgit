import copy
import itertools
import re
import typing
from enum import Enum

from stgit import utils
from stgit.argparse import opt
from stgit.commands.common import (
    CmdException,
    DirectoryGotoTopLevel,
    delete_patches,
    git_commit,
    post_rebase,
    prepare_rebase,
    rebase,
)
from stgit.commands.squash import squash
from stgit.config import config
from stgit.run import RunException
from stgit.utils import edit_string

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

help = 'Move the stack base to another point in history'
kind = 'stack'
usage = ['[options] [--] [new-base-id]']
description = """
Pop all patches from current stack, move the stack base to the given
[new-base-id] and push the patches back.

If you experience merge conflicts, resolve the problem and continue
the rebase by executing the following sequence:

        $ git add --update
        $ stg refresh
        $ stg goto top-patch

Or if you want to skip that patch:

        $ stg undo --hard
        $ stg push next-patch..top-patch"""

args = ['commit']
options = [
    opt(
        '-i',
        '--interactive',
        action='store_true',
        short='Open an interactive editor to manipulate patches',
    ),
    opt(
        '-n',
        '--nopush',
        action='store_true',
        short='Do not push the patches back after rebasing',
    ),
    opt(
        '-m',
        '--merged',
        action='store_true',
        short='Check for patches merged upstream',
    ),
    opt(
        '--autostash',
        action='store_true',
        short='Stash changes before the rebase and apply them after',
        default=config.getbool('stgit.autostash'),
        long='''
    Automatically create a temporary stash before the operation begins, and
    apply it after the operation ends. This means that you can run rebase on a
    dirty work-tree. However, use with care: the final stash application after a
    successful rebase might result in non-trivial conflicts.
    ''',
    ),
]

directory = DirectoryGotoTopLevel()

INTERACTIVE_APPLY_LINE = '# --- APPLY_LINE ---'

INTERACTIVE_HELP_INSTRUCTIONS = """
# Commands:
# k, keep <patch> = do not modify this patch
# s, squash <patch> = squash patch into the previous patch
# d, delete <patch> = delete patch
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# Patches above the APPLY_LINE are applied; other patches are kept unapplied.
"""


class StashPopConflictException(RunException):
    """Exception raised there's a conflict when popping the stash after --autostash"""

    def __init__(self):
        super().__init__(
            'Merge conflict raised when popping stash after rebase. Resolve the '
            'conflict, then delete the stash with `git stash drop`.'
        )


def __get_description(stack, patch):
    """Extract and return a patch's short description"""
    cd = stack.patches[patch].data
    return cd.message_str.strip().split('\n', 1)[0].rstrip()


class Action(Enum):
    # do nothing; a 'no-op'
    KEEP = 0
    # delete the patch
    DELETE = 1
    # squash the patch into the previous patch
    SQUASH = 2


Instruction = typing.NamedTuple(
    'Instruction', [('patch_name', str), ('action', Action), ('apply', bool)]
)


def __do_rebase_interactive(repository, previously_applied_patches, check_merged):
    """Opens an interactive editor, generates instruction list, and executes instructions."""
    stack = repository.get_stack()

    if len(stack.patchorder.all) == 0:
        return utils.STGIT_SUCCESS

    name_len = max((len(s) for s in stack.patchorder.all))
    line = 'keep {{:<{name_len}}} # {{}}'.format(name_len=name_len)

    # create a list of all patches to send to the editor
    instructions = []
    for pn in previously_applied_patches:
        patch = (pn, __get_description(stack, pn))
        instructions.append(line.format(*patch))
    instructions.append(INTERACTIVE_APPLY_LINE)
    for pn in stack.patchorder.all:
        if pn in previously_applied_patches:
            continue
        patch = (pn, __get_description(stack, pn))
        instructions.append(line.format(*patch))
    instructions.append(INTERACTIVE_HELP_INSTRUCTIONS)

    # open an editor to let the user generate the 'todo' instructions
    todo = edit_string(
        '\n'.join(instructions),
        '.stgit-rebase-interactive.txt',
    )

    # parse the instructions we've been given
    seen_apply_line = False
    instructions = []
    for line in todo.splitlines():
        line = line.strip()

        # record when we find the APPLY_LINE so we know which patches to apply
        if INTERACTIVE_APPLY_LINE in line:
            if INTERACTIVE_APPLY_LINE == line:
                seen_apply_line = True
            else:
                raise CmdException("Bad APPLY_LINE: '%s'" % line)

        # ignore comment lines
        if not line or line.startswith('#'):
            continue

        # parse a single instruction
        match = re.match(r'(\S+) (\S+).*', line)
        if not match:
            raise CmdException("Bad todo line: '%s'" % line)
        instruction_str, patch_name = match.groups()
        if patch_name not in stack.patchorder.all:
            raise CmdException("Bad patch name '%s'" % patch_name)
        if instruction_str in ('k', 'keep'):
            instruction_type = Action.KEEP
        elif instruction_str in ('d', 'delete'):
            instruction_type = Action.DELETE
        elif instruction_str in ('s', 'squash'):
            instruction_type = Action.SQUASH
        else:
            raise CmdException("Unknown instruction '%s'" % instruction_str)

        # save the instruction to execute later
        instructions.append(
            Instruction(patch_name, instruction_type, not seen_apply_line)
        )

    # execute all the non-squash instructions first. we use a copy to make sure
    # we don't skip any instructions as we delete from the list
    for instruction in copy.copy(instructions):
        if instruction.apply:
            post_rebase(stack, {instruction.patch_name}, 'rebase', check_merged)
        if instruction.action == Action.DELETE:
            delete_patches(stack, stack.repository.default_iw, {instruction.patch_name})
            instructions.remove(instruction)

    # execute squashes last because patch names may change during squashes. this
    # is a double loop so we can handle multiple chains of multiple squashes
    for index in itertools.count():
        if index >= len(instructions):
            break  # reached the end of the instruction list
        # determine how many of the next N instructions are squashes
        squashes_found = False
        for num_squashes in itertools.count(1):
            if len(instructions) <= index + num_squashes:
                break  # reached the end of the instruction list
            if instructions[index + num_squashes].action == Action.SQUASH:
                squashes_found = True
            else:
                break  # not a squash; chain is over
        # execute squash
        if squashes_found:
            squashes = instructions[index : index + num_squashes]
            squash(
                stack,
                stack.repository.default_iw,
                None,
                None,
                None,
                [p.patch_name for p in squashes],
            )
            # remove the squashed patches from the instruction set
            instructions = (
                instructions[: index + 1] + instructions[index + num_squashes :]
            )
    return utils.STGIT_SUCCESS


def func(parser, options, args):
    """Rebase the current stack"""

    if options.interactive:
        # destination is optional for '--interactive'
        if len(args) not in (0, 1):
            parser.error('incorrect number of arguments')
    else:
        if len(args) != 1:
            parser.error('incorrect number of arguments')

    repository = directory.repository
    stack = repository.get_stack()
    iw = repository.default_iw

    if stack.protected:
        raise CmdException('This branch is protected. Rebase is not permitted')

    if len(args) > 0:
        target = git_commit(args[0], repository)
    else:
        target = stack.base

    if options.autostash and not iw.worktree_clean():
        repository.run(['git', 'stash', 'push']).run()
        stashed_worktree = True
    else:
        stashed_worktree = False

    applied = stack.patchorder.applied
    retval = prepare_rebase(stack, 'rebase')
    if retval:
        return retval
    rebase(stack, iw, target)

    if options.interactive:
        retval = __do_rebase_interactive(
            repository, applied, check_merged=options.merged
        )
    elif not options.nopush:
        retval = post_rebase(stack, applied, 'rebase', check_merged=options.merged)
    else:
        retval = None

    if stashed_worktree:
        try:
            repository.run(['git', 'stash', 'pop']).run()
        except RunException:
            raise StashPopConflictException()

    return retval
