
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

from optparse import make_option

from stgit import utils
from stgit.commands import common
from stgit.lib import git as gitlib, transaction

help = 'create a new patch and make it the topmost one'
usage = """%prog [options] [name]

Create a new, empty patch and make it the topmost one. If the
'--message' option is not passed, an editor is invoked with the
.git/patchdescr.tmpl, ~/.stgit/templates/patchdescr.tmpl or
/usr/share/stgit/templates/patchdescr.tmpl file used a as template,
together with generated lines. The local changes in the working tree
are not included in the patch; an "stg refresh" command is needed for
this.

If no name is given for the new patch, one is generated from the first
line of the commit message."""

directory = common.DirectoryHasRepositoryLib()
options = (utils.make_author_committer_options()
           + utils.make_message_options() + utils.make_sign_options())

def func(parser, options, args):
    """Create a new patch."""
    stack = directory.repository.current_stack
    if stack.repository.default_index.conflicts():
        raise common.CmdException(
            'Cannot create a new patch -- resolve conflicts first')

    # Choose a name for the new patch -- or None, which means make one
    # up later when we've gotten hold of the commit message.
    if len(args) == 0:
        name = None
    elif len(args) == 1:
        name = args[0]
        if stack.patches.exists(name):
            raise common.CmdException('%s: patch already exists' % name)
    else:
        parser.error('incorrect number of arguments')

    head = directory.repository.refs.get(directory.repository.head)
    cd = gitlib.Commitdata(
        tree = head.data.tree, parents = [head], message = '',
        author = gitlib.Person.author(), committer = gitlib.Person.committer())

    # Set patch commit message from commandline.
    if options.message != None:
        cd = cd.set_message(options.message)

    # Modify author and committer data.
    cd = (cd.set_author(options.author(cd.author))
            .set_committer(options.committer(cd.committer)))

    # Add Signed-off-by: or similar.
    if options.sign_str != None:
        cd = cd.set_message(
            utils.add_sign_line(cd.message, options.sign_str,
                                cd.committer.name, cd.committer.email))

    if options.save_template:
        options.save_template(cd.message)
        return utils.STGIT_SUCCESS

    # Let user edit the commit message manually.
    if not options.message:
        cd = cd.set_message(utils.edit_string(cd.message, '.stgit-new.txt'))
    if name == None:
        name = utils.make_patch_name(cd.message,
                                     lambda name: stack.patches.exists(name))

    # Write the new patch.
    iw = stack.repository.default_iw
    trans = transaction.StackTransaction(stack, 'new')
    trans.patches[name] = stack.repository.commit(cd)
    trans.applied.append(name)
    return trans.run()
