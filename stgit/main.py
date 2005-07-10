"""Basic quilt-like functionality
"""

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

import sys, os
from optparse import OptionParser, make_option

from stgit.utils import *
from stgit import stack, git
from stgit.version import version
from stgit.config import config


# Main exception class
class MainException(Exception):
    pass


# Utility functions
def __git_id(string):
    """Return the GIT id
    """
    if not string:
        return None
    
    string_list = string.split('/')

    if len(string_list) == 1:
        patch_name = None
        git_id = string_list[0]

        if git_id == 'HEAD':
            return git.get_head()
        if git_id == 'base':
            return read_string(crt_series.get_base_file())

        for path in [os.path.join(git.base_dir, 'refs', 'heads'),
                     os.path.join(git.base_dir, 'refs', 'tags')]:
            id_file = os.path.join(path, git_id)
            if os.path.isfile(id_file):
                return read_string(id_file)
    elif len(string_list) == 2:
        patch_name = string_list[0]
        if patch_name == '':
            patch_name = crt_series.get_current()
        git_id = string_list[1]

        if not patch_name:
            raise MainException, 'No patches applied'
        elif not (patch_name in crt_series.get_applied()
                + crt_series.get_unapplied()):
            raise MainException, 'Unknown patch "%s"' % patch_name

        if git_id == 'bottom':
            return crt_series.get_patch(patch_name).get_bottom()
        if git_id == 'top':
            return crt_series.get_patch(patch_name).get_top()

    raise MainException, 'Unknown id: %s' % string

def __check_local_changes():
    if git.local_changes():
        raise MainException, \
              'local changes in the tree. Use "refresh" to commit them'

def __check_head_top_equal():
    if not crt_series.head_top_equal():
        raise MainException, \
              'HEAD and top are not the same. You probably committed\n' \
              '  changes to the tree ouside of StGIT. If you know what you\n' \
              '  are doing, use the "refresh -f" command'

def __check_conflicts():
    if os.path.exists(os.path.join(git.base_dir, 'conflicts')):
        raise MainException, 'Unsolved conflicts. Please resolve them first'

def __print_crt_patch():
    patch = crt_series.get_current()
    if patch:
        print 'Now at patch "%s"' % patch
    else:
        print 'No patches applied'


#
# Command functions
#
class Command:
    """This class is used to store the command details
    """
    def __init__(self, func, help, usage, option_list):
        self.func = func
        self.help = help
        self.usage = usage
        self.option_list = option_list


def init(parser, options, args):
    """Performs the repository initialisation
    """
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    crt_series.init()

init_cmd = \
         Command(init,
                 'initialise the tree for use with StGIT',
                 '%prog',
                 [])


def add(parser, options, args):
    """Add files or directories to the repository
    """
    if len(args) < 1:
        parser.error('incorrect number of arguments')

    git.add(args)

add_cmd = \
        Command(add,
                'add files or directories to the repository',
                '%prog <files/dirs...>',
                [])


def rm(parser, options, args):
    """Remove files from the repository
    """
    if len(args) < 1:
        parser.error('incorrect number of arguments')

    git.rm(args, options.force)

rm_cmd = \
       Command(rm,
               'remove files from the repository',
               '%prog [options] <files...>',
               [make_option('-f', '--force',
                            help = 'force removing even if the file exists',
                            action = 'store_true')])


def status(parser, options, args):
    """Show the tree status
    """
    git.status(args, options.modified, options.new, options.deleted,
               options.conflict, options.unknown)

status_cmd = \
           Command(status,
                   'show the tree status',
                   '%prog [options] [<files...>]',
                   [make_option('-m', '--modified',
                                help = 'show modified files only',
                                action = 'store_true'),
                    make_option('-n', '--new',
                                help = 'show new files only',
                                action = 'store_true'),
                    make_option('-d', '--deleted',
                                help = 'show deleted files only',
                                action = 'store_true'),
                    make_option('-c', '--conflict',
                                help = 'show conflict files only',
                                action = 'store_true'),
                    make_option('-u', '--unknown',
                                help = 'show unknown files only',
                                action = 'store_true')])


def diff(parser, options, args):
    """Show the tree diff
    """
    if options.revs:
        rev_list = options.revs.split(':')
        rev_list_len = len(rev_list)
        if rev_list_len == 1:
            if rev_list[0][-1] == '/':
                # the whole patch
                rev1 = rev_list[0] + 'bottom'
                rev2 = rev_list[0] + 'top'
            else:
                rev1 = rev_list[0]
                rev2 = None
        elif rev_list_len == 2:
            rev1 = rev_list[0]
            rev2 = rev_list[1]
            if rev2 == '':
                rev2 = 'HEAD'
        else:
            parser.error('incorrect parameters to -r')
    else:
        rev1 = 'HEAD'
        rev2 = None

    if options.stat:
        print git.diffstat(args, __git_id(rev1), __git_id(rev2))
    else:
        git.diff(args, __git_id(rev1), __git_id(rev2))

diff_cmd = \
           Command(diff,
                   'show the tree diff',
                   '%prog [options] [<files...>]\n\n'
                   'The revision format is "([patch]/[bottom | top]) | <tree-ish>"',
                   [make_option('-r', metavar = 'rev1[:[rev2]]', dest = 'revs',
                                help = 'show the diff between revisions'),
                    make_option('-s', '--stat',
                                help = 'show the stat instead of the diff',
                                action = 'store_true')])


def files(parser, options, args):
    """Show the files modified by a patch (or the current patch)
    """
    if len(args) == 0:
        patch = ''
    elif len(args) == 1:
        patch = args[0]
    else:
        parser.error('incorrect number of arguments')

    rev1 = __git_id('%s/bottom' % patch)
    rev2 = __git_id('%s/top' % patch)

    if options.stat:
        print git.diffstat(rev1 = rev1, rev2 = rev2)
    else:
        print git.files(rev1, rev2)

files_cmd = \
          Command(files,
                  'show the files modified by a patch (or the current patch)',
                  '%prog [options] [<patch>]',
                  [make_option('-s', '--stat',
                               help = 'show the diff stat',
                               action = 'store_true')])


def refresh(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    if config.has_option('stgit', 'autoresolved'):
        autoresolved = config.get('stgit', 'autoresolved')
    else:
        autoresolved = 'no'

    if autoresolved != 'yes':
        __check_conflicts()

    patch = crt_series.get_current()
    if not patch:
        raise MainException, 'No patches applied'

    if not options.force:
        __check_head_top_equal()

    if git.local_changes() \
           or not crt_series.head_top_equal() \
           or options.edit or options.message \
           or options.authname or options.authemail or options.authdate \
           or options.commname or options.commemail:
        print 'Refreshing patch "%s"...' % patch,
        sys.stdout.flush()

        if autoresolved == 'yes':
            __resolved_all()
        crt_series.refresh_patch(message = options.message,
                                 edit = options.edit,
                                 author_name = options.authname,
                                 author_email = options.authemail,
                                 author_date = options.authdate,
                                 committer_name = options.commname,
                                 committer_email = options.commemail)

        print 'done'
    else:
        print 'Patch "%s" is already up to date' % patch

refresh_cmd = \
            Command(refresh,
                    'generate a new commit for the current patch',
                    '%prog [options]',
                    [make_option('-f', '--force',
                                 help = 'force the refresh even if HEAD and '\
                                 'top differ',
                                 action = 'store_true'),
                     make_option('-e', '--edit',
                                 help = 'invoke an editor for the patch '\
                                 'description',
                                 action = 'store_true'),
                     make_option('-m', '--message',
                                 help = 'use MESSAGE as the patch ' \
                                 'description'),
                     make_option('--authname',
                                 help = 'use AUTHNAME as the author name'),
                     make_option('--authemail',
                                 help = 'use AUTHEMAIL as the author e-mail'),
                     make_option('--authdate',
                                 help = 'use AUTHDATE as the author date'),
                     make_option('--commname',
                                 help = 'use COMMNAME as the committer name'),
                     make_option('--commemail',
                                 help = 'use COMMEMAIL as the committer ' \
                                 'e-mail')])


def new(parser, options, args):
    """Creates a new patch
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    __check_local_changes()
    __check_conflicts()
    __check_head_top_equal()

    crt_series.new_patch(args[0], message = options.message,
                         author_name = options.authname,
                         author_email = options.authemail,
                         author_date = options.authdate,
                         committer_name = options.commname,
                         committer_email = options.commemail)

new_cmd = \
        Command(new,
                'create a new patch and make it the topmost one',
                '%prog [options] <name>',
                [make_option('-m', '--message',
                             help = 'use MESSAGE as the patch description'),
                 make_option('--authname',
                             help = 'use AUTHNAME as the author name'),
                 make_option('--authemail',
                             help = 'use AUTHEMAIL as the author e-mail'),
                 make_option('--authdate',
                             help = 'use AUTHDATE as the author date'),
                 make_option('--commname',
                             help = 'use COMMNAME as the committer name'),
                 make_option('--commemail',
                             help = 'use COMMEMAIL as the committer e-mail')])

def delete(parser, options, args):
    """Deletes a patch
    """
    if len(args) != 1:
        parser.error('incorrect number of arguments')

    __check_local_changes()
    __check_conflicts()
    __check_head_top_equal()

    crt_series.delete_patch(args[0])
    print 'Patch "%s" successfully deleted' % args[0]
    __print_crt_patch()

delete_cmd = \
           Command(delete,
                   'remove the topmost or any unapplied patch',
                   '%prog <name>',
                   [])


def push(parser, options, args):
    """Pushes the given patch or all onto the series
    """
    # If --undo is passed, do the work and exit
    if options.undo:
        patch = crt_series.get_current()
        if not patch:
            raise MainException, 'No patch to undo'

        print 'Undoing the "%s" push...' % patch,
        sys.stdout.flush()
        __resolved_all()
        crt_series.undo_push()
        print 'done'
        __print_crt_patch()

        return

    __check_local_changes()
    __check_conflicts()
    __check_head_top_equal()

    unapplied = crt_series.get_unapplied()
    if not unapplied:
        raise MainException, 'No more patches to push'

    if options.to:
        boundaries = options.to.split(':')
        if len(boundaries) == 1:
            if boundaries[0] not in unapplied:
                raise MainException, 'Patch "%s" not unapplied' % boundaries[0]
            patches = unapplied[:unapplied.index(boundaries[0])+1]
        elif len(boundaries) == 2:
            if boundaries[0] not in unapplied:
                raise MainException, 'Patch "%s" not unapplied' % boundaries[0]
            if boundaries[1] not in unapplied:
                raise MainException, 'Patch "%s" not unapplied' % boundaries[1]
            lb = unapplied.index(boundaries[0])
            hb = unapplied.index(boundaries[1])
            if lb > hb:
                raise MainException, 'Patch "%s" after "%s"' \
                      % (boundaries[0], boundaries[1])
            patches = unapplied[lb:hb+1]
        else:
            raise MainException, 'incorrect parameters to "--to"'
    elif options.number:
        patches = unapplied[:options.number]
    elif options.all:
        patches = unapplied
    elif len(args) == 0:
        patches = [unapplied[0]]
    elif len(args) == 1:
        patches = [args[0]]
    else:
        parser.error('incorrect number of arguments')

    if patches == []:
        raise MainException, 'No patches to push'

    if options.reverse:
        patches.reverse()

    for p in patches:
        print 'Pushing patch "%s"...' % p,
        sys.stdout.flush()

        crt_series.push_patch(p)

        if crt_series.empty_patch(p):
            print 'done (empty patch)'
        else:
            print 'done'
    __print_crt_patch()

push_cmd = \
         Command(push,
                 'push a patch on top of the series',
                 '%prog [options] [<name>]',
                 [make_option('-a', '--all',
                              help = 'push all the unapplied patches',
                              action = 'store_true'),
                  make_option('-n', '--number', type = 'int',
                              help = 'push the specified number of patches'),
                  make_option('-t', '--to', metavar = 'PATCH1[:PATCH2]',
                              help = 'push all patches to PATCH1 or between '
                              'PATCH1 and PATCH2'),
                  make_option('--reverse',
                              help = 'push the patches in reverse order',
                              action = 'store_true'),
                  make_option('--undo',
                              help = 'undo the last push operation',
                              action = 'store_true')])


def pop(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    __check_local_changes()
    __check_conflicts()
    __check_head_top_equal()

    applied = crt_series.get_applied()
    if not applied:
        raise MainException, 'No patches applied'
    applied.reverse()

    if options.to:
        if options.to not in applied:
            raise MainException, 'Patch "%s" not applied' % options.to
        patches = applied[:applied.index(options.to)]
    elif options.number:
        patches = applied[:options.number]
    elif options.all:
        patches = applied
    else:
        patches = [applied[0]]

    if patches == []:
        raise MainException, 'No patches to pop'

    # pop everything to the given patch
    p = patches[-1]
    if len(patches) == 1:
        print 'Popping patch "%s"...' % p,
    else:
        print 'Popping "%s" - "%s" patches...' % (patches[0], p),
    sys.stdout.flush()

    crt_series.pop_patch(p)

    print 'done'
    __print_crt_patch()

pop_cmd = \
        Command(pop,
                'pop the top of the series',
                '%prog [options]',
                [make_option('-a', '--all',
                             help = 'pop all the applied patches',
                             action = 'store_true'),
                 make_option('-n', '--number', type = 'int',
                             help = 'pop the specified number of patches'),
                 make_option('-t', '--to', metavar = 'PATCH',
                             help = 'pop all patches up to PATCH')])


def __resolved(filename):
    for ext in ['.local', '.older', '.remote']:
        fn = filename + ext
        if os.path.isfile(fn):
            os.remove(fn)

def __resolved_all():
    conflicts = git.get_conflicts()
    if conflicts:
        for filename in conflicts:
            __resolved(filename)
        os.remove(os.path.join(git.base_dir, 'conflicts'))

def resolved(parser, options, args):
    if options.all:
        __resolved_all()
        return

    if len(args) == 0:
        parser.error('incorrect number of arguments')

    conflicts = git.get_conflicts()
    if not conflicts:
        raise MainException, 'No more conflicts'
    # check for arguments validity
    for filename in args:
        if not filename in conflicts:
            raise MainException, 'No conflicts for "%s"' % filename
    # resolved
    for filename in args:
        __resolved(filename)
        del conflicts[conflicts.index(filename)]

    # save or remove the conflicts file
    if conflicts == []:
        os.remove(os.path.join(git.base_dir, 'conflicts'))
    else:
        f = file(os.path.join(git.base_dir, 'conflicts'), 'w+')
        f.writelines([line + '\n' for line in conflicts])
        f.close()

resolved_cmd = \
             Command(resolved,
                     'mark a file conflict as solved',
                     '%prog [options] [<file>[ <file>]]',
                     [make_option('-a', '--all',
                                  help = 'mark all conflicts as solved',
                                  action = 'store_true')])


def series(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    applied = crt_series.get_applied()
    if len(applied) > 0:
        for p in applied [0:-1]:
            if crt_series.empty_patch(p):
                print '0', p
            else:
                print '+', p
        p = applied[-1]

        if crt_series.empty_patch(p):
            print '0>%s' % p
        else:
            print '> %s' % p

    for p in crt_series.get_unapplied():
        if crt_series.empty_patch(p):
            print '0', p
        else:
            print '-', p

series_cmd = \
           Command(series,
                   'print the patch series',
                   '%prog',
                   [])


def applied(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    for p in crt_series.get_applied():
        print p

applied_cmd = \
            Command(applied,
                    'print the applied patches',
                    '%prog',
                    [])


def unapplied(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    for p in crt_series.get_unapplied():
        print p

unapplied_cmd = \
              Command(unapplied,
                      'print the unapplied patches',
                      '%prog',
                      [])


def top(parser, options, args):
    if len(args) != 0:
        parser.error('incorrect number of arguments')

    name = crt_series.get_current()
    if name:
        print name
    else:
        raise MainException, 'No patches applied'

top_cmd = \
        Command(top,
                'print the name of the top patch',
                '%prog',
                [])


def export(parser, options, args):
    if len(args) == 0:
        dirname = 'patches'
    elif len(args) == 1:
        dirname = args[0]
    else:
        parser.error('incorrect number of arguments')

    if git.local_changes():
        print 'Warning: local changes in the tree. ' \
              'You might want to commit them first'

    if not os.path.isdir(dirname):
        os.makedirs(dirname)
    series = file(os.path.join(dirname, 'series'), 'w+')

    patches = crt_series.get_applied()
    num = len(patches)
    zpadding = len(str(num))
    if zpadding < 2:
        zpadding = 2

    patch_no = 1;
    for p in patches:
        pname = p
        if options.diff:
            pname = '%s.diff' % pname
        if options.numbered:
            pname = '%s-%s' % (str(patch_no).zfill(zpadding), pname)
        pfile = os.path.join(dirname, pname)
        print >> series, pname

        # get the template
        patch_tmpl = os.path.join(git.base_dir, 'patchexport.tmpl')
        if os.path.isfile(patch_tmpl):
            tmpl = file(patch_tmpl).read()
        else:
            tmpl = ''

        # get the patch description
        patch = crt_series.get_patch(p)

        tmpl_dict = {'description': patch.get_description().rstrip(),
                     'diffstat': git.diffstat(rev1 = __git_id('%s/bottom' % p),
                                              rev2 = __git_id('%s/top' % p)),
                     'authname': patch.get_authname(),
                     'authemail': patch.get_authemail(),
                     'authdate': patch.get_authdate(),
                     'commname': patch.get_commname(),
                     'commemail': patch.get_commemail()}
        for key in tmpl_dict:
            if not tmpl_dict[key]:
                tmpl_dict[key] = ''

        try:
            descr = tmpl % tmpl_dict
        except KeyError, err:
            raise MainException, 'Unknown patch template variable: %s' \
                  % err
        except TypeError:
            raise MainException, 'Only "%(name)s" variables are ' \
                  'supported in the patch template'
        f = open(pfile, 'w+')
        f.write(descr)
        f.close()

        # write the diff
        git.diff(rev1 = __git_id('%s/bottom' % p),
                 rev2 = __git_id('%s/top' % p),
                 output = pfile, append = True)
        patch_no += 1

    series.close()

export_cmd = \
           Command(export,
                   'exports a series of patches to <dir> (or patches)',
                   '%prog [options] [<dir>]',
                   [make_option('-n', '--numbered',
                                help = 'number the patch names',
                                action = 'store_true'),
                    make_option('-d', '--diff',
                                help = 'append .diff to the patch names',
                                action = 'store_true')])

#
# The commands map
#
commands = {
    'init':     init_cmd,
    'add':      add_cmd,
    'rm':       rm_cmd,
    'status':   status_cmd,
    'diff':     diff_cmd,
    'files':    files_cmd,
    'new':      new_cmd,
    'delete':   delete_cmd,
    'push':     push_cmd,
    'pop':      pop_cmd,
    'resolved': resolved_cmd,
    'series':   series_cmd,
    'applied':  applied_cmd,
    'unapplied':unapplied_cmd,
    'top':      top_cmd,
    'refresh':  refresh_cmd,
    'export':   export_cmd,
    }

def print_help():
    print 'usage: %s <command> [options]' % os.path.basename(sys.argv[0])
    print
    print 'commands:'
    print '  help        print this message'

    cmds = commands.keys()
    cmds.sort()
    for cmd in cmds:
        print '  ' + cmd + ' ' * (12 - len(cmd)) + commands[cmd].help

#
# The main function (command dispatcher)
#
def main():
    """The main function
    """
    global crt_series

    prog = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print >> sys.stderr, 'Unknown command'
        print >> sys.stderr, \
              '  Try "%s help" for a list of supported commands' % prog
        sys.exit(1)

    cmd = sys.argv[1]

    if cmd in ['-h', '--help', 'help']:
        print_help()
        sys.exit(0)
    if cmd in ['-v', '--version']:
        print '%s %s' % (prog, version)
        sys.exit(0)
    if not cmd in commands:
        print >> sys.stderr, 'Unknown command: %s' % cmd
        print >> sys.stderr, '  Try "%s help" for a list of supported commands' \
              % prog
        sys.exit(1)

    # re-build the command line arguments
    sys.argv[0] += ' %s' % cmd
    del(sys.argv[1])

    command = commands[cmd]
    parser = OptionParser(usage = command.usage,
                          option_list = command.option_list)
    options, args = parser.parse_args()
    try:
        crt_series = stack.Series()
        command.func(parser, options, args)
    except (IOError, MainException, stack.StackException, git.GitException), \
               err:
        print >> sys.stderr, '%s %s: %s' % (prog, cmd, err)
        sys.exit(2)

    sys.exit(0)
