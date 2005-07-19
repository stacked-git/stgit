"""Export command
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

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git


help = 'exports a series of patches to <dir> (or patches)'
usage = """%prog [options] [<dir>]"""

options = [make_option('-n', '--numbered',
                       help = 'number the patch names',
                       action = 'store_true'),
           make_option('-d', '--diff',
                       help = 'append .diff to the patch names',
                       action = 'store_true'),
           make_option('-t', '--template', metavar = 'FILE',
                       help = 'Use FILE as a template'),
           make_option('-r', '--range',
                       metavar = '[PATCH1][:[PATCH2]]',
                       help = 'export patches between PATCH1 and PATCH2')]


def func(parser, options, args):
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

    applied = crt_series.get_applied()

    if options.range:
        boundaries = options.range.split(':')
        if len(boundaries) == 1:
            start = boundaries[0]
            stop = boundaries[0]
        elif len(boundaries) == 2:
            if boundaries[0] == '':
                start = applied[0]
            else:
                start = boundaries[0]
            if boundaries[1] == '':
                stop = applied[-1]
            else:
                stop = boundaries[1]
        else:
            raise CmdException, 'incorrect parameters to "--range"'

        if start in applied:
            start_idx = applied.index(start)
        else:
            raise CmdException, 'Patch "%s" not applied' % start
        if stop in applied:
            stop_idx = applied.index(stop) + 1
        else:
            raise CmdException, 'Patch "%s" not applied' % stop

        if start_idx >= stop_idx:
            raise CmdException, 'Incorrect patch range order'
    else:
        start_idx = 0
        stop_idx = len(applied)

    patches = applied[start_idx:stop_idx]

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
        if options.template:
            patch_tmpl = options.template
        else:
            patch_tmpl = os.path.join(git.base_dir, 'patchexport.tmpl')
        if os.path.isfile(patch_tmpl):
            tmpl = file(patch_tmpl).read()
        else:
            tmpl = ''

        # get the patch description
        patch = crt_series.get_patch(p)

        tmpl_dict = {'description': patch.get_description().rstrip(),
                     'diffstat': git.diffstat(rev1 = git_id('%s/bottom' % p),
                                              rev2 = git_id('%s/top' % p)),
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
            raise CmdException, 'Unknown patch template variable: %s' \
                  % err
        except TypeError:
            raise CmdException, 'Only "%(name)s" variables are ' \
                  'supported in the patch template'
        f = open(pfile, 'w+')
        f.write(descr)

        # write the diff
        git.diff(rev1 = git_id('%s/bottom' % p),
                 rev2 = git_id('%s/top' % p),
                 out_fd = f)
        f.close()
        patch_no += 1

    series.close()
