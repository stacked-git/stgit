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
from stgit import stack, git, templates


help = 'exports patches to a directory'
usage = """%prog [options] [<patch1>] [<patch2>] [<patch3>..<patch4>]

Export a range of applied patches to a given directory (defaults to
'patches-<branch>') in a standard unified GNU diff format. A template
file (defaulting to '.git/patchexport.tmpl' or
'~/.stgit/templates/patchexport.tmpl' or
'/usr/share/stgit/templates/patchexport.tmpl') can be used for the
patch format. The following variables are supported in the template
file:

  %(description)s - patch description
  %(shortdescr)s  - the first line of the patch description
  %(longdescr)s   - the rest of the patch description, after the first line
  %(diffstat)s    - the diff statistics
  %(authname)s    - author's name
  %(authemail)s   - author's e-mail
  %(authdate)s    - patch creation date
  %(commname)s    - committer's name
  %(commemail)s   - committer's e-mail
"""

options = [make_option('-d', '--dir',
                       help = 'export patches to DIR instead of the default'),
           make_option('-p', '--patch',
                       help = 'append .patch to the patch names',
                       action = 'store_true'),
           make_option('-e', '--extension',
                       help = 'append .EXTENSION to the patch names'),
           make_option('-n', '--numbered',
                       help = 'prefix the patch names with order numbers',
                       action = 'store_true'),
           make_option('-t', '--template', metavar = 'FILE',
                       help = 'Use FILE as a template'),
           make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-O', '--diff-opts',
                       help = 'options to pass to git-diff'),
           make_option('-s', '--stdout',
                       help = 'dump the patches to the standard output',
                       action = 'store_true')]


def func(parser, options, args):
    """Export a range of patches.
    """
    if options.dir:
        dirname = options.dir
    else:
        dirname = 'patches-%s' % crt_series.get_name()

    if not options.branch and git.local_changes():
        out.warn('Local changes in the tree;'
                 ' you might want to commit them first')

    if not options.stdout:
        if not os.path.isdir(dirname):
            os.makedirs(dirname)
        series = file(os.path.join(dirname, 'series'), 'w+')

    if options.diff_opts:
        diff_flags = options.diff_opts.split()
    else:
        diff_flags = []

    applied = crt_series.get_applied()
    if len(args) != 0:
        patches = parse_patches(args, applied)
    else:
        patches = applied

    num = len(patches)
    if num == 0:
        raise CmdException, 'No patches applied'

    zpadding = len(str(num))
    if zpadding < 2:
        zpadding = 2

    # get the template
    if options.template:
        tmpl = file(options.template).read()
    else:
        tmpl = templates.get_template('patchexport.tmpl')
        if not tmpl:
            tmpl = ''

    # note the base commit for this series
    if not options.stdout:
        base_commit = crt_series.get_patch(patches[0]).get_bottom()
        print >> series, '# This series applies on GIT commit %s' % base_commit

    patch_no = 1;
    for p in patches:
        pname = p
        if options.patch:
            pname = '%s.patch' % pname
        elif options.extension:
            pname = '%s.%s' % (pname, options.extension)
        if options.numbered:
            pname = '%s-%s' % (str(patch_no).zfill(zpadding), pname)
        pfile = os.path.join(dirname, pname)
        if not options.stdout:
            print >> series, pname

        # get the patch description
        patch = crt_series.get_patch(p)

        descr = patch.get_description().strip()
        descr_lines = descr.split('\n')

        short_descr = descr_lines[0].rstrip()
        long_descr = reduce(lambda x, y: x + '\n' + y,
                            descr_lines[1:], '').strip()

        tmpl_dict = {'description': patch.get_description().rstrip(),
                     'shortdescr': short_descr,
                     'longdescr': long_descr,
                     'diffstat': git.diffstat(rev1 = patch.get_bottom(),
                                              rev2 = patch.get_top()),
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

        if options.stdout:
            f = sys.stdout
        else:
            f = open(pfile, 'w+')

        if options.stdout and num > 1:
            print '-'*79
            print patch.get_name()
            print '-'*79

        # write description
        f.write(descr)
        # write the diff
        git.diff(rev1 = patch.get_bottom(),
                 rev2 = patch.get_top(),
                 out_fd = f,
                 diff_flags = diff_flags )
        if not options.stdout:
            f.close()
        patch_no += 1

    if not options.stdout:
        series.close()
