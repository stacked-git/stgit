# -*- coding: utf-8 -*-
"""Export command"""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import io
import os
import sys

from stgit import templates
from stgit.argparse import diff_opts_option, opt, patch_range
from stgit.commands import common
from stgit.lib.git import diffstat
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

help = 'Export patches to a directory'
kind = 'patch'
usage = ['[options] [--] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = """
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
  %(commemail)s   - committer's e-mail"""

args = [patch_range('applied_patches', 'unapplied_patches', 'hidden_patches')]
options = [
    opt(
        '-d',
        '--dir',
        args=['dir'],
        short='Export patches to DIR instead of the default',
    ),
    opt(
        '-p',
        '--patch',
        action='store_true',
        short='Append .patch to the patch names',
    ),
    opt('-e', '--extension', short='Append .EXTENSION to the patch names'),
    opt(
        '-n',
        '--numbered',
        action='store_true',
        short='Prefix the patch names with order numbers',
    ),
    opt(
        '-t',
        '--template',
        metavar='FILE',
        args=['files'],
        short='Use FILE as a template',
    ),
    opt(
        '-b',
        '--branch',
        args=['stg_branches'],
        short='Use BRANCH instead of the default branch',
    ),
    opt(
        '-s',
        '--stdout',
        action='store_true',
        short='Dump the patches to the standard output',
    ),
] + diff_opts_option()

directory = common.DirectoryHasRepositoryLib()


def func(parser, options, args):
    """Export a range of patches.
    """
    repository = directory.repository
    stack = repository.get_stack(options.branch)

    if options.dir:
        dirname = options.dir
    else:
        dirname = 'patches-%s' % stack.name
        directory.cd_to_topdir()

    if (
        not options.branch
        and repository.default_iw.changed_files(
            repository.rev_parse('HEAD').data.tree
        )
    ):
        out.warn(
            'Local changes in the tree; you might want to commit them first'
        )

    applied = stack.patchorder.applied
    unapplied = stack.patchorder.unapplied
    if len(args) != 0:
        patches = common.parse_patches(args, applied + unapplied, len(applied))
    else:
        patches = applied

    num = len(patches)
    if num == 0:
        raise common.CmdException('No patches applied')

    zpadding = len(str(num))
    if zpadding < 2:
        zpadding = 2

    # get the template
    if options.template:
        with io.open(options.template, 'r') as f:
            tmpl = f.read()
    else:
        tmpl = templates.get_template('patchexport.tmpl')
        if not tmpl:
            tmpl = ''

    if not options.stdout:
        if not os.path.isdir(dirname):
            os.makedirs(dirname)
        series = io.open(os.path.join(dirname, 'series'), 'w')
        # note the base commit for this series
        base_commit = stack.base.sha1
        print(
            '# This series applies on GIT commit %s' % base_commit,
            file=series,
        )

    for patch_no, p in enumerate(patches, 1):
        pname = p
        if options.patch:
            pname = '%s.patch' % pname
        elif options.extension:
            pname = '%s.%s' % (pname, options.extension)
        if options.numbered:
            pname = '%s-%s' % (str(patch_no).zfill(zpadding), pname)
        pfile = os.path.join(dirname, pname)
        if not options.stdout:
            print(pname, file=series)

        # get the patch description
        patch = stack.patches.get(p)
        cd = patch.commit.data

        descr = cd.message.strip()
        descr_lines = descr.split('\n')

        short_descr = descr_lines[0].rstrip()
        long_descr = '\n'.join(descr_lines[1:]).strip()

        diff = stack.repository.diff_tree(
            cd.parent.data.tree,
            cd.tree,
            options.diff_flags,
        )

        tmpl_dict = {
            'description': descr,
            'shortdescr': short_descr,
            'longdescr': long_descr,
            'diffstat': diffstat(diff).rstrip(),
            'authname': cd.author.name,
            'authemail': cd.author.email,
            'authdate': cd.author.date.isoformat(),
            'commname': cd.committer.name,
            'commemail': cd.committer.email,
        }

        try:
            descr = templates.specialize_template(tmpl, tmpl_dict)
        except KeyError as err:
            raise common.CmdException('Unknown patch template variable: %s' %
                                      err)
        except TypeError:
            raise common.CmdException('Only "%(name)s" variables are '
                                      'supported in the patch template')

        if options.stdout:
            if hasattr(sys.stdout, 'buffer'):
                f = sys.stdout.buffer
            else:
                f = sys.stdout
        else:
            f = io.open(pfile, 'wb')

        if options.stdout and num > 1:
            f.write('\n'.join(['-' * 79,
                               patch.name,
                               '-' * 79,
                               '']).encode('utf-8'))

        f.write(descr)
        f.write(diff)
        if not options.stdout:
            f.close()

    if not options.stdout:
        series.close()
