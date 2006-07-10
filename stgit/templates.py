"""Template files look-up
"""

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

import sys, os

from stgit import basedir


def get_template(tfile):
    """Return the string in the template file passed as argument or
    None if the file wasn't found.
    """
    tmpl_list = [ os.path.join(basedir.get(), tfile),
                  os.path.join(os.path.expanduser('~'), '.stgit', 'templates',
                               tfile),
                  os.path.join(sys.prefix, 'share', 'stgit', 'templates',
                               tfile) ]

    tmpl = None
    for t in tmpl_list:
        if os.path.isfile(t):
            tmpl = file(t).read()
            break

    return tmpl
