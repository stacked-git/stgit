# -*- coding: utf-8 -*-
"""Template files look-up"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)
import io
import os
import sys

from stgit import basedir
from stgit.compat import text

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


def get_template(tfile):
    """Return the string in the template file passed as argument or
    None if the file wasn't found.
    """
    tmpl_dirs = [
        basedir.get(),
        os.path.join(os.path.expanduser('~'), '.stgit', 'templates'),
        os.path.join(sys.prefix, 'share', 'stgit', 'templates'),
    ]

    for d in tmpl_dirs:
        tmpl_path = os.path.join(d, tfile)
        if os.path.isfile(tmpl_path):
            with io.open(tmpl_path, 'rb') as f:
                return f.read()
    else:
        return None


def specialize_template(tmpl, tmpl_dict):
    tmpl_dict_b = {}
    for k, v in tmpl_dict.items():
        if isinstance(k, text):
            k = k.encode('utf-8')
        if v is None:
            v = b''
        elif isinstance(v, text):
            v = v.encode('utf-8')
        tmpl_dict_b[k] = v
    return tmpl % tmpl_dict_b
