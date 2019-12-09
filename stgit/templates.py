# -*- coding: utf-8 -*-
"""Template files look-up"""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import io
import os
import sys

from stgit.run import Run

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
        Run('git', 'rev-parse', '--git-dir').output_one_line(),
        os.path.join(os.path.expanduser('~'), '.stgit', 'templates'),
        os.path.join(sys.prefix, 'share', 'stgit', 'templates'),
        os.path.join(os.path.dirname(__file__), 'templates'),
    ]

    for d in tmpl_dirs:
        tmpl_path = os.path.join(d, tfile)
        if os.path.isfile(tmpl_path):
            with io.open(tmpl_path, 'r') as f:
                return f.read()
    else:
        return None


def specialize_template(tmpl, tmpl_dict):
    """Specialize template string using template dict.

    Returns specialized template as bytes.

    Since Python 3.3 and 3.4 do not support the interpolation operator (%) on
    bytes objects; and since we expect at least one tmpl_dict value (diff) to
    be bytes (not str); we use a recursive approach to specialize the str
    specifiers using normal interpolation while handling interpolation of bytes
    values ourselves.

    """
    for k, v in tmpl_dict.items():
        if v is None:
            tmpl_dict[k] = ''
        elif isinstance(v, bytes):
            tmpl_dict.pop(k)
            return v.join(specialize_template(part, tmpl_dict)
                          for part in tmpl.split('%%(%s)s' % k))
    else:
        return (tmpl % tmpl_dict).encode('utf-8')
