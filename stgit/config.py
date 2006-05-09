"""Handles the Stacked GIT configuration files
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

import os, ConfigParser

from stgit import basedir


config = ConfigParser.RawConfigParser()

# Set the defaults
config.add_section('stgit')
config.set('stgit', 'autoresolved', 'no')
config.set('stgit', 'smtpserver', 'localhost:25')
config.set('stgit', 'smtpdelay', '2')
config.set('stgit', 'merger',
           'diff3 -L current -L ancestor -L patched -m -E ' \
           '"%(branch1)s" "%(ancestor)s" "%(branch2)s" > "%(output)s"')
config.set('stgit', 'keeporig', 'yes')
config.set('stgit', 'extensions', '.ancestor .current .patched')

# Read the configuration files (if any) and override the default settings
config.read('/etc/stgitrc')
config.read(os.path.expanduser('~/.stgitrc'))
config.read(os.path.join(basedir.get(), 'stgitrc'))

# Set the PAGER environment to the config value (if any)
if config.has_option('stgit', 'pager'):
    os.environ['PAGER'] = config.get('stgit', 'pager')

# [gitmergeonefile] section is deprecated. In case it exists copy the
# options/values to the [stgit] one
if config.has_section('gitmergeonefile'):
    for option, value in config.items('gitmergeonefile'):
        config.set('stgit', option, value)


# cached extensions
__extensions = None

def file_extensions():
    """Returns a dictionary with the conflict file extensions
    """
    global __extensions

    if not __extensions:
        cfg_ext = config.get('stgit', 'extensions').split()
        if len(cfg_ext) != 3:
            raise CmdException, '"extensions" configuration error'

        __extensions = { 'ancestor': cfg_ext[0],
                         'current':  cfg_ext[1],
                         'patched':  cfg_ext[2] }

    return __extensions
