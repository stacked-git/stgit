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


if 'GIT_DIR' in os.environ:
    __git_dir = os.environ['GIT_DIR']
else:
    __git_dir = '.git'

config = ConfigParser.RawConfigParser()

# Set the defaults
config.add_section('stgit')
config.set('stgit', 'autoresolved', 'no')
config.set('stgit', 'smtpserver', 'localhost:25')
config.set('stgit', 'smtpdelay', '2')
config.set('stgit', 'merger',
           'diff3 -L local -L older -L remote -m -E ' \
           '"%(branch1)s" "%(ancestor)s" "%(branch2)s" > "%(output)s"')
config.set('stgit', 'keeporig', 'yes')

# Read the configuration files (if any) and override the default settings
config.read('/etc/stgitrc')
config.read(os.path.expanduser('~/.stgitrc'))
config.read(os.path.join(__git_dir, 'stgitrc'))

# [gitmergeonefile] section is deprecated. In case it exists copy the
# options/values to the [stgit] one
if config.has_section('gitmergeonefile'):
    for option, value in config.items('gitmergeonefile'):
        config.set('stgit', option, value)
