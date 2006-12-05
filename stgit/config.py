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
from StringIO import StringIO
from stgit import basedir

config = ConfigParser.RawConfigParser()

def git_config(filename):
    """Open a git config file and convert it to be understood by
    Python."""
    try:
        f = file(filename)
        cont = False
        lines = []
        for line in f:
            line = line.strip()

            if cont:
                # continued line, add a space at the beginning
                line = ' ' + line

            if line and line[-1] == '\\':
                line = line[:-1].rstrip()
                cont = True
            else:
                line = line + '\n'
                cont = False

            lines.append(line)

        f.close()
        cfg_str = ''.join(lines)
    except IOError:
        cfg_str = ''

    strio = StringIO(cfg_str)
    strio.name = filename

    return strio

def config_setup():
    global config

    # Set the defaults
    config.add_section('stgit')
    config.set('stgit', 'autoresolved', 'no')
    config.set('stgit', 'smtpserver', 'localhost:25')
    config.set('stgit', 'smtpdelay', '5')
    config.set('stgit', 'pullcmd', 'git-pull')
    config.set('stgit', 'merger',
               'diff3 -L current -L ancestor -L patched -m -E ' \
               '"%(branch1)s" "%(ancestor)s" "%(branch2)s" > "%(output)s"')
    config.set('stgit', 'keeporig', 'yes')
    config.set('stgit', 'keepoptimized', 'no')
    config.set('stgit', 'extensions', '.ancestor .current .patched')

    # Read the configuration files (if any) and override the default settings
    # stgitrc are read for backward compatibility
    config.read('/etc/stgitrc')
    config.read(os.path.expanduser('~/.stgitrc'))
    config.read(os.path.join(basedir.get(), 'stgitrc'))

    # GIT configuration files can have a [stgit] section
    try:
        global_config = os.environ['GIT_CONFIG']
    except KeyError:
        global_config = os.path.expanduser('~/.gitconfig')
    try:
        local_config = os.environ['GIT_CONFIG_LOCAL']
    except KeyError:
        local_config = os.path.join(basedir.get(), 'config')
    config.readfp(git_config(global_config))
    config.readfp(git_config(local_config))

    # Set the PAGER environment to the config value (if any)
    if config.has_option('stgit', 'pager'):
        os.environ['PAGER'] = config.get('stgit', 'pager')

    # [gitmergeonefile] section is deprecated. In case it exists copy the
    # options/values to the [stgit] one
    if config.has_section('gitmergeonefile'):
        for option, value in config.items('gitmergeonefile'):
            config.set('stgit', option, value)


class ConfigOption:
    """Delayed cached reading of a configuration option.
    """
    def __init__(self, section, option):
        self.__section = section
        self.__option = option
        self.__value = None

    def __str__(self):
        if not self.__value:
            self.__value = config.get(self.__section, self.__option)
        return self.__value


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
