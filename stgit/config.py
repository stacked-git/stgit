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

import os, re
from stgit import basedir
from stgit.exception import *
from stgit.run import *

class GitConfigException(StgException):
    pass

class GitConfig:
    __defaults={
        'stgit.smtpserver':     ['localhost:25'],
        'stgit.smtpdelay':      ['5'],
        'stgit.pullcmd':        ['git pull'],
        'stgit.fetchcmd':       ['git fetch'],
        'stgit.pull-policy':    ['pull'],
        'stgit.autoimerge':     ['no'],
        'stgit.keepoptimized':  ['no'],
        'stgit.shortnr':        ['5'],
        'stgit.pager':          ['less'],
        'stgit.alias.add':      ['git add'],
        'stgit.alias.rm':       ['git rm'],
        'stgit.alias.mv':       ['git mv'],
        'stgit.alias.resolved': ['git add'],
        'stgit.alias.status':   ['git status -s']
        }

    __cache = None

    def load(self):
        """Load the whole configuration in __cache unless it has been
        done already."""
        if self.__cache is not None:
            return
        self.__cache = self.__defaults
        lines = Run('git', 'config', '--null', '--list'
                   ).discard_exitcode().raw_output()
        for line in filter(None, lines.split('\0')):
            key, value = line.split('\n', 1)
            self.__cache.setdefault(key, []).append(value)

    def get(self, name):
        self.load()
        try:
            return self.__cache[name][-1]
        except KeyError:
            return None

    def getall(self, name):
        self.load()
        try:
            return self.__cache[name]
        except KeyError:
            return []

    def getint(self, name):
        value = self.get(name)
        if value == None:
            return None
        elif value.isdigit():
            return int(value)
        else:
            raise GitConfigException, 'Value for "%s" is not an integer: "%s"' % (name, value)

    def getstartswith(self, name):
        self.load()
        return ((n, v[-1]) for (n, v) in self.__cache.iteritems()
                if n.startswith(name))

    def rename_section(self, from_name, to_name):
        """Rename a section in the config file. Silently do nothing if
        the section doesn't exist."""
        Run('git', 'config', '--rename-section', from_name, to_name
            ).returns([0, 1, 128]).run()
        self.__cache.clear()

    def remove_section(self, name):
        """Remove a section in the config file. Silently do nothing if
        the section doesn't exist."""
        Run('git', 'config', '--remove-section', name
            ).returns([0, 1, 128]).discard_stderr().discard_output()
        self.__cache.clear()

    def set(self, name, value):
        Run('git', 'config', name, value).run()
        self.__cache[name] = value

    def unset(self, name):
        Run('git', 'config', '--unset', name).run()
        self.__cache[name] = [None]

    def sections_matching(self, regexp):
        """Takes a regexp with a single group, matches it against all
        config variables, and returns a list whose members are the
        group contents, for all variable names matching the regexp.
        """
        result = []
        for line in Run('git', 'config', '--get-regexp', '"^%s$"' % regexp
                        ).returns([0, 1]).output_lines():
            m = re.match('^%s ' % regexp, line)
            if m:
                result.append(m.group(1))
        return result

    def get_colorbool(self, name, stdout_is_tty):
        """Invoke 'git config --get-colorbool' and return the result."""
        return Run('git', 'config', '--get-colorbool', name,
                   stdout_is_tty).output_one_line()
        
config=GitConfig()

def config_setup():
    global config

    os.environ.setdefault('PAGER', config.get('stgit.pager'))
    os.environ.setdefault('LESS', '-FRSX')
    # FIXME: handle EDITOR the same way ?

class ConfigOption:
    """Delayed cached reading of a configuration option.
    """
    def __init__(self, section, option):
        self.__section = section
        self.__option = option
        self.__value = None

    def __str__(self):
        if not self.__value:
            self.__value = config.get(self.__section + '.' + self.__option)
        return self.__value


# cached extensions
__extensions = None

def file_extensions():
    """Returns a dictionary with the conflict file extensions
    """
    global __extensions

    if not __extensions:
        cfg_ext = config.get('stgit.extensions').split()
        if len(cfg_ext) != 3:
            raise CmdException, '"extensions" configuration error'

        __extensions = { 'ancestor': cfg_ext[0],
                         'current':  cfg_ext[1],
                         'patched':  cfg_ext[2] }

    return __extensions
