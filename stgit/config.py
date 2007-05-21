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

class GitConfigException(Exception):
    pass

class GitConfig:
    __defaults={
        'stgit.autoresolved':	'no',
        'stgit.smtpserver':	'localhost:25',
        'stgit.smtpdelay':	'5',
        'stgit.pullcmd':	'git-pull',
        'stgit.fetchcmd':	'git-fetch',
        'stgit.pull-policy':	'pull',
        'stgit.merger':		'diff3 -L current -L ancestor -L patched -m -E ' \
				'"%(branch1)s" "%(ancestor)s" "%(branch2)s" > "%(output)s"',
        'stgit.autoimerge':	'no',
        'stgit.keeporig':	'yes',
        'stgit.keepoptimized':	'no',
        'stgit.extensions':	'.ancestor .current .patched',
        'stgit.shortnr':	 '5'
        }

    __cache={}

    def __run(self, cmd, args=None):
        """__run: runs cmd using spawnvp.
    
        Runs cmd using spawnvp.  The shell is avoided so it won't mess up
        our arguments.  If args is very large, the command is run multiple
        times; args is split xargs style: cmd is passed on each
        invocation.  Unlike xargs, returns immediately if any non-zero
        return code is received.  
        """
        
        args_l=cmd.split()
        if args is None:
            args = []
        for i in range(0, len(args)+1, 100):
            r=os.spawnvp(os.P_WAIT, args_l[0], args_l + args[i:min(i+100, len(args))])
        if r:
            return r
        return 0
    
    def get(self, name):
        if self.__cache.has_key(name):
            return self.__cache[name]

        stream = os.popen('git repo-config --get %s' % name, 'r')
        value = stream.readline().strip()
        stream.close()
        if len(value) > 0:
            pass
        elif (self.__defaults.has_key(name)):
            value = self.__defaults[name]
        else:
            value = None

        self.__cache[name] = value
        return value

    def getall(self, name):
        if self.__cache.has_key(name):
            return self.__cache[name]

        stream = os.popen('git repo-config --get-all %s' % name, 'r')
        values = [line.strip() for line in stream]
        stream.close()

        self.__cache[name] = values
        return values

    def getint(self, name):
        value = self.get(name)
        if value.isdigit():
            return int(value)
        else:
            raise GitConfigException, 'Value for "%s" is not an integer: "%s"' % (name, value)

    def rename_section(self, from_name, to_name):
        self.__run('git-repo-config --rename-section', [from_name, to_name])
        self.__cache.clear()

    def set(self, name, value):
        self.__run('git-repo-config', [name, value])
        self.__cache[name] = value

    def unset(self, name):
        self.__run('git-repo-config --unset', [name])
        self.__cache[name] = None

    def sections_matching(self, regexp):
        """Takes a regexp with a single group, matches it against all
        config variables, and returns a list whose members are the
        group contents, for all variable names matching the regexp.
        """
        result = []
        stream = os.popen('git repo-config --get-regexp "^%s$"' % regexp, 'r')
        for line in stream:
            m = re.match('^%s ' % regexp, line)
            if m:
                result.append(m.group(1))
        stream.close()
        return result
        
config=GitConfig()

def config_setup():
    global config

    # Set the PAGER environment to the config value (if any)
    pager = config.get('stgit.pager')
    if pager:
        os.environ['PAGER'] = pager
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
