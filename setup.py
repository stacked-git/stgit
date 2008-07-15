#!/usr/bin/env python

import sys, glob, os
from distutils.core import setup

from stgit.version import version, git_min_ver, python_min_ver

def __version_to_list(version):
    """Convert a version string to a list of numbers or strings
    """
    ver_list = []
    for p in version.split('.'):
        try:
            n = int(p)
        except ValueError:
            n = p
        ver_list.append(n)
    return ver_list

def __check_min_version(min_ver, ver):
    """Check whether ver is greater or equal to min_ver
    """
    min_ver_list = __version_to_list(min_ver)
    ver_list = __version_to_list(ver)
    return min_ver_list <= ver_list

def __check_python_version():
    """Check the minimum Python version
    """
    pyver = '.'.join(map(lambda x: str(x), sys.version_info))
    if not __check_min_version(python_min_ver, pyver):
        print >> sys.stderr, 'Python version %s or newer required. Found %s' \
              % (python_min_ver, pyver)
        sys.exit(1)

def __check_git_version():
    """Check the minimum GIT version
    """
    from stgit.run import Run
    gitver = Run('git', '--version').output_one_line().split()[2]
    if not __check_min_version(git_min_ver, gitver):
        print >> sys.stderr, 'GIT version %s or newer required. Found %s' \
              % (git_min_ver, gitver)
        sys.exit(1)

# Check the minimum versions required
if sys.argv[1] in ['install', 'build']:
    __check_python_version()
    __check_git_version()

# ensure readable template files
old_mask = os.umask(0022)

setup(name = 'stgit',
      version = version,
      license = 'GPLv2',
      author = 'Catalin Marinas',
      author_email = 'catalin.marinas@gmail.com',
      url = 'http://www.procode.org/stgit/',
      description = 'Stacked GIT',
      long_description = 'Push/pop utility on top of GIT',
      scripts = ['stg'],
      packages = ['stgit', 'stgit.commands'],
      data_files = [('share/stgit/templates', glob.glob('templates/*.tmpl')),
                    ('share/stgit/examples', glob.glob('examples/*.tmpl')),
                    ('share/stgit/examples', ['examples/gitconfig']),
                    ('share/stgit/contrib', ['contrib/diffcol.sh',
                                             'contrib/stgbashprompt.sh',
                                             'contrib/stgit-completion.bash']),
                    ('share/doc/stgit', glob.glob('doc/*.txt'))]
      )

# restore the old mask
os.umask(old_mask)
