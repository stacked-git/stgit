#!/usr/bin/env python

import glob
from distutils.core import setup

from stgit.version import version

setup(name = 'stgit',
      version = version,
      license = 'GPLv2',
      author = 'Catalin Marinas',
      author_email = 'catalin.marinas@gmail.org',
      url = 'http://www.procode.org/stgit/',
      description = 'Stacked GIT',
      long_description = 'Push/pop utility on top of GIT',
      scripts = ['stg', 'gitmergeonefile.py'],
      packages = ['stgit', 'stgit.commands'],
      data_files = [('share/stgit/templates', glob.glob('templates/*.tmpl')),
                    ('share/stgit/examples', glob.glob('examples/*.tmpl')),
                    ('share/stgit/examples', ['examples/stgitrc'])]
      )
