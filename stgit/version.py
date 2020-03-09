# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import os
import re
import sys

from stgit.run import Run, RunException


class VersionUnavailable(Exception):
    pass


def git_describe_version():
    root = sys.path[0] if sys.path[0] else None
    try:
        v = (
            Run('git', 'describe', '--tags', '--abbrev=4')
            .cwd(root)
            .discard_stderr()
            .output_one_line()
        )
    except RunException as e:
        raise VersionUnavailable(str(e))
    m = re.match(r'^v([0-9].*)', v)
    if m:
        v = m.group(1)
    else:
        raise VersionUnavailable('bad version: %s' % v)
    try:
        dirty = (
            Run('git', 'diff-index', '--name-only', 'HEAD')
            .cwd(root)
            .discard_stderr()
            .raw_output()
        )
    except RunException as e:
        raise VersionUnavailable(str(e))
    if dirty:
        v += '-dirty'
    return v


def git_archival_version():
    archival_path = os.path.join(sys.path[0], '.git_archival.txt')
    if not os.path.isfile(archival_path):
        # The archival file will not be present in sdist archives.
        raise VersionUnavailable('%s does not exist' % archival_path)
    tag_re = re.compile(r'(?<=\btag: )([^,]+)\b')
    with open(archival_path) as f:
        for line in f:
            if line.startswith('ref-names:'):
                for tag in tag_re.findall(line):
                    if tag.startswith('v'):
                        return tag[1:]
        else:
            raise VersionUnavailable('no tags found in %s' % archival_path)


def get_builtin_version():
    try:
        import stgit.builtin_version
    except ImportError:
        raise VersionUnavailable('could not import stgit.builtin_version')
    else:
        return stgit.builtin_version.version


def get_version():
    for v in [get_builtin_version, git_describe_version, git_archival_version]:
        try:
            return v()
        except VersionUnavailable:
            pass
    return 'unknown-version'


# minimum version requirements
git_min_ver = '1.5.2'
python_min_ver = '2.6'
