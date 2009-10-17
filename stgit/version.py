from stgit.exception import StgException
from stgit import run, utils
import os, os.path, re, sys

class VersionUnavailable(StgException):
    pass

def git_describe_version():
    path = sys.path[0]
    try:
        v = run.Run('git', 'describe', '--tags', '--abbrev=4'
                    ).cwd(path).output_one_line()
    except run.RunException, e:
        raise VersionUnavailable(str(e))
    if not re.match(r'^v[0-9]', v):
        raise VersionUnavailable('%s: bad version' % v)
    try:
        dirty = run.Run('git', 'diff-index', '--name-only', 'HEAD'
                        ).cwd(path).raw_output()
    except run.RunException, e:
        raise VersionUnavailable(str(e))
    if dirty:
        v += '-dirty'
    return utils.strip_prefix('v', v)

def builtin_version():
    try:
        import builtin_version as bv
    except ImportError:
        raise VersionUnavailable()
    else:
        return bv.version

def _builtin_version_file(ext = 'py'):
    return os.path.join(sys.path[0], 'stgit', 'builtin_version.%s' % ext)

def write_builtin_version():
    try:
        v = git_describe_version()
    except VersionUnavailable:
        return
    f = file(_builtin_version_file(), 'w')
    f.write('# This file was generated automatically. Do not edit by hand.\n'
            'version = %r\n' % v)

def get_version():
    for v in [builtin_version, git_describe_version]:
        try:
            return v()
        except VersionUnavailable:
            pass
    return 'unknown-version'

version = get_version()

# minimum version requirements
git_min_ver = '1.5.2'
python_min_ver = '2.4'
