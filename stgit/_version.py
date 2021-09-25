# Adapted from https://github.com/jbweston/miniver
#
# LICENSE: CC0 1.0 Universal license.
# http://creativecommons.org/publicdomain/zero/1.0/
#
# This file is in the public domain.

# -------------------
# Theory of operation
# -------------------
#
# When in the context of a stgit git repository or a stgit git archive
# tarball, this code will dynamically determine the stgit version.
#
# At build-time, this file is overwritten such that stgit distributions
# only contain the __version__ attribute. Obtaining the version from a
# rewritten _version.py is extremely inexpensive at runtime.
#
# The _get_version() function in this module is capable of determining
# the stgit package version in a variety of contexts, including:
#
# * When running stgit from its Git worktree, the version is dynamically
#   determined by _get_version() which runs `git describe`. As such, the
#   version is based on the last tag.
#
# * When running `setup.py build` or `setup.py sdist` from a stgit
#   worktree, the version is dynamically determined from `git describe`
#   as above, BUT the generated build artifacts are seeded with an
#   updated _version.py file containing only this dynamically determined
#   version. Thus stgit distributions contain a static version such that
#   the version can be determined at runtime without consulting Git or
#   any other external resources.
#
# * When running `setup.py` from a stgit source distribution, i.e. an
#   unpacked tarball previously obtained by running `setup.py sdist`,
#   the version is determined statically from a rewritten version of
#   this file (_version.py). A key feature here is that obtaining the
#   version from _version.py is done by exec()ing that file and not
#   importing it as a stgit submodule. This is important at pip
#   install-time when the stgit package cannot (yet) be imported.
#
# * When running an installed version of stgit, the version is also
#   statically determined from a rewritten _version.py.
#
# * When running `setup.py build` or `setup.py sdist` from a Git archive
#   generated with, for example,
#
#       git archive --prefix=stgit/ --output stgit.tar.gz HEAD
#
#   the _version.py file is updated by `git archive` with ref names and
#   the commit hash using the export-subst gitattribute mechanism.
#   _get_version_from_git_archive() is able to construct the version
#   from the substituted ref names and commit id information.

import os
import subprocess
from collections import namedtuple

__all__ = []

_Version = namedtuple("_Version", ("release", "dev", "labels"))


def _get_version():
    package_root = os.path.dirname(os.path.realpath(__file__))
    worktree_path = os.path.dirname(package_root)

    version = _get_version_from_git(worktree_path)
    if version is not None:
        return _pep440_format(version)

    version = _get_version_from_git_archive()
    if version is not None:
        return _pep440_format(version)

    return _pep440_format(_Version("unknown", None, None))


def _pep440_format(version):
    release, dev, labels = version

    parts = [release]
    if dev:
        if release.endswith("-dev") or release.endswith(".dev"):
            parts.append(dev)
        else:  # prefer PEP440 over strict adhesion to semver
            parts.append(".dev{}".format(dev))

    if labels:
        parts.append("+")
        parts.append(".".join(labels))

    return "".join(parts)


def _parse_git_describe(description, worktree_path=None):
    if description.startswith('v'):
        description = description[1:]

    parts = description.split('-', 2)

    if len(parts) == 1:
        # Looks like tagged version from archive
        release = description
        dev = "0"
    elif len(parts) == 3:
        release, dev, git_hash = parts
    else:
        # No tags, only the git hash
        git_hash = 'g' + parts[0]
        release = 'unknown'
        dev = None

    labels = []
    if dev == "0":
        dev = None
    else:
        labels.append(git_hash)

    if worktree_path is not None:
        try:
            p = subprocess.run(["git", "diff", "--quiet"], cwd=worktree_path)
        except OSError:
            labels.append("confused")  # This should never happen.
        else:
            if p.returncode == 1:
                labels.append("dirty")

    return _Version(release, dev, labels)


def _get_version_from_git(worktree_path):
    try:
        p = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            cwd=worktree_path,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
    except OSError:
        return None
    if p.returncode != 0:
        return None
    if not os.path.samefile(p.stdout.decode().rstrip("\n"), worktree_path):
        # The top-level directory of the current Git worktree is not the same
        # as the root directory of the distribution: do not extract the version
        # from Git.
        return None

    # git describe --first-parent does not take into account tags from branches
    # that were merged-in. The '--long' flag gets us the 'dev' version and
    # git hash, '--always' returns the git hash even if there are no tags.
    for opts in [["--first-parent"], []]:
        try:
            p = subprocess.run(
                ["git", "describe", "--long", "--always"] + opts,
                cwd=worktree_path,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
        except OSError:
            return None
        if p.returncode == 0:
            break
    else:
        return None

    description = p.stdout.decode().strip()

    return _parse_git_describe(description, worktree_path)


def _get_version_from_git_archive():
    # git archive replaces these export-subst placeholders.
    refnames = "$Format:%D$"
    git_hash = "$Format:%h$"
    describe = "$Format:%(describe:match=v*)$"

    if not describe.startswith('$Format'):
        # As of Git 2.32, the %(describe) is a valid export-subst placeholder
        return _parse_git_describe(describe, worktree_path=None)

    # Older versions of Git can only provide either a tag if the archive is
    # from a tagged commit, or the commit id without information about the
    # preceeding tag for archives from a non-tagged commit.

    if git_hash.startswith("$Format") or refnames.startswith("$Format"):
        # variables not expanded during 'git archive'
        return None

    vtag = "tag: v"
    refs = set(r.strip() for r in refnames.split(","))
    version_tags = set(r[len(vtag) :] for r in refs if r.startswith(vtag))
    if version_tags:
        release = sorted(version_tags)[0]  # prefer e.g. "2.0" over "2.0rc1"
        return _Version(release, dev=None, labels=None)
    else:
        return _Version("unknown", dev=None, labels=["g" + git_hash])


__version__ = _get_version()
