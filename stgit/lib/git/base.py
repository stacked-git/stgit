# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from stgit.run import Run
from stgit.utils import add_dict


class Immutable(object):
    """I{Immutable} objects cannot be modified once created. Any
    modification methods will return a new object, leaving the
    original object as it was.

    The reason for this is that we want to be able to represent git
    objects, which are immutable, and want to be able to create new
    git objects that are just slight modifications of other git
    objects. (Such as, for example, modifying the commit message of a
    commit object while leaving the rest of it intact. This involves
    creating a whole new commit object that's exactly like the old one
    except for the commit message.)

    The L{Immutable} class doesn't actually enforce immutability --
    that is up to the individual immutable subclasses. It just serves
    as documentation."""


class ImmutableDict(dict):
    """A dictionary that cannot be modified once it has been created."""

    def error(*args, **kwargs):
        raise TypeError('Cannot modify immutable dict')

    __delitem__ = error
    __setitem__ = error
    clear = error
    pop = error
    popitem = error
    setdefault = error
    update = error


class NoValue(object):
    """Value guaranteed to be distinct from any real argument value."""


def make_defaults(defaults):
    def d(val, attr, default_fun=lambda: None):
        if val != NoValue:
            return val
        elif defaults != NoValue:
            return getattr(defaults, attr)
        else:
            return default_fun()

    return d


class RunWithEnv(object):
    def run(self, args, env={}):
        """Run the given command with an environment given by self.env.

        @type args: list of strings
        @param args: Command and argument vector
        @type env: dict
        @param env: Extra environment"""
        return Run(*args).env(add_dict(self.env, env))


class RunWithEnvCwd(RunWithEnv):
    def run(self, args, env={}):
        """Run the given command with an environment given by self.env, and
        current working directory given by self.cwd.

        @type args: list of strings
        @param args: Command and argument vector
        @type env: dict
        @param env: Extra environment"""
        return RunWithEnv.run(self, args, env).cwd(self.cwd)

    def run_in_cwd(self, args):
        """Run the given command with an environment given by self.env and
        self.env_in_cwd, without changing the current working
        directory.

        @type args: list of strings
        @param args: Command and argument vector"""
        return RunWithEnv.run(self, args, self.env_in_cwd)
