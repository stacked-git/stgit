# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)


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
