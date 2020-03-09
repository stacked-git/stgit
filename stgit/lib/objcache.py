# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals


class ObjectCache(object):
    """Cache for Python objects, for making sure that we create only one
    Python object per git object. This reduces memory consumption and
    makes object comparison very cheap."""

    def __init__(self, create):
        self.__objects = {}
        self.__create = create

    def __getitem__(self, name):
        if name not in self.__objects:
            self.__objects[name] = self.__create(name)
        return self.__objects[name]

    def __contains__(self, name):
        return name in self.__objects

    def __setitem__(self, name, val):
        assert name not in self.__objects
        self.__objects[name] = val
