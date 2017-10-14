# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)


class StgException(Exception):
    """Base class for all StGit exceptions."""


class StackException(StgException):
    """Exception raised by L{stack} objects."""
