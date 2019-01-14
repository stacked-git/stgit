# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from datetime import datetime, timedelta, tzinfo
import re

from stgit.exception import StgException
from stgit.lib.git.base import Immutable, NoValue
from stgit.run import Run, RunException


class DateException(StgException):
    """Exception raised when a date+time string could not be parsed."""

    def __init__(self, string, type):
        StgException.__init__(self, '"%s" is not a valid %s' % (string, type))


class TimeZone(tzinfo):
    """A simple time zone class for static offsets from UTC. (We have to
    define our own since Python's standard library doesn't define any
    time zone classes.)"""

    def __init__(self, tzstring):
        m = re.match(r'^([+-])(\d{2}):?(\d{2})$', tzstring)
        if not m:
            raise DateException(tzstring, 'time zone')
        sign = int(m.group(1) + '1')
        try:
            self.__offset = timedelta(
                hours=sign * int(m.group(2)), minutes=sign * int(m.group(3))
            )
        except OverflowError:
            raise DateException(tzstring, 'time zone')
        self.__name = tzstring

    def utcoffset(self, dt):
        return self.__offset

    def tzname(self, dt):
        return self.__name

    def dst(self, dt):
        return timedelta(0)

    def __repr__(self):
        return self.__name


def system_date(datestring):
    m = re.match(r"^(.+)([+-]\d\d:?\d\d)$", datestring)
    if m:
        # Time zone included; we parse it ourselves, since "date"
        # would convert it to the local time zone.
        (ds, z) = m.groups()
        try:
            t = Run("date", "+%Y-%m-%d-%H-%M-%S", "-d", ds).output_one_line()
        except RunException:
            return None
    else:
        # Time zone not included; we ask "date" to provide it for us.
        try:
            d = Run(
                "date", "+%Y-%m-%d-%H-%M-%S_%z", "-d", datestring
            ).output_one_line()
        except RunException:
            return None
        (t, z) = d.split("_")
    year, month, day, hour, minute, second = [int(x) for x in t.split("-")]
    try:
        return datetime(
            year, month, day, hour, minute, second, tzinfo=TimeZone(z)
        )
    except ValueError:
        raise DateException(datestring, "date")


def git_date(datestring=''):
    try:
        ident = (
            Run('git', 'var', 'GIT_AUTHOR_IDENT')
            .env(
                {
                    'GIT_AUTHOR_NAME': 'XXX',
                    'GIT_AUTHOR_EMAIL': 'XXX',
                    'GIT_AUTHOR_DATE': datestring,
                }
            )
            .output_one_line()
        )
    except RunException:
        return None
    _, _, timestamp, offset = ident.split()
    return datetime.fromtimestamp(int(timestamp), TimeZone(offset))


class Date(Immutable):
    """Represents a timestamp used in git commits."""

    def __init__(self, datestring):
        # Try git-formatted date.
        m = re.match(r'^(\d+)\s+([+-]\d\d:?\d\d)$', datestring)
        if m:
            try:
                self.__time = datetime.fromtimestamp(
                    int(m.group(1)), TimeZone(m.group(2))
                )
            except ValueError:
                raise DateException(datestring, 'date')
            return

        # Try iso-formatted date.
        m = re.match(
            r'^(\d{4})-(\d{2})-(\d{2})\s+(\d{2}):(\d{2}):(\d{2})\s+'
            r'([+-]\d\d:?\d\d)$',
            datestring,
        )
        if m:
            try:
                self.__time = datetime(
                    *[int(m.group(i + 1)) for i in range(6)],
                    **{'tzinfo': TimeZone(m.group(7))}
                )
            except ValueError:
                raise DateException(datestring, 'date')
            return

        if datestring == 'now':
            self.__time = git_date()
            assert self.__time
            return

        # Try parsing with `git var`.
        gd = git_date(datestring)
        if gd:
            self.__time = gd
            return

        # Try parsing with the system's "date" command.
        sd = system_date(datestring)
        if sd:
            self.__time = sd
            return

        raise DateException(datestring, 'date')

    def __repr__(self):
        return self.isoformat()

    def isoformat(self):
        """Human-friendly ISO 8601 format."""
        return '%s %s' % (
            self.__time.replace(tzinfo=None).isoformat(str(' ')),
            self.__time.tzinfo,
        )

    @classmethod
    def maybe(cls, datestring):
        """Return a new object initialized with the argument if it contains a
        value (otherwise, just return the argument)."""
        if datestring in [None, NoValue]:
            return datestring
        return cls(datestring)
