# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import re

from stgit.compat import environ_get
from stgit.config import config

from .base import Immutable
from .date import Date


class Person(Immutable):
    """Represents an author or committer in a git commit object. Contains
    name, email and timestamp."""

    def __init__(self, name, email, date):
        self.name = name
        self.email = email
        self.date = date

    @property
    def name_email(self):
        return '%s <%s>' % (self.name, self.email)

    def set_name(self, name):
        return self._replace(name=name)

    def set_email(self, email):
        return self._replace(email=email)

    def set_date(self, date):
        return self._replace(date=date)

    def _replace(self, **kws):
        return type(self)(
            kws.get('name', self.name),
            kws.get('email', self.email),
            kws.get('date', self.date),
        )

    def __repr__(self):
        return '%s %s' % (self.name_email, self.date)

    @classmethod
    def parse(cls, s):
        m = re.match(r'^([^<]*)<([^>]*)>\s+(\d+\s+[+-]\d{4})$', s)
        name = m.group(1).strip()
        email = m.group(2)
        date = Date(m.group(3))
        return cls(name, email, date)

    @classmethod
    def user(cls):
        if not hasattr(cls, '_user'):
            cls._user = cls(
                config.get('user.name'), config.get('user.email'), date=None
            )
        return cls._user

    @classmethod
    def author(cls):
        if not hasattr(cls, '_author'):
            user = cls.user()
            cls._author = cls(
                environ_get('GIT_AUTHOR_NAME', user.name),
                environ_get('GIT_AUTHOR_EMAIL', user.email),
                Date.maybe(environ_get('GIT_AUTHOR_DATE')),
            )
        return cls._author

    @classmethod
    def committer(cls):
        if not hasattr(cls, '_committer'):
            user = cls.user()
            cls._committer = cls(
                environ_get('GIT_COMMITTER_NAME', user.name),
                environ_get('GIT_COMMITTER_EMAIL', user.email),
                Date.maybe(environ_get('GIT_COMMITTER_DATE')),
            )
        return cls._committer
