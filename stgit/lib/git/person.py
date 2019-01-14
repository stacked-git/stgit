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

from .base import Immutable, NoValue, make_defaults
from .date import Date


class Person(Immutable):
    """Represents an author or committer in a git commit object. Contains
    name, email and timestamp."""

    def __init__(
        self, name=NoValue, email=NoValue, date=NoValue, defaults=NoValue
    ):
        d = make_defaults(defaults)
        self.__name = d(name, 'name')
        self.__email = d(email, 'email')
        self.__date = d(date, 'date')
        assert isinstance(self.__date, Date) or self.__date in [None, NoValue]

    @property
    def name(self):
        return self.__name

    @property
    def email(self):
        return self.__email

    @property
    def name_email(self):
        return '%s <%s>' % (self.name, self.email)

    @property
    def date(self):
        return self.__date

    def set_name(self, name):
        return type(self)(name=name, defaults=self)

    def set_email(self, email):
        return type(self)(email=email, defaults=self)

    def set_date(self, date):
        return type(self)(date=date, defaults=self)

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
        if not hasattr(cls, '__user'):
            cls.__user = cls(
                name=config.get('user.name'), email=config.get('user.email')
            )
        return cls.__user

    @classmethod
    def author(cls):
        if not hasattr(cls, '__author'):
            cls.__author = cls(
                name=environ_get('GIT_AUTHOR_NAME', NoValue),
                email=environ_get('GIT_AUTHOR_EMAIL', NoValue),
                date=Date.maybe(environ_get('GIT_AUTHOR_DATE', NoValue)),
                defaults=cls.user(),
            )
        return cls.__author

    @classmethod
    def committer(cls):
        if not hasattr(cls, '__committer'):
            cls.__committer = cls(
                name=environ_get('GIT_COMMITTER_NAME', NoValue),
                email=environ_get('GIT_COMMITTER_EMAIL', NoValue),
                date=Date.maybe(environ_get('GIT_COMMITTER_DATE', NoValue)),
                defaults=cls.user(),
            )
        return cls.__committer
