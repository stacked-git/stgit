# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import email.utils
import os
import sys
import time

# With unicode_literals enabled, the type of a string literal will be `unicode`
# for Python 2 and `str` for Python 3.
text = type('')

# PEP-540 (Add a new UTF-8 mode) makes a compelling argument for Python
# programs making special effort to work around misconfigured locale
# settings. This largely boils down to treating incoming byte sequences,
# i.e. command line arguments and environment variables, as UTF-8.
#
# This is specifically relevant when the POSIX (aka C) locale is in effect.
#
# https://www.python.org/dev/peps/pep-0540/
#
# The following functions help achieve this goal by using UTF-8 as a fallback
# encoding when the nominal encoding (sys.getfilesystemencoding()) fails.
if sys.version_info[0] <= 2:
    _fs_enc = sys.getfilesystemencoding()

    def fsdecode_utf8(b):
        """Decode to filesystem encoding, with UTF-8 fallback."""
        if isinstance(b, bytes):
            try:
                return b.decode(_fs_enc)
            except UnicodeDecodeError:
                return b.decode('utf-8')
        else:
            return fsdecode_utf8(fsencode_utf8(b))

    def fsencode_utf8(s):
        """Encode to filesystem encoding, with UTF-8 fallback."""
        try:
            return s.encode(_fs_enc)
        except UnicodeEncodeError:
            return s.encode('utf-8')

    def environ_get(key, default=None):
        b = os.environ.get(key, default)
        if b is default:
            return default
        else:
            return fsdecode_utf8(b)

    def environ_copy():
        return dict((fsdecode_utf8(k), fsdecode_utf8(v))
                    for k, v in os.environ.iteritems())

    def rfc2822_format(dt):
        return text(email.utils.formatdate(time.mktime(dt.timetuple())))

else:  # Python 3
    def fsdecode_utf8(b):
        if isinstance(b, bytes):
            try:
                return os.fsdecode(b)
            except UnicodeDecodeError:
                return b.decode('utf-8')
        else:
            return os.fsencode(b).decode('utf-8')

    def fsencode_utf8(s):
        try:
            return os.fsencode(s)
        except UnicodeEncodeError:
            return s.encode('utf-8')

    def environ_get(key, default=None):
        s = os.environ.get(key, default)
        if s is default:
            return default
        else:
            return s.encode('utf-8', 'surrogateescape').decode('utf-8')

    def environ_copy():
        return os.environ.copy()

    rfc2822_format = email.utils.format_datetime


class file_wrapper(object):
    """Wrap file object with missing methods needed by TextIOWrapper."""

    def __init__(self, f, readable=False, writable=False, seekable=False):
        self.__f = f
        self.__readable = readable
        self.__writable = writable
        self.__seekable = seekable

    def __getattr__(self, name):
        return getattr(self.__f, name)

    def readable(self):
        return self.__readable

    def writable(self):
        return self.__writable

    def seekable(self):
        return self.__seekable


# Python 2 only has email.message_from_file(), but it behaves like Python 3's
# email.message_from_binary_file().
message_from_binary_file = getattr(email, 'message_from_binary_file',
                                   email.message_from_file)
message_from_bytes = getattr(email, 'message_from_bytes',
                             email.message_from_string)


def decode_utf8_with_latin1(input, errors='strict'):
    """Decode utf-8 bytes with possible latin-1 encoded bytes.

    There are cases where encoded byte streams may nominally be utf-8 encoded,
    but contain stray latin-1 (iso8859-1) characters. The input bytes are
    decoded as utf-8, but with any non-utf-8 byte sequences decoded as latin-1.

    This is the decode strategy employed by git when decoding utf-8 email
    bodies.

    """
    s = ''
    while True:
        try:
            s += input.decode('utf-8', 'strict')
        except UnicodeDecodeError as e:
            _, _, start, end, _ = e.args
            s += input[:start].decode('utf-8')
            s += input[start:end].decode('latin1')
            input = input[end:]
        else:
            break
    return s
