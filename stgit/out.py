# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)
import io
import sys
import textwrap

__copyright__ = """
Copyright (C) 2007, Karl Hasselström <kha@treskal.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see http://www.gnu.org/licenses/.
"""


class Output(object):
    def __init__(self, file):
        if file is not None:
            self.write = file.write
            self.write_bytes = file.buffer.write
            self.flush = file.flush
        else:
            self.write = self.write_bytes = lambda s: None
            self.flush = lambda: None
        self.at_start_of_line = True
        self.level = 0

    def new_line(self):
        """Ensure that we're at the beginning of a line."""
        if not self.at_start_of_line:
            self.write('\n')
            self.at_start_of_line = True

    def single_line(self, msg, print_newline=True, need_newline=True):
        """Write a single line. Newline before and after are
        separately configurable."""
        if need_newline:
            self.new_line()
        if self.at_start_of_line:
            self.write('  '*self.level)
        self.write(msg)
        if print_newline:
            self.write('\n')
            self.at_start_of_line = True
        else:
            self.flush()
            self.at_start_of_line = False

    def tagged_lines(self, tag, lines):
        tag += ': '
        width = 79 - 2*self.level - len(tag)
        lines = [wl for line in lines
                 for wl in textwrap.wrap(line, width, break_long_words=False)]
        for line in lines:
            self.single_line(tag + line)
            tag = ' '*len(tag)

    def write_line(self, line):
        """Write one line of text on a lines of its own, not
        indented."""
        self.new_line()
        self.write('%s\n' % line)
        self.at_start_of_line = True

    def write_raw(self, string):
        """Write an arbitrary string, possibly containing
        newlines."""
        self.new_line()
        self.write(string)
        self.at_start_of_line = string.endswith('\n')

    def write_bytes(self, byte_data):
        self.new_line()
        self.write_bytes(byte_data)
        self.at_start_of_line = byte_data.endswith(b'\n')


class MessagePrinter(object):
    def __init__(self, file=None):
        if file:
            self.__stdout = self.__stderr = Output(file)
        else:
            self.__stdout = Output(io.open(sys.stdout.fileno(), 'w',
                                           buffering=1, encoding='utf-8'))
            self.__stderr = Output(io.open(sys.stderr.fileno(), 'w',
                                           buffering=1, encoding='utf-8'))
        if file or sys.stdout.isatty():
            self.__out = self.__stdout
        else:
            self.__out = Output(None)

    def stdout(self, line):
        """Write a line to stdout."""
        self.__stdout.write_line(line)

    def stdout_raw(self, string):
        """Write a string possibly containing newlines to stdout."""
        self.__stdout.write_raw(string)

    def stdout_bytes(self, byte_data):
        self.__stdout.write_bytes(byte_data)

    def err_raw(self, string):
        """Write a string possibly containing newlines to the error
        output."""
        self.__stderr.write_raw(string)

    def info(self, *msgs):
        for msg in msgs:
            self.__out.single_line(msg)

    def note(self, *msgs, **kw):
        self.__out.tagged_lines(kw.get('title', 'Notice'), msgs)

    def warn(self, *msgs, **kw):
        self.__stderr.tagged_lines(kw.get('title', 'Warning'), msgs)

    def error(self, *msgs, **kw):
        self.__stderr.tagged_lines(kw.get('title', 'Error'), msgs)

    def start(self, msg):
        """Start a long-running operation."""
        self.__out.single_line('%s ... ' % msg, print_newline=False)
        self.__out.level += 1

    def done(self, extramsg = None):
        """Finish long-running operation."""
        self.__out.level -= 1
        if extramsg:
            msg = 'done (%s)' % extramsg
        else:
            msg = 'done'
        self.__out.single_line(msg, need_newline=False)


out = MessagePrinter()
