# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import datetime
import io
import subprocess

from stgit.compat import environ_copy, environ_get, file_wrapper, fsencode_utf8, text
from stgit.exception import StgException
from stgit.out import MessagePrinter, out

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


class RunException(StgException):
    """Thrown when something bad happened when we tried to run the
    subprocess."""

    pass


def get_log_mode(spec):
    if ':' not in spec:
        spec += ':'
    (log_mode, outfile) = spec.split(':', 1)
    all_log_modes = ['debug', 'profile']
    if log_mode and log_mode not in all_log_modes:
        out.warn(
            ('Unknown log mode "%s" specified in $STGIT_SUBPROCESS_LOG.' % log_mode),
            'Valid values are: %s' % ', '.join(all_log_modes),
        )
    if outfile:
        f = MessagePrinter(io.open(outfile, 'a', encoding='utf-8'))
    else:
        f = out
    return (log_mode, f)


_log_mode, _logfile = get_log_mode(environ_get('STGIT_SUBPROCESS_LOG', ''))
if _log_mode == 'profile':
    _log_starttime = datetime.datetime.now()
    _log_subproctime = 0.0


def duration(t1, t2):
    d = t2 - t1
    return 86400 * d.days + d.seconds + 1e-6 * d.microseconds


def finish_logging():
    if _log_mode != 'profile':
        return
    ttime = duration(_log_starttime, datetime.datetime.now())
    rtime = ttime - _log_subproctime
    _logfile.info(
        'Total time: %1.3f s' % ttime,
        'Time spent in subprocess calls: %1.3f s (%1.1f%%)'
        % (_log_subproctime, 100 * _log_subproctime / ttime),
        'Remaining time: %1.3f s (%1.1f%%)' % (rtime, 100 * rtime / ttime),
    )


class Run(object):
    exc = RunException

    def __init__(self, *cmd):
        self._cmd = list(cmd)
        for c in cmd:
            if not isinstance(c, text):
                raise Exception('Bad command: %r' % (cmd,))
        self._good_retvals = [0]
        self._env = self._cwd = None
        self._indata = None
        self._in_encoding = 'utf-8'
        self._out_encoding = 'utf-8'
        self._discard_stderr = False

    def _prep_cmd(self):
        return [fsencode_utf8(c) for c in self._cmd]

    def _prep_env(self):
        if self._env:
            return dict(
                (fsencode_utf8(k), fsencode_utf8(v)) for k, v in self._env.items()
            )
        else:
            return self._env

    def _log_start(self):
        if _log_mode == 'debug':
            _logfile.start('Running subprocess %s' % self._cmd)
            if self._cwd is not None:
                _logfile.info('cwd: %s' % self._cwd)
            if self._env is not None:
                for k in sorted(self._env):
                    v = environ_get(k)
                    if v is None or v != self._env[k]:
                        _logfile.info('%s: %s' % (k, self._env[k]))
        elif _log_mode == 'profile':
            _logfile.start('Running subprocess %s' % self._cmd)
            self._starttime = datetime.datetime.now()

    def _log_end(self, retcode):
        global _log_subproctime, _log_starttime
        if _log_mode == 'debug':
            _logfile.done('return code: %d' % retcode)
        elif _log_mode == 'profile':
            n = datetime.datetime.now()
            d = duration(self._starttime, n)
            _logfile.done('%1.3f s' % d)
            _log_subproctime += d
            _logfile.info(
                'Time since program start: %1.3f s' % duration(_log_starttime, n)
            )

    def _check_exitcode(self):
        if self._good_retvals is None:
            return
        if self.exitcode not in self._good_retvals:
            raise self.exc('%s failed with code %d' % (self._cmd[0], self.exitcode))

    def _run_io(self):
        """Run with captured IO."""
        self._log_start()
        try:
            p = subprocess.Popen(
                self._prep_cmd(),
                env=self._prep_env(),
                cwd=self._cwd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            outdata, errdata = p.communicate(self._indata)
            self.exitcode = p.returncode
        except OSError as e:
            raise self.exc('%s failed: %s' % (self._cmd[0], e))
        if errdata and not self._discard_stderr:
            out.err_bytes(errdata)
        self._log_end(self.exitcode)
        self._check_exitcode()
        if self._out_encoding:
            return outdata.decode(self._out_encoding)
        else:
            return outdata

    def _run_noio(self):
        """Run without captured IO."""
        assert self._indata is None
        self._log_start()
        try:
            p = subprocess.Popen(self._prep_cmd(), env=self._prep_env(), cwd=self._cwd,)
            self.exitcode = p.wait()
        except OSError as e:
            raise self.exc('%s failed: %s' % (self._cmd[0], e))
        self._log_end(self.exitcode)
        self._check_exitcode()

    def run_background(self):
        """Run as a background process."""
        assert self._indata is None
        try:
            p = subprocess.Popen(
                self._prep_cmd(),
                env=self._prep_env(),
                cwd=self._cwd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
        except OSError as e:
            raise self.exc('%s failed: %s' % (self._cmd[0], e))
        if self._in_encoding:
            if hasattr(p.stdin, 'readable'):
                self.stdin = io.TextIOWrapper(p.stdin, encoding=self._in_encoding)
            else:
                self.stdin = io.TextIOWrapper(
                    file_wrapper(p.stdin, writable=True), encoding=self._in_encoding
                )
        else:
            self.stdin = p.stdin
        self.stdout = p.stdout
        self.stderr = p.stderr
        self.wait = p.wait
        self.pid = lambda: p.pid
        return self

    def returns(self, retvals):
        self._good_retvals = retvals
        return self

    def discard_exitcode(self):
        self._good_retvals = None
        return self

    def discard_stderr(self, discard=True):
        self._discard_stderr = discard
        return self

    def env(self, env):
        self._env = environ_copy()
        self._env.update(env)
        return self

    def cwd(self, cwd):
        self._cwd = cwd
        return self

    def encoding(self, encoding):
        self._in_encoding = encoding
        return self

    def decoding(self, encoding):
        self._out_encoding = encoding
        return self

    def raw_input(self, indata):
        if self._in_encoding:
            self._indata = indata.encode(self._in_encoding)
        else:
            self._indata = indata
        return self

    def input_nulterm(self, lines):
        return self.raw_input('\0'.join(lines))

    def no_output(self):
        outdata = self._run_io()
        if outdata:
            raise self.exc('%s produced output' % self._cmd[0])

    def discard_output(self):
        self._run_io()

    def raw_output(self):
        return self._run_io()

    def output_lines(self, sep='\n'):
        outdata = self._run_io()
        if outdata.endswith(sep):
            outdata = outdata[:-1]
        if outdata:
            return outdata.split(sep)
        else:
            return []

    def output_one_line(self, sep='\n'):
        outlines = self.output_lines(sep)
        if len(outlines) == 1:
            return outlines[0]
        else:
            raise self.exc(
                '%s produced %d lines, expected 1' % (self._cmd[0], len(outlines))
            )

    def run(self):
        """Just run, with no IO redirection."""
        self._run_noio()
