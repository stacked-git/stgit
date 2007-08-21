# -*- coding: utf-8 -*-

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"""

# popen2 and os.spawn* suck. We should really use subprocess instead,
# but that's only available in Python 2.4 and up, and we try our best
# to stay Python 2.3 compatible.
import popen2, os

class RunException(Exception):
    """Thrown when something bad happened when we tried to run the
    subprocess."""
    pass

class Run:
    exc = RunException
    def __init__(self, *cmd):
        self.__cmd = list(cmd)
        self.__good_retvals = [0]
        self.__env = None
        self.__indata = None
    def __run_io(self, cmd):
        """Run with captured IO. Note: arguments are parsed by the
        shell. We single-quote them, so don't use anything with single
        quotes in it."""
        if self.__env == None:
            ecmd = cmd
        else:
            ecmd = (['env'] + ['%s=%s' % (key, val)
                               for key, val in self.__env.iteritems()]
                    + cmd)
        p = popen2.Popen3(' '.join(["'%s'" % c for c in ecmd]), True)
        if self.__indata != None:
            p.tochild.write(self.__indata)
        p.tochild.close()
        outdata = p.fromchild.read()
        errdata = p.childerr.read()
        self.exitcode = p.wait() >> 8
        if errdata or self.exitcode not in self.__good_retvals:
            raise self.exc('%s failed with code %d:\n%s'
                           % (cmd[0], self.exitcode, errdata))
        return outdata
    def __run_noshell(self, cmd):
        """Run without captured IO. Note: arguments are not parsed by
        the shell."""
        assert self.__env == None
        assert self.__indata == None
        self.exitcode = os.spawnvp(os.P_WAIT, cmd[0], cmd)
        if not self.exitcode in self.__good_retvals:
            raise self.exc('%s failed with code %d'
                           % (cmd[0], self.exitcode))
    def returns(self, retvals):
        self.__good_retvals = retvals
        return self
    def env(self, env):
        self.__env = env
        return self
    def raw_input(self, indata):
        self.__indata = indata
        return self
    def input_lines(self, lines):
        self.__indata = ''.join(['%s\n' % line for line in lines])
        return self
    def no_output(self):
        outdata = self.__run_io(self.__cmd)
        if outdata:
            raise self.exc, '%s produced output' % self.__cmd[0]
    def discard_output(self):
        self.__run_io(self.__cmd)
    def raw_output(self):
        return self.__run_io(self.__cmd)
    def output_lines(self):
        outdata = self.__run_io(self.__cmd)
        if outdata.endswith('\n'):
            outdata = outdata[:-1]
        if outdata:
            return outdata.split('\n')
        else:
            return []
    def output_one_line(self):
        outlines = self.output_lines()
        if len(outlines) == 1:
            return outlines[0]
        else:
            raise self.exc('%s produced %d lines, expected 1'
                           % (self.__cmd[0], len(outlines)))
    def run(self):
        """Just run, with no IO redirection."""
        self.__run_noshell(self.__cmd)
    def xargs(self, xargs):
        """Just run, with no IO redirection. The extra arguments are
        appended to the command line a few at a time; the command is
        run as many times as needed to consume them all."""
        step = 100
        for i in xrange(0, len(xargs), step):
            self.__run_noshell(self.__cmd + xargs[i:i+step])
