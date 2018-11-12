# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)
import datetime
import os
import subprocess
import sys


def duration(t1, t2):
    d = t2 - t1
    return 86400 * d.days + d.seconds + 1e-6 * d.microseconds


class Run(object):
    def __init__(self):
        self.__cwd = None
        self.__log = []

    def __logfile(self, cmd):
        fn = os.path.join(os.getcwd(), '%04d.log' % len(self.__log))
        f = open(fn, 'w')
        f.write(' '.join(cmd) + '\n' + '-' * 70 + '\n\n')
        f.close()
        return fn

    def __call__(self, *cmd, **args):
        env = dict(os.environ)
        env['STGIT_SUBPROCESS_LOG'] = 'profile:' + self.__logfile(cmd)
        kwargs = {'cwd': self.__cwd, 'env': env}
        if args.get('capture_stdout', False):
            kwargs['stdout'] = subprocess.PIPE
        start = datetime.datetime.now()
        p = subprocess.Popen(cmd, **kwargs)
        (out, err) = p.communicate()
        stop = datetime.datetime.now()
        self.__log.append((cmd, duration(start, stop)))
        return out

    def cd(self, dir):
        self.__cwd = dir

    def summary(self):
        def pcmd(c):
            return ' '.join(c)

        def ptime(t):
            return '%.3f' % t

        (cs, times) = zip(*self.__log)
        ttime = sum(times)
        cl = max(len(pcmd(c)) for c in cs)
        tl = max(len(ptime(t)) for t in list(times) + [ttime])
        for (c, t) in self.__log:
            print('%*s  %*s' % (tl, ptime(t), -cl, pcmd(c)))
        print('%*s' % (tl, ptime(ttime)))


perftests = {}
perftestdesc = {}


def perftest(desc, name=None):
    def decorator(f):
        def g():
            r = Run()
            f(r)
            r.summary()
        perftests[name or f.__name__] = g
        perftestdesc[name or f.__name__] = desc
        return g
    return decorator


def copy_testdir(dir):
    tmp = dir + '.trash'
    r = Run()
    r('rsync', '-a', '--delete', dir + '.orig/', tmp)
    return tmp


def new_rebase(r, ref):
    top = r('stg', 'top', capture_stdout=True)
    r('stg', 'pop', '-a')
    r('git', 'reset', '--hard', ref)
    r('stg', 'goto', top.strip())


def old_rebase(r, ref):
    r('stg', 'rebase', ref)


def def_rebasetest(rebase, dir, tag):
    @perftest('%s rebase onto %s in %s' % (rebase, tag, dir),
              'rebase-%srebase-%s-%s' % (rebase, tag, dir))
    def rebasetest(r):
        r.cd(copy_testdir(dir))
        r('stg', 'init')
        if dir == 'synt':
            r('stg', 'uncommit', '-n', '500')
        else:
            r('stg', 'uncommit', '-x', '-t', 'bomb-base')
        if rebase == 'new':
            new_rebase(r, tag)
        else:
            old_rebase(r, tag)


for rebase in ['old', 'new']:
    for (dir, tag) in [('synt', 'add-file'),
                       ('synt', 'modify-all'),
                       ('linux', 'add-file')]:
        def_rebasetest(rebase, dir, tag)

args = sys.argv[1:]
if len(args) == 0:
    for (fun, desc) in sorted(perftestdesc.items()):
        print('%s: %s' % (fun, desc))
else:
    for test in args:
        perftests[test]()
