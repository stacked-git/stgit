# -*- coding: utf-8 -*-
from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

import os
import subprocess
import sys

from stgit.out import out


def pager(msg):
    if any([not hasattr(sys.stdin, 'isatty'),
            not hasattr(sys.stdout, 'isatty'),
            not sys.stdin.isatty(),
            not sys.stdout.isatty()]):
        return out.stdout_bytes(msg)
    pager = _choose_pager()
    if pager:
        return _run_pager(pager, msg)
    else:
        return out.stdout_bytes(msg)


def _run_pager(pager, msg):
    proc = subprocess.Popen(pager, shell=True, stdin=subprocess.PIPE)
    try:
        proc.stdin.write(msg)
    except (KeyboardInterrupt, OSError):
        pass
    finally:
        proc.stdin.close()
    while True:
        try:
            proc.wait()
            break
        except KeyboardInterrupt:
            pass


def _choose_pager():
    for k in ['GIT_PAGER', 'PAGER', 'MANPAGER']:
        pager = os.environ.get(k)
        if pager:
            return pager
    if hasattr(os, 'system'):
        for pager in ['less', 'more']:
            if os.system('{pager} -V >{null} 2>{null}'.format(
                    pager=pager, null=os.devnull)) == 0:
                return pager
    return None
