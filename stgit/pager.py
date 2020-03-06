import os
import shutil
import subprocess
import sys

from stgit.out import out


def pager(msg):
    pager = _choose_pager()
    if not sys.stdin.isatty() or not sys.stdout.isatty() or not pager:
        return out.stdout_bytes(msg)
    else:
        return _run_pager(pager, msg)


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
    for pager in ['less', 'more']:
        if shutil.which(pager):
            return pager
    return None
