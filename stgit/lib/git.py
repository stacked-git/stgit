import atexit, os, os.path, re, signal
class CatFileProcess(object):
    def __init__(self, repo):
        self.__repo = repo
        self.__proc = None
        atexit.register(self.__shutdown)
    def __get_process(self):
        if not self.__proc:
            self.__proc = self.__repo.run(['git', 'cat-file', '--batch']
                                          ).run_background()
        return self.__proc
    def __shutdown(self):
        p = self.__proc
        if p:
            os.kill(p.pid(), signal.SIGTERM)
            p.wait()
    def cat_file(self, sha1):
        p = self.__get_process()
        p.stdin.write('%s\n' % sha1)
        p.stdin.flush()

        # Read until we have the entire status line.
        s = ''
        while not '\n' in s:
            s += os.read(p.stdout.fileno(), 4096)
        h, b = s.split('\n', 1)
        if h == '%s missing' % sha1:
            raise SomeException()
        hash, type, length = h.split()
        assert hash == sha1
        length = int(length)

        # Read until we have the entire object plus the trailing
        # newline.
        while len(b) < length + 1:
            b += os.read(p.stdout.fileno(), 4096)
        return type, b[:-1]

class DiffTreeProcesses(object):
    def __init__(self, repo):
        self.__repo = repo
        self.__procs = {}
        atexit.register(self.__shutdown)
    def __get_process(self, args):
        args = tuple(args)
        if not args in self.__procs:
            self.__procs[args] = self.__repo.run(
                ['git', 'diff-tree', '--stdin'] + list(args)).run_background()
        return self.__procs[args]
    def __shutdown(self):
        for p in self.__procs.values():
            os.kill(p.pid(), signal.SIGTERM)
            p.wait()
    def diff_trees(self, args, sha1a, sha1b):
        p = self.__get_process(args)
        query = '%s %s\n' % (sha1a, sha1b)
        end = 'EOF\n' # arbitrary string that's not a 40-digit hex number
        p.stdin.write(query + end)
        p.stdin.flush()
        s = ''
        while not (s.endswith('\n' + end) or s.endswith('\0' + end)):
            s += os.read(p.stdout.fileno(), 4096)
        assert s.startswith(query)
        assert s.endswith(end)
        return s[len(query):-len(end)]

        self.__catfile = CatFileProcess(self)
        self.__difftree = DiffTreeProcesses(self)
        return self.__catfile.cat_file(sha1)[1]
        return [self.get_commit(sha1) for sha1 in sha1_list]
        return self.__difftree.diff_trees(['-p'] + diff_opts,
                                          t1.sha1, t2.sha1)
        i = iter(self.__difftree.diff_trees(
                ['-r', '-z'], t1.sha1, t2.sha1).split('\0'))