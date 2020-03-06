next_mark = 1


def get_mark():
    global next_mark
    next_mark += 1
    return next_mark - 1


def write_data(s):
    print('data %d' % len(s))
    print(s)


def write_blob(s):
    print('blob')
    m = get_mark()
    print('mark :%d' % m)
    write_data(s)
    return m


def write_commit(branch, files, msg, parent=None):
    print('commit %s' % branch)
    m = get_mark()
    print('mark :%d' % m)
    auth = 'X Ample <xa@example.com> %d +0000' % (1000000000 + m)
    print('author %s' % auth)
    print('committer %s' % auth)
    write_data(msg)
    if parent is not None:
        print('from :%d' % parent)
    for fn, fm in sorted(files.items()):
        print('M 100644 :%d %s' % (fm, fn))
    return m


def set_ref(ref, mark):
    print('reset %s' % ref)
    print('from :%d' % mark)


def stdblob(fn):
    return ''.join('%d %s\n' % (x, fn) for x in range(10))


def iter_paths():
    for i in range(32):
        for j in range(32):
            for k in range(32):
                yield '%02d/%02d/%02d' % (i, j, k)


def setup():
    def t(name):
        return 'refs/tags/%s' % name

    files = {fn: write_blob(stdblob(fn)) for fn in iter_paths()}
    initial = write_commit(t('bomb-base'), files, 'Initial commit')
    set_ref(t('bomb-top'), initial)
    for fn in iter_paths():
        write_commit(
            t('bomb-top'),
            {fn: write_blob(stdblob(fn) + 'Last line\n')},
            'Add last line to %s' % fn,
        )
    write_commit(
        t('add-file'),
        {'woo-hoo.txt': write_blob('woo-hoo\n')},
        'Add a new file',
        parent=initial,
    )
    files = dict((fn, write_blob('First line\n' + stdblob(fn))) for fn in iter_paths())
    write_commit(
        t('modify-all'), files, 'Add first line to all files', parent=initial,
    )


setup()
