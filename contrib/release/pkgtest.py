#!/usr/bin/env python3
"""Test various aspects of StGit packaging.

[x] Use XDG_RUNTIME_DIR
[ ] Use chroot
[x] Use clone of repo
[ ] Make annotated tags within test repo
[x] Different python versions
[ ] Different setuptools versions

Test source distribution creation:
[x] Ensure generated files are in sdist tarball
[x] Ensure that generated files are not in worktree
[x] Ensure dirty worktree is reflected in version

Test build:
[x] Ensure generated python files land in build/
[x] Ensure generated python files also land in worktree
[x] Ensure generated completions land in worktree

Test install from git worktree:
[x] Test develop mode
[x] Test installing to virtualenv
[ ] Test installing to system (i.e. w/chroot)

Test install from unpacked sdist tarball
[x] Test installing to virtualenv
[ ] Test installing to system (i.e. w/chroot)

[x] Test install from git URL

Test install sdist tarball with pip
[x] Ensure generated python is in right place
[x] Ensure generated completions in right place
[x] Ensure other files in right places

Test making sdist from sdist
[x] Make sdist from git worktree
[x] Unpack sdist
[x] Make sdist from unpacked sdist tree
[x] Ensure identical tarballs

Test wheels
[x] Test build with bdist_wheel
[x] Test install whl to virtualenv

Test git archives
[x] Test generate git archive
[x] Test make sdist from git archive
[ ] Test install from unpacked git archive

Test running test suite
[x] from sdist
[ ] from git archive
[ ] with installed stg

"""

import argparse
import os
import shutil
import subprocess
import sys
import tarfile
import zipfile
from pathlib import Path

PYTHON_VERSIONS = [
    'python3.9',
    'python3.8',
    'python3.7',
    'python3.6',
    'python3.5',
    'pypy3',
]

BUILD_PACKAGE_NAMES = ['pip', 'wheel', 'setuptools']


def log(*args):
    print(':::', *args)


def log_test(testname, *args):
    print('\n###', testname, *args)


def printable_cmd(cmd_list):
    runtime_dir = Path(os.environ['XDG_RUNTIME_DIR'])
    home = Path.home()
    printable_items = []
    for item in cmd_list:
        if isinstance(item, Path):
            if item.is_relative_to(runtime_dir):
                printable = f'{item.relative_to(runtime_dir)}'
            elif item.is_relative_to(home):
                printable = f'~/{item.relative_to(home)}'
            else:
                printable = f'{item}'
            printable_items.append(printable)
        else:
            printable_items.append(item)
    return ' '.join(printable_items)


def subprocess_cmd(cmd_list):
    norm_cmd = []
    for item in cmd_list:
        if isinstance(item, Path):
            norm_cmd.append(str(item))
        else:
            norm_cmd.append(item)
    return norm_cmd


def call(cmd, cwd, **kwargs):
    print('!!!', printable_cmd(cmd), f'(in {printable_cmd([cwd])})')
    stdout = kwargs.get('stdout')
    if stdout is None:
        proc = subprocess.run(
            subprocess_cmd(cmd),
            cwd=cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        for line in proc.stdout.decode().splitlines():
            print('...', line)
        return proc.returncode
    else:
        return subprocess.call(subprocess_cmd(cmd), cwd=cwd, **kwargs)


def check_call(cmd, cwd, **kwargs):
    print('!!!', printable_cmd(cmd), f'(in {printable_cmd([cwd])})')
    stdout = kwargs.get('stdout')
    if stdout is None:
        proc = subprocess.run(
            subprocess_cmd(cmd),
            cwd=cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        for line in proc.stdout.decode().splitlines():
            print('...', line)
        proc.check_returncode()
    else:
        subprocess.check_call(subprocess_cmd(cmd), cwd=cwd, **kwargs)


def check_output(cmd, cwd):
    print('!!!', printable_cmd(cmd), f'(in {printable_cmd([cwd])})')
    return subprocess.check_output(subprocess_cmd(cmd), cwd=cwd)


def main():
    parser = argparse.ArgumentParser()
    parser.set_defaults(pythons=[])
    for pyver in PYTHON_VERSIONS:
        parser.add_argument(
            f'--{pyver}', dest='pythons', action='append_const', const=pyver
        )
    parser.add_argument('--all-pythons', action='store_true')
    args = parser.parse_args()

    if args.all_pythons:
        args.pythons[:] = PYTHON_VERSIONS
    elif not args.pythons:
        args.pythons.append(PYTHON_VERSIONS[0])

    assert 'VIRTUAL_ENV' not in os.environ, 'Do not run this from a virtual env'

    system_stg = shutil.which('stg')
    log(f'{system_stg=}')
    if system_stg is None:
        system_stg_version = None
    else:
        system_stg_version = version_from_stg_version(
            check_output(['stg', '--version'], cwd=Path('/')).decode()
        )
    log(f'{system_stg_version=}')

    this_repo = Path(
        check_output(['git', 'rev-parse', '--show-toplevel'], cwd=Path())
        .rstrip()
        .decode()
    )
    log(f'{this_repo=}')
    assert this_repo.is_absolute()

    runtime_dir = Path(os.environ['XDG_RUNTIME_DIR'])
    log(f'{runtime_dir=}')
    test_root = runtime_dir / 'stgit-pkgtest'
    log(f'{test_root=}')
    assert test_root.is_absolute()
    if test_root.exists():
        shutil.rmtree(test_root)
    test_root.mkdir(exist_ok=False)

    cache_root = runtime_dir / 'stgit-pkgcache'
    should_create_cache = not cache_root.exists()
    log(f'{cache_root=} {should_create_cache=}')
    assert cache_root.is_absolute()

    if should_create_cache:
        create_cache(cache_root)

    for python in args.pythons:
        base = test_root / python
        repo = prepare_test_repo(base, this_repo)
        prepare_virtual_env(base, python, cache_root)
        test_dirty_version(base, repo)
        test_install_from_git_url(base, repo, cache_root)
        test_uninstall_from_venv(base)
        test_sdist_creation(base, repo)
        test_create_git_archive(base, repo)
        test_build(base, repo)
        test_bdist_wheel(base, repo)
        test_develop_mode(base, repo, cache_root)
        test_uninstall_from_venv(base)
        test_running_tests_from_sdist(base)
        test_install_from_unpacked_sdist(base, cache_root)
        test_uninstall_from_venv(base)
        test_install_sdist(base, cache_root)
        test_uninstall_from_venv(base)
        test_install_wheel(base)
        test_uninstall_from_venv(base)


def venv_py_exe(base):
    return base / 'venv' / 'bin' / 'python'


def venv_stg_exe(base):
    return base / 'venv' / 'bin' / 'stg'


def version_from_sdist(tgz_path):
    return tgz_path.name.split('stgit-', 1)[1].rsplit('.tar.gz', 1)[0]


def version_from_wheel(whl_path):
    return whl_path.name.split('stgit-', 1)[1].rsplit('-py3-none-any.whl')[0]


def version_from_stg_version(output):
    for line in output.splitlines():
        if line.lower().startswith('stacked git'):
            return line[12:]


def create_cache(cache_root):
    log_test('create_cache')
    cache_root.mkdir()
    check_call(
        [sys.executable, '-m', 'pip', 'download'] + BUILD_PACKAGE_NAMES,
        cwd=cache_root,
    )

    for package_name in BUILD_PACKAGE_NAMES:
        wheel_paths = list(cache_root.glob(f'{package_name}-*.whl'))
        assert len(wheel_paths) == 1, wheel_paths
        wheel_path = wheel_paths[0]
        wheel_link = cache_root / f'{package_name}.whl'
        wheel_link.symlink_to(wheel_path)


def prepare_test_repo(base, this_repo):
    log_test('prepare_test_repo')
    test_repo = base / 'stgit-git'
    check_call(
        ['git', 'clone', '--quiet', '--local', '--no-hardlinks', this_repo, test_repo],
        cwd=this_repo,
    )
    log(f'{test_repo=}')
    return test_repo


def prepare_virtual_env(base, python, cache_root):
    log_test('prepare_virtual_env')
    venv_path = base / 'venv'
    check_call([python, '-m', 'venv', venv_path], cwd=base)
    packages = [
        (cache_root / f'{package_name}.whl').resolve()
        for package_name in BUILD_PACKAGE_NAMES
    ]
    check_call(
        [venv_py_exe(base), '-m', 'pip', 'install', '--no-index'] + packages,
        cwd=cache_root,
    )
    check_call(
        [venv_py_exe(base), '-m', 'pip', 'list', '--no-index', '--format=freeze'],
        cwd=base,
    )
    log(f'{venv_path=}')
    return venv_path


def test_dirty_version(base, repo):
    log_test('test_dirty_version')

    # Modify a checked-in file to make worktree dirty
    dirty_file = repo / 'README.md'
    with open(dirty_file, 'a') as f:
        print("DIRTY STUFF", file=f)

    # Make sdist from dirty repo
    check_call(
        [venv_py_exe(base), 'setup.py', 'sdist'],
        cwd=repo,
        stdout=subprocess.DEVNULL,
    )
    sdist_paths = list((repo / 'dist').glob('stgit-*.tar.gz'))
    assert len(sdist_paths) == 1
    stgit_tgz = sdist_paths[0]
    log(f'{stgit_tgz=}')

    # Ensure sdist's version is marked 'dirty'
    version = version_from_sdist(stgit_tgz)
    log(f'{version=}')
    assert 'dirty' in version

    # Restore worktree to pristine state
    check_call(['git', 'checkout', '--', dirty_file], cwd=repo)
    check_call(['git', 'diff', '--quiet'], cwd=repo)
    check_call(['git', 'clean', '-qfx'], cwd=repo)


def test_install_from_git_url(base, repo, cache):
    log_test('test_install_from_git_url')
    check_call(
        [
            venv_py_exe(base),
            '-m',
            'pip',
            'install',
            # '--no-clean',
            '--no-index',
            '--find-links',
            cache,
            f'git+file://{str(repo)}',
        ],
        cwd=base,
    )
    version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    log(f'{version=}')

    # Ensure cmdlist.py was generated
    check_call([venv_py_exe(base), '-c', 'import stgit.commands.cmdlist'], cwd=base)


def test_sdist_creation(base, repo):
    log_test('test_sdist_creation')
    dist = base / 'dist'
    check_call(
        [venv_py_exe(base), 'setup.py', 'sdist', '--dist-dir', dist],
        cwd=repo,
        stdout=subprocess.DEVNULL,
    )
    sdist_paths = list(dist.glob('stgit-*.tar.gz'))
    assert len(sdist_paths) == 1
    stgit_tgz = sdist_paths[0]
    log(f'{stgit_tgz=}')
    prefix = Path(stgit_tgz.with_suffix('').with_suffix('').name)
    version = version_from_sdist(stgit_tgz)
    log(f'{version=}')

    # Ensure generated files are in sdist tarball
    with tarfile.open(stgit_tgz) as tf:
        tf.getmember(str(prefix / 'completion' / 'stg.fish'))
        tf.getmember(str(prefix / 'completion' / 'stgit.bash'))
        tf.getmember(str(prefix / 'stgit' / 'commands' / 'cmdlist.py'))
        _version_tar_info = tf.getmember(str(prefix / 'stgit' / '_version.py'))
        assert 'def _get_version()' not in _version_tar_info.tobuf().decode()

    # Ensure that no checked-in files in the working tree were altered
    check_call(['git', 'diff', '--quiet'], cwd=repo)

    # Extract existing sdist from base/dist
    orig_sdist = base / 'orig-sdist'
    orig_sdist.mkdir()
    work_sdist = base / 'work-sdist'
    work_sdist.mkdir()
    with tarfile.open(stgit_tgz) as tf:
        tf.extractall(orig_sdist)
        tf.extractall(work_sdist)

    # Create an sdist from an sdist
    dist2 = base / 'dist2'
    check_call(
        [venv_py_exe(base), 'setup.py', 'sdist', '--dist-dir', dist2],
        cwd=work_sdist / prefix,
        stdout=subprocess.DEVNULL,
    )
    for pycache in work_sdist.rglob('**/__pycache__'):
        shutil.rmtree(pycache)

    # Ensure derivative sdist is same as original sdist
    rc = call(['diff', '-qr', orig_sdist, work_sdist], cwd=base)
    if rc != 0:
        call(['diff', '-uNr', orig_sdist, work_sdist], cwd=base)
        raise Exception('Recursive sdist is different')


def test_create_git_archive(base, repo):
    log_test('test_create_git_archive')
    archive_tgz = base / 'stgit-archive.tar.gz'
    check_call(
        ['git', 'archive', '--prefix', 'stgit-archive/', '-o', archive_tgz, 'HEAD'],
        cwd=repo,
    )

    with tarfile.open(archive_tgz) as tf:
        tf.extractall(base)

    archive_dir = base / 'stgit-archive'

    assert archive_dir.is_dir()

    dist = base / 'dist-archive'

    check_call(
        [venv_py_exe(base), 'setup.py', 'sdist', '--dist-dir', dist],
        cwd=archive_dir,
        stdout=subprocess.DEVNULL,
    )
    sdist_paths = list(dist.glob('stgit-*.tar.gz'))
    assert len(sdist_paths) == 1
    stgit_tgz = sdist_paths[0]
    log(f'{stgit_tgz=}')
    prefix = Path(stgit_tgz.with_suffix('').with_suffix('').name)
    version = version_from_sdist(stgit_tgz)
    log(f'{version=}')
    assert 'unknown' not in version

    # Ensure generated files are in sdist tarball
    with tarfile.open(stgit_tgz) as tf:
        tf.getmember(str(prefix / 'completion' / 'stg.fish'))
        tf.getmember(str(prefix / 'completion' / 'stgit.bash'))
        tf.getmember(str(prefix / 'stgit' / 'commands' / 'cmdlist.py'))
        _version_tar_info = tf.getmember(str(prefix / 'stgit' / '_version.py'))
        assert 'def _get_version()' not in _version_tar_info.tobuf().decode()


def test_build(base, repo):
    log_test('test_build')
    build = base / 'build'
    check_call(
        [venv_py_exe(base), 'setup.py', 'build', '--build-base', build],
        cwd=repo,
        stdout=subprocess.DEVNULL,
    )

    # Ensure generated files exist in build tree
    assert (build / 'lib' / 'stgit').is_dir()
    assert (build / 'lib' / 'stgit' / 'templates').is_dir()
    assert (build / 'lib' / 'stgit' / 'templates' / 'patchmail.tmpl').is_file()
    assert (build / 'lib' / 'stgit' / 'commands' / 'cmdlist.py').is_file()

    # _version.py file in the built tree should be written with a static version
    assert (
        'def _get_version()'
        not in (build / 'lib' / 'stgit' / '_version.py').read_text()
    )

    # Ensure that generated artifacts also exist in worktree
    assert (repo / 'stgit' / 'commands' / 'cmdlist.py').is_file()
    assert (repo / 'completion' / 'stg.fish').is_file()
    assert (repo / 'completion' / 'stgit.bash').is_file()

    # The _version.py in the worktree should never be modified
    assert 'def _get_version()' in (repo / 'stgit' / '_version.py').read_text()

    # Ensure no checked-in files have been modified by the build
    check_call(['git', 'diff', '--quiet'], cwd=repo)


def test_bdist_wheel(base, repo):
    log_test('test_bdist_wheel')
    dist = base / 'dist-whl'
    check_call(
        [venv_py_exe(base), 'setup.py', 'bdist_wheel', '--dist-dir', dist],
        cwd=repo,
        stdout=subprocess.DEVNULL,
    )
    sdist_paths = list(dist.glob('stgit-*.whl'))
    assert len(sdist_paths) == 1
    stgit_whl = sdist_paths[0]
    log(f'{stgit_whl=}')
    version = version_from_wheel(stgit_whl)
    log(f'{version=}')

    # Ensure generated files exist in .whl file
    with zipfile.ZipFile(stgit_whl, 'r') as zf:
        zf.getinfo('stgit/commands/cmdlist.py')
        assert 'def _get_version' not in zf.read('stgit/_version.py').decode()
        zf.getinfo('stgit/templates/patchmail.tmpl')


def test_develop_mode(base, repo, cache):
    log_test('test_develop_mode')
    # PEP-518 build isolation means build-time deps are installed even
    # if they're already installed (i.e. already installed in the
    # virtual env). Thus --find-links is used with --no-index so that
    # wheel and setuptools can be installed without going to the network.
    check_call(
        [
            venv_py_exe(base),
            '-m',
            'pip',
            'install',
            '--no-index',
            '--find-links',
            cache,
            '-e',
            '.',
        ],
        cwd=repo,
    )

    # Gather versions in various ways and ensure they are all consistent
    py_c_stgit_version = (
        check_output(
            [venv_py_exe(base), '-c', 'import stgit; print(stgit.get_version())'],
            cwd=base,
        )
        .decode()
        .rstrip()
    )
    py_m_stgit_version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    assert (
        py_c_stgit_version == py_m_stgit_version
    ), f'{py_c_stgit_version=} {py_m_stgit_version=}'

    venv_stg_version = version_from_stg_version(
        check_output([venv_stg_exe(base), '--version'], cwd=base).decode()
    )
    assert (
        py_c_stgit_version == venv_stg_version
    ), f'{py_c_stgit_version=} {venv_stg_version=}'

    log(f'{py_c_stgit_version=}')
    assert 'dirty' not in py_m_stgit_version

    # Modify a checked-in file to make worktree dirty
    dirty_file = repo / 'README.md'
    with open(dirty_file, 'a') as f:
        print("DIRTY STUFF", file=f)

    py_c_dirty_version = (
        check_output(
            [venv_py_exe(base), '-c', 'import stgit; print(stgit.get_version())'],
            cwd=base,
        )
        .decode()
        .rstrip()
    )
    py_m_dirty_version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    assert (
        py_c_dirty_version == py_m_dirty_version
    ), f'{py_c_dirty_version=} {py_m_dirty_version=}'

    venv_std_dirty_version = version_from_stg_version(
        check_output([venv_stg_exe(base), '--version'], cwd=base).decode()
    )
    assert (
        py_c_dirty_version == venv_std_dirty_version
    ), f'{py_c_dirty_version=} {venv_std_dirty_version=}'

    log(f'{py_c_dirty_version=}')
    assert 'dirty' in py_m_dirty_version
    assert (
        f'{py_m_stgit_version}.dirty' == py_m_dirty_version  # Untagged
        or f'{py_m_stgit_version}+dirty' == py_m_dirty_version  # Tagged
    )

    # Restore worktree to pristine state
    check_call(['git', 'checkout', '--', dirty_file], cwd=repo)
    check_call(['git', 'diff', '--quiet'], cwd=repo)
    check_call(['git', 'clean', '-qfx'], cwd=repo)


def test_uninstall_from_venv(base):
    log_test('test_uninstall_from_venv')
    check_call(
        [venv_py_exe(base), '-m', 'pip', 'uninstall', '--yes', 'stgit'], cwd=base
    )

    venv_path = base / 'venv'
    assert not (venv_path / 'bin' / 'stg').exists()
    site_packages = next((venv_path / 'lib').glob('python3.*')) / 'site-packages'
    assert site_packages.is_dir()
    assert not list(site_packages.glob('stgit*'))
    share_dir = venv_path / 'share' / 'stgit'
    assert not share_dir.exists() or not list(share_dir.iterdir())


def test_running_tests_from_sdist(base):
    log_test('test_running_tests_from_sdist')
    work_sdist = next((base / 'work-sdist').iterdir())
    check_call(['./t0000-init.sh'], cwd=work_sdist / 't')


def test_install_from_unpacked_sdist(base, cache):
    log_test('test_install_from_unpacked_sdist')
    work_sdist = next((base / 'work-sdist').iterdir())

    check_call(
        [
            venv_py_exe(base),
            '-m',
            'pip',
            'install',
            '--no-index',
            '--find-links',
            cache,
            '.',
        ],
        cwd=work_sdist,
    )
    version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    assert work_sdist.name.endswith(version)


def test_install_sdist(base, cache):
    log_test('test_install_sdist')
    sdist_path = next((base / 'dist').iterdir())
    check_call(
        [
            venv_py_exe(base),
            '-m',
            'pip',
            'install',
            '--no-index',
            '--find-links',
            cache,
            sdist_path,
        ],
        cwd=base,
    )
    version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    assert sdist_path.name.split('stgit-', 1)[1].startswith(version)


def test_install_wheel(base):
    log_test('test_install_wheel')
    whl_path = next((base / 'dist-whl').iterdir())

    check_call(
        [venv_py_exe(base), '-m', 'pip', 'install', '--no-index', whl_path],
        cwd=base,
    )
    version = version_from_stg_version(
        check_output([venv_py_exe(base), '-m', 'stgit', '--version'], cwd=base).decode()
    )
    assert whl_path.name.split('stgit-', 1)[1].startswith(version)


if __name__ == '__main__':
    main()
