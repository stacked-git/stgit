import json
import os
import shutil

from stgit.config import config
from stgit.exception import StackException
from stgit.lib.git.objects import BlobData, CommitData, TreeData
from stgit.out import out
from stgit.run import RunException

# The current StGit metadata format version.
FORMAT_VERSION = 5


def _format_version_key(branch):
    return 'branch.%s.stgit.stackformatversion' % branch


def _read_strings(filename):
    """Reads the lines from a file"""
    with open(filename, encoding='utf-8') as f:
        return [line.strip() for line in f.readlines()]


def _read_string(filename):
    """Reads the first line from a file"""
    with open(filename, encoding='utf-8') as f:
        return f.readline().strip()


def _try_rm(f):
    if os.path.exists(f):
        os.remove(f)


def update_to_current_format_version(repository, branch):
    """Update a potentially older StGit directory structure to the latest version.

    Note: This function should depend as little as possible on external functions that
    may change during a format version bump, since it must remain able to process older
    formats.

    """

    patches_dir = os.path.join(repository.directory, 'patches')
    branch_dir = os.path.join(patches_dir, branch)
    old_format_key = _format_version_key(branch)
    older_format_key = 'branch.%s.stgitformatversion' % branch

    def get_meta_file_version():
        """Get format version from the ``meta`` file in the stack log branch."""
        new_version = get_stack_json_file_version()
        if new_version is not None:
            return new_version

        old_version = get_old_meta_file_version()
        if old_version is not None:
            return old_version

    def get_old_meta_file_version():
        """Get format version from the ``meta`` file in the stack log branch."""
        stack_ref = 'refs/heads/%s.stgit:meta' % branch
        try:
            lines = (
                repository.run(['git', 'show', stack_ref])
                .discard_stderr()
                .output_lines()
            )
        except RunException:
            return None

        for line in lines:
            if line.startswith('Version: '):
                return int(line.split('Version: ', 1)[1])
        else:
            return None

    def get_stack_json_file_version():
        stack_ref = 'refs/stacks/%s:stack.json' % branch
        try:
            data = (
                repository.run(['git', 'show', stack_ref])
                .decoding('utf-8')
                .discard_stderr()
                .raw_output()
            )
        except RunException:
            return None

        try:
            stack_json = json.loads(data)
        except json.JSONDecodeError:
            return None
        else:
            return stack_json.get('version')

    def get_format_version():
        """Return the integer format version number.

        :returns: the format version number or None if the branch does not have any
                  StGit metadata at all, of any version

        """
        mfv = get_meta_file_version()
        if mfv is not None and mfv >= 4:
            # Modern-era format version found in branch meta blob.
            return mfv

        # Older format versions were stored in the Git config.
        fv = config.get(old_format_key)
        ofv = config.get(older_format_key)
        if fv:
            # Great, there's an explicitly recorded format version
            # number, which means that the branch is initialized and
            # of that exact version.
            return int(fv)
        elif ofv:
            # Old name for the version info: upgrade it.
            config.set(old_format_key, ofv)
            config.unset(older_format_key)
            return int(ofv)
        elif os.path.isdir(os.path.join(branch_dir, 'patches')):
            # There's a .git/patches/<branch>/patches dirctory, which
            # means this is an initialized version 1 branch.
            return 1
        elif os.path.isdir(branch_dir):
            # There's a .git/patches/<branch> directory, which means
            # this is an initialized version 0 branch.
            return 0
        else:
            # The branch doesn't seem to be initialized at all.
            return None

    def set_format_version_in_config(v):
        out.info('Upgraded branch %s to format version %d' % (branch, v))
        config.set(old_format_key, '%d' % v)

    def rm_ref(ref):
        if repository.refs.exists(ref):
            repository.refs.delete(ref)

    # Update 0 -> 1.
    if get_format_version() == 0:
        os.makedirs(os.path.join(branch_dir, 'trash'), exist_ok=True)
        patch_dir = os.path.join(branch_dir, 'patches')
        os.makedirs(patch_dir, exist_ok=True)
        refs_base = 'refs/patches/%s' % branch
        with open(os.path.join(branch_dir, 'unapplied')) as f:
            patches = f.readlines()
        with open(os.path.join(branch_dir, 'applied')) as f:
            patches.extend(f.readlines())
        for patch in patches:
            patch = patch.strip()
            os.rename(os.path.join(branch_dir, patch), os.path.join(patch_dir, patch))
            topfield = os.path.join(patch_dir, patch, 'top')
            if os.path.isfile(topfield):
                top = _read_string(topfield)
            else:
                top = None
            if top:
                repository.refs.set(
                    refs_base + '/' + patch,
                    repository.get_commit(top),
                    'StGit upgrade',
                )
        set_format_version_in_config(1)

    # Update 1 -> 2.
    if get_format_version() == 1:
        desc_file = os.path.join(branch_dir, 'description')
        if os.path.isfile(desc_file):
            desc = _read_string(desc_file)
            if desc:
                config.set('branch.%s.description' % branch, desc)
            _try_rm(desc_file)
        _try_rm(os.path.join(branch_dir, 'current'))
        rm_ref('refs/bases/%s' % branch)
        set_format_version_in_config(2)

    # Update 2 -> 3
    if get_format_version() == 2:
        protect_file = os.path.join(branch_dir, 'protected')
        if os.path.isfile(protect_file):
            config.set('branch.%s.stgit.protect' % branch, 'true')
            os.remove(protect_file)
        set_format_version_in_config(3)

    # compatibility with the new infrastructure. The changes here do not
    # affect the compatibility with the old infrastructure (format version 2)
    if get_format_version() == 3:
        os.makedirs(branch_dir, exist_ok=True)
        hidden_file = os.path.join(branch_dir, 'hidden')
        if not os.path.isfile(hidden_file):
            open(hidden_file, 'w+', encoding='utf-8').close()

        applied_file = os.path.join(branch_dir, 'applied')
        unapplied_file = os.path.join(branch_dir, 'unapplied')

        applied = _read_strings(applied_file)
        unapplied = _read_strings(unapplied_file)
        hidden = _read_strings(hidden_file)

        state_ref = 'refs/heads/%s.stgit' % branch

        head = repository.refs.get('refs/heads/%s' % branch)
        parents = [head]
        meta_lines = [
            'Version: 4',
            'Previous: None',
            'Head: %s' % head.sha1,
        ]

        patches_tree = {}

        for patch_list, title in [
            (applied, 'Applied'),
            (unapplied, 'Unapplied'),
            (hidden, 'Hidden'),
        ]:
            meta_lines.append('%s:' % title)
            for i, pn in enumerate(patch_list):
                patch_ref = 'refs/patches/%s/%s' % (branch, pn)
                commit = repository.refs.get(patch_ref)
                meta_lines.append('  %s: %s' % (pn, commit.sha1))
                if title != 'Applied' or i == len(patch_list) - 1:
                    if commit not in parents:
                        parents.append(commit)
                cd = commit.data
                patch_meta = '\n'.join(
                    [
                        'Bottom: %s' % cd.parent.data.tree.sha1,
                        'Top:    %s' % cd.tree.sha1,
                        'Author: %s' % cd.author.name_email,
                        'Date:   %s' % cd.author.date,
                        '',
                        cd.message_str,
                    ]
                ).encode('utf-8')
                patches_tree[pn] = repository.commit(BlobData(patch_meta))
        meta_lines.append('')

        meta = '\n'.join(meta_lines).encode('utf-8')
        tree = repository.commit(
            TreeData(
                {
                    'meta': repository.commit(BlobData(meta)),
                    'patches': repository.commit(TreeData(patches_tree)),
                }
            )
        )
        state_commit = repository.commit(
            CommitData(
                tree=tree,
                message='stack upgrade to version 4',
                parents=parents,
            )
        )
        repository.refs.set(state_ref, state_commit, 'stack upgrade to v4')

        for patch_list in [applied, unapplied, hidden]:
            for pn in patch_list:
                patch_log_ref = 'refs/patches/%s/%s.log' % (branch, pn)
                if repository.refs.exists(patch_log_ref):
                    repository.refs.delete(patch_log_ref)

        config.unset(old_format_key)

        shutil.rmtree(branch_dir)
        try:
            # .git/patches will be removed after the last stack is converted
            os.rmdir(patches_dir)
        except OSError:
            pass
        out.info('Upgraded branch %s to format version %d' % (branch, 4))

    # Metadata moves from refs/heads/<branch>.stgit to refs/stacks/<branch>.
    # Also, metadata file format is JSON instead of custom format.
    if get_format_version() == 4:
        old_state_ref = 'refs/heads/%s.stgit' % branch
        old_state = repository.refs.get(old_state_ref)
        old_meta = old_state.data.tree.data['meta'][1].data.bytes
        lines = old_meta.decode('utf-8').splitlines()
        if not lines[0].startswith('Version: 4'):
            raise StackException('Malformed metadata (expected version 4)')

        parsed = {}
        key = None
        for line in lines:
            if line.startswith(' '):
                assert key is not None
                parsed[key].append(line.strip())
            else:
                key, val = [x.strip() for x in line.split(':', 1)]
                if val:
                    parsed[key] = val
                else:
                    parsed[key] = []

        head = repository.refs.get('refs/heads/%s' % branch)

        new_meta = dict(
            version=5,
            prev=parsed['Previous'],
            head=head.sha1,
            applied=[],
            unapplied=[],
            hidden=[],
            patches=dict(),
        )

        if parsed['Head'] != new_meta['head']:
            raise StackException('Unexpected head mismatch')

        for patch_list_name in ['Applied', 'Unapplied', 'Hidden']:
            for entry in parsed[patch_list_name]:
                pn, sha1 = [x.strip() for x in entry.split(':')]
                new_patch_list_name = patch_list_name.lower()
                new_meta[new_patch_list_name].append(pn)
                new_meta['patches'][pn] = dict(oid=sha1)

        meta_bytes = json.dumps(new_meta, indent=2).encode('utf-8')

        tree = repository.commit(
            TreeData(
                {
                    'stack.json': repository.commit(BlobData(meta_bytes)),
                    'patches': old_state.data.tree.data['patches'],
                }
            )
        )

        repository.refs.set(
            'refs/stacks/%s' % branch,
            repository.commit(
                CommitData(
                    tree=tree,
                    message='stack upgrade to version 5',
                    parents=[head],
                )
            ),
            'stack upgrade to v5',
        )

        repository.refs.delete(old_state_ref)
        out.info('Upgraded branch %s to format version %d' % (branch, 5))

    # Make sure we're at the latest version.
    fv = get_format_version()
    if fv not in [None, FORMAT_VERSION]:
        raise StackException(
            'Branch %s is at format version %d, expected %d'
            % (branch, fv, FORMAT_VERSION)
        )
    return fv is not None  # true if branch is initialized
