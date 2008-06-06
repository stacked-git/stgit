import os.path
from stgit import utils
from stgit.out import out
from stgit.config import config

# The current StGit metadata format version.
FORMAT_VERSION = 2

def format_version_key(branch):
    return 'branch.%s.stgit.stackformatversion' % branch

def update_to_current_format_version(repository, branch):
    """Update a potentially older StGit directory structure to the latest
    version. Note: This function should depend as little as possible
    on external functions that may change during a format version
    bump, since it must remain able to process older formats."""

    branch_dir = os.path.join(repository.directory, 'patches', branch)
    key = format_version_key(branch)
    old_key = 'branch.%s.stgitformatversion' % branch
    def get_format_version():
        """Return the integer format version number, or None if the
        branch doesn't have any StGit metadata at all, of any version."""
        fv = config.get(key)
        ofv = config.get(old_key)
        if fv:
            # Great, there's an explicitly recorded format version
            # number, which means that the branch is initialized and
            # of that exact version.
            return int(fv)
        elif ofv:
            # Old name for the version info: upgrade it.
            config.set(key, ofv)
            config.unset(old_key)
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
    def set_format_version(v):
        out.info('Upgraded branch %s to format version %d' % (branch, v))
        config.set(key, '%d' % v)
    def mkdir(d):
        if not os.path.isdir(d):
            os.makedirs(d)
    def rm(f):
        if os.path.exists(f):
            os.remove(f)
    def rm_ref(ref):
        if repository.refs.exists(ref):
            repository.refs.delete(ref)

    # Update 0 -> 1.
    if get_format_version() == 0:
        mkdir(os.path.join(branch_dir, 'trash'))
        patch_dir = os.path.join(branch_dir, 'patches')
        mkdir(patch_dir)
        refs_base = 'refs/patches/%s' % branch
        for patch in (file(os.path.join(branch_dir, 'unapplied')).readlines()
                      + file(os.path.join(branch_dir, 'applied')).readlines()):
            patch = patch.strip()
            os.rename(os.path.join(branch_dir, patch),
                      os.path.join(patch_dir, patch))
            topfield = os.path.join(patch_dir, patch, 'top')
            if os.path.isfile(topfield):
                top = utils.read_string(topfield, False)
            else:
                top = None
            if top:
                repository.refs.set(refs_base + '/' + patch,
                                    repository.get_commit(top), 'StGit upgrade')
        set_format_version(1)

    # Update 1 -> 2.
    if get_format_version() == 1:
        desc_file = os.path.join(branch_dir, 'description')
        if os.path.isfile(desc_file):
            desc = utils.read_string(desc_file)
            if desc:
                config.set('branch.%s.description' % branch, desc)
            rm(desc_file)
        rm(os.path.join(branch_dir, 'current'))
        rm_ref('refs/bases/%s' % branch)
        set_format_version(2)

    # compatibility with the new infrastructure. The changes here do not
    # affect the compatibility with the old infrastructure (format version 2)
    if get_format_version() == 2:
        hidden_file = os.path.join(branch_dir, 'hidden')
        if not os.path.isfile(hidden_file):
            utils.create_empty_file(hidden_file)

    # Make sure we're at the latest version.
    fv = get_format_version()
    if not fv in [None, FORMAT_VERSION]:
        raise StackException('Branch %s is at format version %d, expected %d'
                             % (branch, fv, FORMAT_VERSION))
    return fv != None # true if branch is initialized
