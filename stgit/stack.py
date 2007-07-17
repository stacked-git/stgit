"""Basic quilt-like functionality
"""

__copyright__ = """
Copyright (C) 2005, Catalin Marinas <catalin.marinas@gmail.com>

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

import sys, os, re

from stgit.utils import *
from stgit import git, basedir, templates
from stgit.config import config
from shutil import copyfile


# stack exception class
class StackException(Exception):
    pass

class FilterUntil:
    def __init__(self):
        self.should_print = True
    def __call__(self, x, until_test, prefix):
        if until_test(x):
            self.should_print = False
        if self.should_print:
            return x[0:len(prefix)] != prefix
        return False

#
# Functions
#
__comment_prefix = 'STG:'
__patch_prefix = 'STG_PATCH:'

def __clean_comments(f):
    """Removes lines marked for status in a commit file
    """
    f.seek(0)

    # remove status-prefixed lines
    lines = f.readlines()

    patch_filter = FilterUntil()
    until_test = lambda t: t == (__patch_prefix + '\n')
    lines = [l for l in lines if patch_filter(l, until_test, __comment_prefix)]

    # remove empty lines at the end
    while len(lines) != 0 and lines[-1] == '\n':
        del lines[-1]

    f.seek(0); f.truncate()
    f.writelines(lines)

def edit_file(series, line, comment, show_patch = True):
    fname = '.stgitmsg.txt'
    tmpl = templates.get_template('patchdescr.tmpl')

    f = file(fname, 'w+')
    if line:
        print >> f, line
    elif tmpl:
        print >> f, tmpl,
    else:
        print >> f
    print >> f, __comment_prefix, comment
    print >> f, __comment_prefix, \
          'Lines prefixed with "%s" will be automatically removed.' \
          % __comment_prefix
    print >> f, __comment_prefix, \
          'Trailing empty lines will be automatically removed.'

    if show_patch:
       print >> f, __patch_prefix
       # series.get_patch(series.get_current()).get_top()
       git.diff([], series.get_patch(series.get_current()).get_bottom(), None, f)

    #Vim modeline must be near the end.
    print >> f, __comment_prefix, 'vi: set textwidth=75 filetype=diff nobackup:'
    f.close()

    call_editor(fname)

    f = file(fname, 'r+')

    __clean_comments(f)
    f.seek(0)
    result = f.read()

    f.close()
    os.remove(fname)

    return result

#
# Classes
#

class StgitObject:
    """An object with stgit-like properties stored as files in a directory
    """
    def _set_dir(self, dir):
        self.__dir = dir
    def _dir(self):
        return self.__dir

    def create_empty_field(self, name):
        create_empty_file(os.path.join(self.__dir, name))

    def _get_field(self, name, multiline = False):
        id_file = os.path.join(self.__dir, name)
        if os.path.isfile(id_file):
            line = read_string(id_file, multiline)
            if line == '':
                return None
            else:
                return line
        else:
            return None

    def _set_field(self, name, value, multiline = False):
        fname = os.path.join(self.__dir, name)
        if value and value != '':
            write_string(fname, value, multiline)
        elif os.path.isfile(fname):
            os.remove(fname)

    
class Patch(StgitObject):
    """Basic patch implementation
    """
    def __init__(self, name, series_dir, refs_dir):
        self.__series_dir = series_dir
        self.__name = name
        self._set_dir(os.path.join(self.__series_dir, self.__name))
        self.__refs_dir = refs_dir
        self.__top_ref_file = os.path.join(self.__refs_dir, self.__name)
        self.__log_ref_file = os.path.join(self.__refs_dir,
                                           self.__name + '.log')

    def create(self):
        os.mkdir(self._dir())
        self.create_empty_field('bottom')
        self.create_empty_field('top')

    def delete(self):
        for f in os.listdir(self._dir()):
            os.remove(os.path.join(self._dir(), f))
        os.rmdir(self._dir())
        os.remove(self.__top_ref_file)
        if os.path.exists(self.__log_ref_file):
            os.remove(self.__log_ref_file)

    def get_name(self):
        return self.__name

    def rename(self, newname):
        olddir = self._dir()
        old_top_ref_file = self.__top_ref_file
        old_log_ref_file = self.__log_ref_file
        self.__name = newname
        self._set_dir(os.path.join(self.__series_dir, self.__name))
        self.__top_ref_file = os.path.join(self.__refs_dir, self.__name)
        self.__log_ref_file = os.path.join(self.__refs_dir,
                                           self.__name + '.log')

        os.rename(olddir, self._dir())
        os.rename(old_top_ref_file, self.__top_ref_file)
        if os.path.exists(old_log_ref_file):
            os.rename(old_log_ref_file, self.__log_ref_file)

    def __update_top_ref(self, ref):
        write_string(self.__top_ref_file, ref)

    def __update_log_ref(self, ref):
        write_string(self.__log_ref_file, ref)

    def update_top_ref(self):
        top = self.get_top()
        if top:
            self.__update_top_ref(top)

    def get_old_bottom(self):
        return self._get_field('bottom.old')

    def get_bottom(self):
        return self._get_field('bottom')

    def set_bottom(self, value, backup = False):
        if backup:
            curr = self._get_field('bottom')
            self._set_field('bottom.old', curr)
        self._set_field('bottom', value)

    def get_old_top(self):
        return self._get_field('top.old')

    def get_top(self):
        return self._get_field('top')

    def set_top(self, value, backup = False):
        if backup:
            curr = self._get_field('top')
            self._set_field('top.old', curr)
        self._set_field('top', value)
        self.__update_top_ref(value)

    def restore_old_boundaries(self):
        bottom = self._get_field('bottom.old')
        top = self._get_field('top.old')

        if top and bottom:
            self._set_field('bottom', bottom)
            self._set_field('top', top)
            self.__update_top_ref(top)
            return True
        else:
            return False

    def get_description(self):
        return self._get_field('description', True)

    def set_description(self, line):
        self._set_field('description', line, True)

    def get_authname(self):
        return self._get_field('authname')

    def set_authname(self, name):
        self._set_field('authname', name or git.author().name)

    def get_authemail(self):
        return self._get_field('authemail')

    def set_authemail(self, email):
        self._set_field('authemail', email or git.author().email)

    def get_authdate(self):
        return self._get_field('authdate')

    def set_authdate(self, date):
        self._set_field('authdate', date or git.author().date)

    def get_commname(self):
        return self._get_field('commname')

    def set_commname(self, name):
        self._set_field('commname', name or git.committer().name)

    def get_commemail(self):
        return self._get_field('commemail')

    def set_commemail(self, email):
        self._set_field('commemail', email or git.committer().email)

    def get_log(self):
        return self._get_field('log')

    def set_log(self, value, backup = False):
        self._set_field('log', value)
        self.__update_log_ref(value)

# The current StGIT metadata format version.
FORMAT_VERSION = 2

class PatchSet(StgitObject):
    def __init__(self, name = None):
        try:
            if name:
                self.set_name (name)
            else:
                self.set_name (git.get_head_file())
            self.__base_dir = basedir.get()
        except git.GitException, ex:
            raise StackException, 'GIT tree not initialised: %s' % ex

        self._set_dir(os.path.join(self.__base_dir, 'patches', self.get_name()))

    def get_name(self):
        return self.__name
    def set_name(self, name):
        self.__name = name

    def _basedir(self):
        return self.__base_dir

    def get_head(self):
        """Return the head of the branch
        """
        crt = self.get_current_patch()
        if crt:
            return crt.get_top()
        else:
            return self.get_base()

    def get_protected(self):
        return os.path.isfile(os.path.join(self._dir(), 'protected'))

    def protect(self):
        protect_file = os.path.join(self._dir(), 'protected')
        if not os.path.isfile(protect_file):
            create_empty_file(protect_file)

    def unprotect(self):
        protect_file = os.path.join(self._dir(), 'protected')
        if os.path.isfile(protect_file):
            os.remove(protect_file)

    def __branch_descr(self):
        return 'branch.%s.description' % self.get_name()

    def get_description(self):
        return config.get(self.__branch_descr()) or ''

    def set_description(self, line):
        if line:
            config.set(self.__branch_descr(), line)
        else:
            config.unset(self.__branch_descr())

    def head_top_equal(self):
        """Return true if the head and the top are the same
        """
        crt = self.get_current_patch()
        if not crt:
            # we don't care, no patches applied
            return True
        return git.get_head() == crt.get_top()

    def is_initialised(self):
        """Checks if series is already initialised
        """
        return bool(config.get(self.format_version_key()))


class Series(PatchSet):
    """Class including the operations on series
    """
    def __init__(self, name = None):
        """Takes a series name as the parameter.
        """
        PatchSet.__init__(self, name)

        # Update the branch to the latest format version if it is
        # initialized, but don't touch it if it isn't.
        self.update_to_current_format_version()

        self.__refs_dir = os.path.join(self._basedir(), 'refs', 'patches',
                                       self.get_name())

        self.__applied_file = os.path.join(self._dir(), 'applied')
        self.__unapplied_file = os.path.join(self._dir(), 'unapplied')
        self.__hidden_file = os.path.join(self._dir(), 'hidden')

        # where this series keeps its patches
        self.__patch_dir = os.path.join(self._dir(), 'patches')

        # trash directory
        self.__trash_dir = os.path.join(self._dir(), 'trash')

    def format_version_key(self):
        return 'branch.%s.stgit.stackformatversion' % self.get_name()

    def update_to_current_format_version(self):
        """Update a potentially older StGIT directory structure to the
        latest version. Note: This function should depend as little as
        possible on external functions that may change during a format
        version bump, since it must remain able to process older formats."""

        branch_dir = os.path.join(self._basedir(), 'patches', self.get_name())
        def get_format_version():
            """Return the integer format version number, or None if the
            branch doesn't have any StGIT metadata at all, of any version."""
            fv = config.get(self.format_version_key())
            ofv = config.get('branch.%s.stgitformatversion' % self.get_name())
            if fv:
                # Great, there's an explicitly recorded format version
                # number, which means that the branch is initialized and
                # of that exact version.
                return int(fv)
            elif ofv:
                # Old name for the version info, upgrade it
                config.set(self.format_version_key(), ofv)
                config.unset('branch.%s.stgitformatversion' % self.get_name())
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
            out.info('Upgraded branch %s to format version %d' % (self.get_name(), v))
            config.set(self.format_version_key(), '%d' % v)
        def mkdir(d):
            if not os.path.isdir(d):
                os.makedirs(d)
        def rm(f):
            if os.path.exists(f):
                os.remove(f)

        # Update 0 -> 1.
        if get_format_version() == 0:
            mkdir(os.path.join(branch_dir, 'trash'))
            patch_dir = os.path.join(branch_dir, 'patches')
            mkdir(patch_dir)
            refs_dir = os.path.join(self._basedir(), 'refs', 'patches', self.get_name())
            mkdir(refs_dir)
            for patch in (file(os.path.join(branch_dir, 'unapplied')).readlines()
                          + file(os.path.join(branch_dir, 'applied')).readlines()):
                patch = patch.strip()
                os.rename(os.path.join(branch_dir, patch),
                          os.path.join(patch_dir, patch))
                Patch(patch, patch_dir, refs_dir).update_top_ref()
            set_format_version(1)

        # Update 1 -> 2.
        if get_format_version() == 1:
            desc_file = os.path.join(branch_dir, 'description')
            if os.path.isfile(desc_file):
                desc = read_string(desc_file)
                if desc:
                    config.set('branch.%s.description' % self.get_name(), desc)
                rm(desc_file)
            rm(os.path.join(branch_dir, 'current'))
            rm(os.path.join(self._basedir(), 'refs', 'bases', self.get_name()))
            set_format_version(2)

        # Make sure we're at the latest version.
        if not get_format_version() in [None, FORMAT_VERSION]:
            raise StackException('Branch %s is at format version %d, expected %d'
                                 % (self.get_name(), get_format_version(), FORMAT_VERSION))

    def __patch_name_valid(self, name):
        """Raise an exception if the patch name is not valid.
        """
        if not name or re.search('[^\w.-]', name):
            raise StackException, 'Invalid patch name: "%s"' % name

    def get_patch(self, name):
        """Return a Patch object for the given name
        """
        return Patch(name, self.__patch_dir, self.__refs_dir)

    def get_current_patch(self):
        """Return a Patch object representing the topmost patch, or
        None if there is no such patch."""
        crt = self.get_current()
        if not crt:
            return None
        return Patch(crt, self.__patch_dir, self.__refs_dir)

    def get_current(self):
        """Return the name of the topmost patch, or None if there is
        no such patch."""
        try:
            applied = self.get_applied()
        except StackException:
            # No "applied" file: branch is not initialized.
            return None
        try:
            return applied[-1]
        except IndexError:
            # No patches applied.
            return None

    def get_applied(self):
        if not os.path.isfile(self.__applied_file):
            raise StackException, 'Branch "%s" not initialised' % self.get_name()
        return read_strings(self.__applied_file)

    def get_unapplied(self):
        if not os.path.isfile(self.__unapplied_file):
            raise StackException, 'Branch "%s" not initialised' % self.get_name()
        return read_strings(self.__unapplied_file)

    def get_hidden(self):
        if not os.path.isfile(self.__hidden_file):
            return []
        return read_strings(self.__hidden_file)

    def get_base(self):
        # Return the parent of the bottommost patch, if there is one.
        if os.path.isfile(self.__applied_file):
            bottommost = file(self.__applied_file).readline().strip()
            if bottommost:
                return self.get_patch(bottommost).get_bottom()
        # No bottommost patch, so just return HEAD
        return git.get_head()

    def get_parent_remote(self):
        value = config.get('branch.%s.remote' % self.get_name())
        if value:
            return value
        elif 'origin' in git.remotes_list():
            out.note(('No parent remote declared for stack "%s",'
                      ' defaulting to "origin".' % self.get_name()),
                     ('Consider setting "branch.%s.remote" and'
                      ' "branch.%s.merge" with "git repo-config".'
                      % (self.get_name(), self.get_name())))
            return 'origin'
        else:
            raise StackException, 'Cannot find a parent remote for "%s"' % self.get_name()

    def __set_parent_remote(self, remote):
        value = config.set('branch.%s.remote' % self.get_name(), remote)

    def get_parent_branch(self):
        value = config.get('branch.%s.stgit.parentbranch' % self.get_name())
        if value:
            return value
        elif git.rev_parse('heads/origin'):
            out.note(('No parent branch declared for stack "%s",'
                      ' defaulting to "heads/origin".' % self.get_name()),
                     ('Consider setting "branch.%s.stgit.parentbranch"'
                      ' with "git repo-config".' % self.get_name()))
            return 'heads/origin'
        else:
            raise StackException, 'Cannot find a parent branch for "%s"' % self.get_name()

    def __set_parent_branch(self, name):
        if config.get('branch.%s.remote' % self.get_name()):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.get_name(), name)
        config.set('branch.%s.stgit.parentbranch' % self.get_name(), name)

    def set_parent(self, remote, localbranch):
        if localbranch:
            self.__set_parent_remote(remote)
            self.__set_parent_branch(localbranch)
        # We'll enforce this later
#         else:
#             raise StackException, 'Parent branch (%s) should be specified for %s' % localbranch, self.get_name()

    def __patch_is_current(self, patch):
        return patch.get_name() == self.get_current()

    def patch_applied(self, name):
        """Return true if the patch exists in the applied list
        """
        return name in self.get_applied()

    def patch_unapplied(self, name):
        """Return true if the patch exists in the unapplied list
        """
        return name in self.get_unapplied()

    def patch_hidden(self, name):
        """Return true if the patch is hidden.
        """
        return name in self.get_hidden()

    def patch_exists(self, name):
        """Return true if there is a patch with the given name, false
        otherwise."""
        return self.patch_applied(name) or self.patch_unapplied(name) \
               or self.patch_hidden(name)

    def init(self, create_at=False, parent_remote=None, parent_branch=None):
        """Initialises the stgit series
        """
        if self.is_initialised():
            raise StackException, '%s already initialized' % self.get_name()
        for d in [self._dir(), self.__refs_dir]:
            if os.path.exists(d):
                raise StackException, '%s already exists' % d

        if (create_at!=False):
            git.create_branch(self.get_name(), create_at)

        os.makedirs(self.__patch_dir)

        self.set_parent(parent_remote, parent_branch)

        self.create_empty_field('applied')
        self.create_empty_field('unapplied')
        os.makedirs(self.__refs_dir)
        self._set_field('orig-base', git.get_head())

        config.set(self.format_version_key(), str(FORMAT_VERSION))

    def rename(self, to_name):
        """Renames a series
        """
        to_stack = Series(to_name)

        if to_stack.is_initialised():
            raise StackException, '"%s" already exists' % to_stack.get_name()

        git.rename_branch(self.get_name(), to_name)

        if os.path.isdir(self._dir()):
            rename(os.path.join(self._basedir(), 'patches'),
                   self.get_name(), to_stack.get_name())
        if os.path.exists(self.__refs_dir):
            rename(os.path.join(self._basedir(), 'refs', 'patches'),
                   self.get_name(), to_stack.get_name())

        # Rename the config section
        config.rename_section("branch.%s" % self.get_name(),
                              "branch.%s" % to_name)

        self.__init__(to_name)

    def clone(self, target_series):
        """Clones a series
        """
        try:
            # allow cloning of branches not under StGIT control
            base = self.get_base()
        except:
            base = git.get_head()
        Series(target_series).init(create_at = base)
        new_series = Series(target_series)

        # generate an artificial description file
        new_series.set_description('clone of "%s"' % self.get_name())

        # clone self's entire series as unapplied patches
        try:
            # allow cloning of branches not under StGIT control
            applied = self.get_applied()
            unapplied = self.get_unapplied()
            patches = applied + unapplied
            patches.reverse()
        except:
            patches = applied = unapplied = []
        for p in patches:
            patch = self.get_patch(p)
            newpatch = new_series.new_patch(p, message = patch.get_description(),
                                            can_edit = False, unapplied = True,
                                            bottom = patch.get_bottom(),
                                            top = patch.get_top(),
                                            author_name = patch.get_authname(),
                                            author_email = patch.get_authemail(),
                                            author_date = patch.get_authdate())
            if patch.get_log():
                out.info('Setting log to %s' %  patch.get_log())
                newpatch.set_log(patch.get_log())
            else:
                out.info('No log for %s' % p)

        # fast forward the cloned series to self's top
        new_series.forward_patches(applied)

        # Clone parent informations
        value = config.get('branch.%s.remote' % self.get_name())
        if value:
            config.set('branch.%s.remote' % target_series, value)

        value = config.get('branch.%s.merge' % self.get_name())
        if value:
            config.set('branch.%s.merge' % target_series, value)

        value = config.get('branch.%s.stgit.parentbranch' % self.get_name())
        if value:
            config.set('branch.%s.stgit.parentbranch' % target_series, value)

    def delete(self, force = False):
        """Deletes an stgit series
        """
        if self.is_initialised():
            patches = self.get_unapplied() + self.get_applied()
            if not force and patches:
                raise StackException, \
                      'Cannot delete: the series still contains patches'
            for p in patches:
                Patch(p, self.__patch_dir, self.__refs_dir).delete()

            # remove the trash directory if any
            if os.path.exists(self.__trash_dir):
                for fname in os.listdir(self.__trash_dir):
                    os.remove(os.path.join(self.__trash_dir, fname))
                os.rmdir(self.__trash_dir)

            # FIXME: find a way to get rid of those manual removals
            # (move functionality to StgitObject ?)
            if os.path.exists(self.__applied_file):
                os.remove(self.__applied_file)
            if os.path.exists(self.__unapplied_file):
                os.remove(self.__unapplied_file)
            if os.path.exists(self.__hidden_file):
                os.remove(self.__hidden_file)
            if os.path.exists(self._dir()+'/orig-base'):
                os.remove(self._dir()+'/orig-base')

            if not os.listdir(self.__patch_dir):
                os.rmdir(self.__patch_dir)
            else:
                out.warn('Patch directory %s is not empty' % self.__patch_dir)

            try:
                os.removedirs(self._dir())
            except OSError:
                raise StackException('Series directory %s is not empty'
                                     % self._dir())

            try:
                os.removedirs(self.__refs_dir)
            except OSError:
                out.warn('Refs directory %s is not empty' % self.__refs_dir)

        # Cleanup parent informations
        # FIXME: should one day make use of git-config --section-remove,
        # scheduled for 1.5.1
        config.unset('branch.%s.remote' % self.get_name())
        config.unset('branch.%s.merge' % self.get_name())
        config.unset('branch.%s.stgit.parentbranch' % self.get_name())
        config.unset(self.format_version_key())

    def refresh_patch(self, files = None, message = None, edit = False,
                      show_patch = False,
                      cache_update = True,
                      author_name = None, author_email = None,
                      author_date = None,
                      committer_name = None, committer_email = None,
                      backup = False, sign_str = None, log = 'refresh',
                      notes = None):
        """Generates a new commit for the given patch
        """
        name = self.get_current()
        if not name:
            raise StackException, 'No patches applied'

        patch = Patch(name, self.__patch_dir, self.__refs_dir)

        descr = patch.get_description()
        if not (message or descr):
            edit = True
            descr = ''
        elif message:
            descr = message

        if not message and edit:
            descr = edit_file(self, descr.rstrip(), \
                              'Please edit the description for patch "%s" ' \
                              'above.' % name, show_patch)

        if not author_name:
            author_name = patch.get_authname()
        if not author_email:
            author_email = patch.get_authemail()
        if not author_date:
            author_date = patch.get_authdate()
        if not committer_name:
            committer_name = patch.get_commname()
        if not committer_email:
            committer_email = patch.get_commemail()

        if sign_str:
            descr = descr.rstrip()
            if descr.find("\nSigned-off-by:") < 0 \
               and descr.find("\nAcked-by:") < 0:
                descr = descr + "\n"

            descr = '%s\n%s: %s <%s>\n' % (descr, sign_str,
                                           committer_name, committer_email)

        bottom = patch.get_bottom()

        commit_id = git.commit(files = files,
                               message = descr, parents = [bottom],
                               cache_update = cache_update,
                               allowempty = True,
                               author_name = author_name,
                               author_email = author_email,
                               author_date = author_date,
                               committer_name = committer_name,
                               committer_email = committer_email)

        patch.set_bottom(bottom, backup = backup)
        patch.set_top(commit_id, backup = backup)
        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

        if log:
            self.log_patch(patch, log, notes)

        return commit_id

    def undo_refresh(self):
        """Undo the patch boundaries changes caused by 'refresh'
        """
        name = self.get_current()
        assert(name)

        patch = Patch(name, self.__patch_dir, self.__refs_dir)
        old_bottom = patch.get_old_bottom()
        old_top = patch.get_old_top()

        # the bottom of the patch is not changed by refresh. If the
        # old_bottom is different, there wasn't any previous 'refresh'
        # command (probably only a 'push')
        if old_bottom != patch.get_bottom() or old_top == patch.get_top():
            raise StackException, 'No undo information available'

        git.reset(tree_id = old_top, check_out = False)
        if patch.restore_old_boundaries():
            self.log_patch(patch, 'undo')

    def new_patch(self, name, message = None, can_edit = True,
                  unapplied = False, show_patch = False,
                  top = None, bottom = None, commit = True,
                  author_name = None, author_email = None, author_date = None,
                  committer_name = None, committer_email = None,
                  before_existing = False):
        """Creates a new patch
        """

        if name != None:
            self.__patch_name_valid(name)
            if self.patch_exists(name):
                raise StackException, 'Patch "%s" already exists' % name

        if not message and can_edit:
            descr = edit_file(
                self, None,
                'Please enter the description for the patch above.',
                show_patch)
        else:
            descr = message

        head = git.get_head()

        if name == None:
            name = make_patch_name(descr, self.patch_exists)

        patch = Patch(name, self.__patch_dir, self.__refs_dir)
        patch.create()

        if not bottom:
            bottom = head
        if not top:
            top = head

        patch.set_bottom(bottom)
        patch.set_top(top)
        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

        if before_existing:
            insert_string(self.__applied_file, patch.get_name())
            # no need to commit anything as the object is already
            # present (mainly used by 'uncommit')
            commit = False
        elif unapplied:
            patches = [patch.get_name()] + self.get_unapplied()
            write_strings(self.__unapplied_file, patches)
            set_head = False
        else:
            append_string(self.__applied_file, patch.get_name())
            set_head = True

        if commit:
            # create a commit for the patch (may be empty if top == bottom);
            # only commit on top of the current branch
            assert(unapplied or bottom == head)
            top_commit = git.get_commit(top)
            commit_id = git.commit(message = descr, parents = [bottom],
                                   cache_update = False,
                                   tree_id = top_commit.get_tree(),
                                   allowempty = True, set_head = set_head,
                                   author_name = author_name,
                                   author_email = author_email,
                                   author_date = author_date,
                                   committer_name = committer_name,
                                   committer_email = committer_email)
            # set the patch top to the new commit
            patch.set_top(commit_id)

        self.log_patch(patch, 'new')

        return patch

    def delete_patch(self, name):
        """Deletes a patch
        """
        self.__patch_name_valid(name)
        patch = Patch(name, self.__patch_dir, self.__refs_dir)

        if self.__patch_is_current(patch):
            self.pop_patch(name)
        elif self.patch_applied(name):
            raise StackException, 'Cannot remove an applied patch, "%s", ' \
                  'which is not current' % name
        elif not name in self.get_unapplied():
            raise StackException, 'Unknown patch "%s"' % name

        # save the commit id to a trash file
        write_string(os.path.join(self.__trash_dir, name), patch.get_top())

        patch.delete()

        unapplied = self.get_unapplied()
        unapplied.remove(name)
        write_strings(self.__unapplied_file, unapplied)

    def forward_patches(self, names):
        """Try to fast-forward an array of patches.

        On return, patches in names[0:returned_value] have been pushed on the
        stack. Apply the rest with push_patch
        """
        unapplied = self.get_unapplied()

        forwarded = 0
        top = git.get_head()

        for name in names:
            assert(name in unapplied)

            patch = Patch(name, self.__patch_dir, self.__refs_dir)

            head = top
            bottom = patch.get_bottom()
            top = patch.get_top()

            # top != bottom always since we have a commit for each patch
            if head == bottom:
                # reset the backup information. No logging since the
                # patch hasn't changed
                patch.set_bottom(head, backup = True)
                patch.set_top(top, backup = True)

            else:
                head_tree = git.get_commit(head).get_tree()
                bottom_tree = git.get_commit(bottom).get_tree()
                if head_tree == bottom_tree:
                    # We must just reparent this patch and create a new commit
                    # for it
                    descr = patch.get_description()
                    author_name = patch.get_authname()
                    author_email = patch.get_authemail()
                    author_date = patch.get_authdate()
                    committer_name = patch.get_commname()
                    committer_email = patch.get_commemail()

                    top_tree = git.get_commit(top).get_tree()

                    top = git.commit(message = descr, parents = [head],
                                     cache_update = False,
                                     tree_id = top_tree,
                                     allowempty = True,
                                     author_name = author_name,
                                     author_email = author_email,
                                     author_date = author_date,
                                     committer_name = committer_name,
                                     committer_email = committer_email)

                    patch.set_bottom(head, backup = True)
                    patch.set_top(top, backup = True)

                    self.log_patch(patch, 'push(f)')
                else:
                    top = head
                    # stop the fast-forwarding, must do a real merge
                    break

            forwarded+=1
            unapplied.remove(name)

        if forwarded == 0:
            return 0

        git.switch(top)

        append_strings(self.__applied_file, names[0:forwarded])
        write_strings(self.__unapplied_file, unapplied)

        return forwarded

    def merged_patches(self, names):
        """Test which patches were merged upstream by reverse-applying
        them in reverse order. The function returns the list of
        patches detected to have been applied. The state of the tree
        is restored to the original one
        """
        patches = [Patch(name, self.__patch_dir, self.__refs_dir)
                   for name in names]
        patches.reverse()

        merged = []
        for p in patches:
            if git.apply_diff(p.get_top(), p.get_bottom()):
                merged.append(p.get_name())
        merged.reverse()

        git.reset()

        return merged

    def push_patch(self, name, empty = False):
        """Pushes a patch on the stack
        """
        unapplied = self.get_unapplied()
        assert(name in unapplied)

        patch = Patch(name, self.__patch_dir, self.__refs_dir)

        head = git.get_head()
        bottom = patch.get_bottom()
        top = patch.get_top()

        ex = None
        modified = False

        # top != bottom always since we have a commit for each patch
        if empty:
            # just make an empty patch (top = bottom = HEAD). This
            # option is useful to allow undoing already merged
            # patches. The top is updated by refresh_patch since we
            # need an empty commit
            patch.set_bottom(head, backup = True)
            patch.set_top(head, backup = True)
            modified = True
        elif head == bottom:
            # reset the backup information. No need for logging
            patch.set_bottom(bottom, backup = True)
            patch.set_top(top, backup = True)

            git.switch(top)
        else:
            # new patch needs to be refreshed.
            # The current patch is empty after merge.
            patch.set_bottom(head, backup = True)
            patch.set_top(head, backup = True)

            # Try the fast applying first. If this fails, fall back to the
            # three-way merge
            if not git.apply_diff(bottom, top):
                # if git.apply_diff() fails, the patch requires a diff3
                # merge and can be reported as modified
                modified = True

                # merge can fail but the patch needs to be pushed
                try:
                    git.merge(bottom, head, top, recursive = True)
                except git.GitException, ex:
                    out.error('The merge failed during "push".',
                              'Use "refresh" after fixing the conflicts or'
                              ' revert the operation with "push --undo".')

        append_string(self.__applied_file, name)

        unapplied.remove(name)
        write_strings(self.__unapplied_file, unapplied)

        # head == bottom case doesn't need to refresh the patch
        if empty or head != bottom:
            if not ex:
                # if the merge was OK and no conflicts, just refresh the patch
                # The GIT cache was already updated by the merge operation
                if modified:
                    log = 'push(m)'
                else:
                    log = 'push'
                self.refresh_patch(cache_update = False, log = log)
            else:
                # we store the correctly merged files only for
                # tracking the conflict history. Note that the
                # git.merge() operations should always leave the index
                # in a valid state (i.e. only stage 0 files)
                self.refresh_patch(cache_update = False, log = 'push(c)')
                raise StackException, str(ex)

        return modified

    def undo_push(self):
        name = self.get_current()
        assert(name)

        patch = Patch(name, self.__patch_dir, self.__refs_dir)
        old_bottom = patch.get_old_bottom()
        old_top = patch.get_old_top()

        # the top of the patch is changed by a push operation only
        # together with the bottom (otherwise the top was probably
        # modified by 'refresh'). If they are both unchanged, there
        # was a fast forward
        if old_bottom == patch.get_bottom() and old_top != patch.get_top():
            raise StackException, 'No undo information available'

        git.reset()
        self.pop_patch(name)
        ret = patch.restore_old_boundaries()
        if ret:
            self.log_patch(patch, 'undo')

        return ret

    def pop_patch(self, name, keep = False):
        """Pops the top patch from the stack
        """
        applied = self.get_applied()
        applied.reverse()
        assert(name in applied)

        patch = Patch(name, self.__patch_dir, self.__refs_dir)

        if git.get_head_file() == self.get_name():
            if keep and not git.apply_diff(git.get_head(), patch.get_bottom()):
                raise StackException(
                    'Failed to pop patches while preserving the local changes')
            git.switch(patch.get_bottom(), keep)
        else:
            git.set_branch(self.get_name(), patch.get_bottom())

        # save the new applied list
        idx = applied.index(name) + 1

        popped = applied[:idx]
        popped.reverse()
        unapplied = popped + self.get_unapplied()
        write_strings(self.__unapplied_file, unapplied)

        del applied[:idx]
        applied.reverse()
        write_strings(self.__applied_file, applied)

    def empty_patch(self, name):
        """Returns True if the patch is empty
        """
        self.__patch_name_valid(name)
        patch = Patch(name, self.__patch_dir, self.__refs_dir)
        bottom = patch.get_bottom()
        top = patch.get_top()

        if bottom == top:
            return True
        elif git.get_commit(top).get_tree() \
                 == git.get_commit(bottom).get_tree():
            return True

        return False

    def rename_patch(self, oldname, newname):
        self.__patch_name_valid(newname)

        applied = self.get_applied()
        unapplied = self.get_unapplied()

        if oldname == newname:
            raise StackException, '"To" name and "from" name are the same'

        if newname in applied or newname in unapplied:
            raise StackException, 'Patch "%s" already exists' % newname

        if oldname in unapplied:
            Patch(oldname, self.__patch_dir, self.__refs_dir).rename(newname)
            unapplied[unapplied.index(oldname)] = newname
            write_strings(self.__unapplied_file, unapplied)
        elif oldname in applied:
            Patch(oldname, self.__patch_dir, self.__refs_dir).rename(newname)

            applied[applied.index(oldname)] = newname
            write_strings(self.__applied_file, applied)
        else:
            raise StackException, 'Unknown patch "%s"' % oldname

    def log_patch(self, patch, message, notes = None):
        """Generate a log commit for a patch
        """
        top = git.get_commit(patch.get_top())
        old_log = patch.get_log()

        if message is None:
            # replace the current log entry
            if not old_log:
                raise StackException, \
                      'No log entry to annotate for patch "%s"' \
                      % patch.get_name()
            replace = True
            log_commit = git.get_commit(old_log)
            msg = log_commit.get_log().split('\n')[0]
            log_parent = log_commit.get_parent()
            if log_parent:
                parents = [log_parent]
            else:
                parents = []
        else:
            # generate a new log entry
            replace = False
            msg = '%s\t%s' % (message, top.get_id_hash())
            if old_log:
                parents = [old_log]
            else:
                parents = []

        if notes:
            msg += '\n\n' + notes

        log = git.commit(message = msg, parents = parents,
                         cache_update = False, tree_id = top.get_tree(),
                         allowempty = True)
        patch.set_log(log)

    def hide_patch(self, name):
        """Add the patch to the hidden list.
        """
        unapplied = self.get_unapplied()
        if name not in unapplied:
            # keep the checking order for backward compatibility with
            # the old hidden patches functionality
            if self.patch_applied(name):
                raise StackException, 'Cannot hide applied patch "%s"' % name
            elif self.patch_hidden(name):
                raise StackException, 'Patch "%s" already hidden' % name
            else:
                raise StackException, 'Unknown patch "%s"' % name

        if not self.patch_hidden(name):
            # check needed for backward compatibility with the old
            # hidden patches functionality
            append_string(self.__hidden_file, name)

        unapplied.remove(name)
        write_strings(self.__unapplied_file, unapplied)

    def unhide_patch(self, name):
        """Remove the patch from the hidden list.
        """
        hidden = self.get_hidden()
        if not name in hidden:
            if self.patch_applied(name) or self.patch_unapplied(name):
                raise StackException, 'Patch "%s" not hidden' % name
            else:
                raise StackException, 'Unknown patch "%s"' % name

        hidden.remove(name)
        write_strings(self.__hidden_file, hidden)

        if not self.patch_applied(name) and not self.patch_unapplied(name):
            # check needed for backward compatibility with the old
            # hidden patches functionality
            append_string(self.__unapplied_file, name)
