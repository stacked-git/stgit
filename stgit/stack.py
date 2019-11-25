# -*- coding: utf-8 -*-
"""Basic quilt-like functionality"""

from __future__ import (
    absolute_import,
    division,
    print_function,
    unicode_literals,
)

from email.utils import formatdate
import os
import re

from stgit import basedir, git, templates
from stgit.compat import text
from stgit.config import config
from stgit.exception import StackException
from stgit.lib import stackupgrade
from stgit.lib.git import Repository
from stgit.out import out
from stgit.utils import (
    add_sign_line,
    append_string,
    append_strings,
    call_editor,
    create_empty_file,
    make_patch_name,
    read_string,
    read_strings,
    rename,
    write_string,
    write_strings,
)

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""


class FilterUntil(object):
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

    def until_test(t):
        return t == (__patch_prefix + '\n')

    lines = [l for l in lines if patch_filter(l, until_test, __comment_prefix)]

    # remove empty lines at the end
    while len(lines) != 0 and lines[-1] == '\n':
        del lines[-1]

    f.seek(0)
    f.truncate()
    f.writelines(lines)


# TODO: move this out of the stgit.stack module, it is really for
# higher level commands to handle the user interaction
def edit_file(series, line, comment, show_patch=True):
    fname = '.stgitmsg.txt'
    tmpl = templates.get_template('patchdescr.tmpl')

    with open(fname, 'w+') as f:
        if line:
            print(line, file=f)
        elif tmpl:
            print(tmpl, end=' ', file=f)
        else:
            print(file=f)
        print(__comment_prefix, comment, file=f)
        print(__comment_prefix,
              'Lines prefixed with "%s" will be automatically removed.'
              % __comment_prefix, file=f)
        print(__comment_prefix,
              'Trailing empty lines will be automatically removed.', file=f)

        if show_patch:
            print(__patch_prefix, file=f)
            # series.get_patch(series.get_current()).get_top()
            diff_str = git.diff(
                rev1=series.get_patch(series.get_current()).get_bottom()
            )
            f.write(diff_str)

        # Vim modeline must be near the end.
        print(
            __comment_prefix,
            'vi: set textwidth=75 filetype=diff nobackup:',
            file=f,
        )

    call_editor(fname)

    with open(fname, 'r+') as f:
        __clean_comments(f)
        f.seek(0)
        result = f.read()

    os.remove(fname)

    return result


#
# Classes
#

class StgitObject(object):
    """An object with stgit-like properties stored as files in a directory
    """

    def create_empty_field(self, name):
        create_empty_file(os.path.join(self._dir, name))

    def _get_field(self, name, multiline=False):
        id_file = os.path.join(self._dir, name)
        if os.path.isfile(id_file):
            line = read_string(id_file, multiline)
            if line == '':
                return None
            else:
                return line
        else:
            return None

    def _set_field(self, name, value, multiline=False):
        fname = os.path.join(self._dir, name)
        if value and value != '':
            write_string(fname, value, multiline)
        elif os.path.isfile(fname):
            os.remove(fname)


class Patch(StgitObject):
    """Basic patch implementation
    """

    def _init_refs(self):
        self._top_ref = self._refs_base + '/' + self.name
        self._log_ref = self._top_ref + '.log'

    def __init__(self, name, series_dir, refs_base):
        self.name = name
        self._series_dir = series_dir
        self._dir = os.path.join(self._series_dir, self.name)
        self._refs_base = refs_base
        self._init_refs()

    def create(self):
        os.mkdir(self._dir)

    def delete(self, keep_log=False):
        if os.path.isdir(self._dir):
            for f in os.listdir(self._dir):
                os.remove(os.path.join(self._dir, f))
            os.rmdir(self._dir)
        else:
            out.warn('Patch directory "%s" does not exist' % self._dir)
        try:
            # the reference might not exist if the repository was corrupted
            git.delete_ref(self._top_ref)
        except git.GitException as e:
            out.warn(str(e))
        if not keep_log and git.ref_exists(self._log_ref):
            git.delete_ref(self._log_ref)

    def _update_top_ref(self, ref):
        git.set_ref(self._top_ref, ref)
        self._set_field('top', ref)
        self._set_field('bottom', git.get_commit(ref).get_parent())

    def _update_log_ref(self, ref):
        git.set_ref(self._log_ref, ref)

    def get_bottom(self):
        return git.get_commit(self.get_top()).get_parent()

    def get_top(self):
        return git.rev_parse(self._top_ref)

    def set_top(self, value, backup=False):
        if backup:
            curr_top = self.get_top()
            self._set_field('top.old', curr_top)
            self._set_field(
                'bottom.old',
                git.get_commit(curr_top).get_parent(),
            )
        self._update_top_ref(value)

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
        date = self._get_field('authdate')
        if not date:
            return date

        if re.match(r'[0-9]+\s+[+-][0-9]+', date):
            # Unix time (seconds) + time zone
            secs_tz = date.split()
            date = formatdate(int(secs_tz[0]))[:-5] + secs_tz[1]

        return date

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

    def set_log(self, value, backup=False):
        self._set_field('log', value)
        self._update_log_ref(value)


class PatchSet(StgitObject):
    def __init__(self, name=None):
        try:
            if name:
                self.name = name
            else:
                self.name = git.get_head_file()
            self._base_dir = basedir.get()
        except git.GitException as ex:
            raise StackException('GIT tree not initialised: %s' % ex)

        self._dir = os.path.join(self._base_dir, 'patches', self.name)

    def _branch_protect(self):
        return 'branch.%s.stgit.protect' % self.name

    def get_protected(self):
        return config.getbool(self._branch_protect())

    def protect(self):
        config.set(self._branch_protect(), 'true')

    def unprotect(self):
        if self.get_protected():
            config.unset(self._branch_protect())

    def _branch_descr(self):
        return 'branch.%s.description' % self.name

    def get_description(self):
        return config.get(self._branch_descr()) or ''

    def set_description(self, line):
        if line:
            config.set(self._branch_descr(), line)
        else:
            config.unset(self._branch_descr())

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
        return config.get(
            stackupgrade.format_version_key(self.name)
        ) is not None


class Series(PatchSet):
    """Class including the operations on series
    """

    def __init__(self, name=None):
        """Takes a series name as the parameter.
        """
        PatchSet.__init__(self, name)

        # Update the branch to the latest format version if it is
        # initialized, but don't touch it if it isn't.
        stackupgrade.update_to_current_format_version(
            Repository.default(), self.name
        )

        self._refs_base = 'refs/patches/%s' % self.name

        self._applied_file = os.path.join(self._dir, 'applied')
        self._unapplied_file = os.path.join(self._dir, 'unapplied')
        self._hidden_file = os.path.join(self._dir, 'hidden')

        # where this series keeps its patches
        self._patch_dir = os.path.join(self._dir, 'patches')

        # trash directory
        self._trash_dir = os.path.join(self._dir, 'trash')

    def _patch_name_valid(self, name):
        """Raise an exception if the patch name is not valid.
        """
        if not name or re.search(r'[^\w.-]', name):
            raise StackException('Invalid patch name: "%s"' % name)

    def get_patch(self, name):
        """Return a Patch object for the given name
        """
        return Patch(name, self._patch_dir, self._refs_base)

    def get_current_patch(self):
        """Return a Patch object representing the topmost patch, or
        None if there is no such patch."""
        crt = self.get_current()
        if not crt:
            return None
        return self.get_patch(crt)

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
        if not os.path.isfile(self._applied_file):
            raise StackException('Branch "%s" not initialised' % self.name)
        return read_strings(self._applied_file)

    def get_unapplied(self):
        if not os.path.isfile(self._unapplied_file):
            raise StackException('Branch "%s" not initialised' % self.name)
        return read_strings(self._unapplied_file)

    def get_hidden(self):
        if not os.path.isfile(self._hidden_file):
            return []
        return read_strings(self._hidden_file)

    def get_base(self):
        # Return the parent of the bottommost patch, if there is one.
        if os.path.isfile(self._applied_file):
            with open(self._applied_file) as f:
                bottommost = f.readline().strip()
            if bottommost:
                return self.get_patch(bottommost).get_bottom()
        # No bottommost patch, so just return HEAD
        return git.get_head()

    def _set_parent_remote(self, remote):
        config.set('branch.%s.remote' % self.name, remote)

    def _set_parent_branch(self, name):
        if config.get('branch.%s.remote' % self.name):
            # Never set merge if remote is not set to avoid
            # possibly-erroneous lookups into 'origin'
            config.set('branch.%s.merge' % self.name, name)
        config.set('branch.%s.stgit.parentbranch' % self.name, name)

    def set_parent(self, remote, localbranch):
        if localbranch:
            if remote:
                self._set_parent_remote(remote)
            self._set_parent_branch(localbranch)
        # We'll enforce this later
        # else:
        #     raise StackException(
        #         'Parent branch (%s) should be specified for %s' % (
        #             localbranch, self.name
        #         )
        #     )

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
        return (
            self.patch_applied(name)
            or self.patch_unapplied(name)
            or self.patch_hidden(name)
        )

    def init(self, create_at=False, parent_remote=None, parent_branch=None):
        """Initialises the stgit series
        """
        if self.is_initialised():
            raise StackException('%s already initialized' % self.name)
        for d in [self._dir]:
            if os.path.exists(d):
                raise StackException('%s already exists' % d)

        if create_at is not False:
            git.create_branch(self.name, create_at)

        os.makedirs(self._patch_dir)

        self.set_parent(parent_remote, parent_branch)

        self.create_empty_field('applied')
        self.create_empty_field('unapplied')

        config.set(stackupgrade.format_version_key(self.name),
                   text(stackupgrade.FORMAT_VERSION))

    def rename(self, to_name):
        """Renames a series
        """
        to_stack = Series(to_name)

        if to_stack.is_initialised():
            raise StackException('"%s" already exists' % to_stack.name)

        patches = self.get_applied() + self.get_unapplied()

        git.rename_branch(self.name, to_name)

        for patch in patches:
            git.rename_ref('refs/patches/%s/%s' % (self.name, patch),
                           'refs/patches/%s/%s' % (to_name, patch))
            git.rename_ref('refs/patches/%s/%s.log' % (self.name, patch),
                           'refs/patches/%s/%s.log' % (to_name, patch))
        if os.path.isdir(self._dir):
            rename(os.path.join(self._base_dir, 'patches'),
                   self.name, to_stack.name)

        # Rename the config section
        for k in ['branch.%s', 'branch.%s.stgit']:
            config.rename_section(k % self.name, k % to_name)

        self.__init__(to_name)

    def clone(self, target_series):
        """Clones a series
        """
        try:
            # allow cloning of branches not under StGIT control
            base = self.get_base()
        except BaseException:
            base = git.get_head()
        Series(target_series).init(create_at=base)
        new_series = Series(target_series)

        # generate an artificial description file
        new_series.set_description('clone of "%s"' % self.name)

        # clone self's entire series as unapplied patches
        try:
            # allow cloning of branches not under StGIT control
            applied = self.get_applied()
            unapplied = self.get_unapplied()
            patches = applied + unapplied
            patches.reverse()
        except BaseException:
            patches = applied = unapplied = []
        for p in patches:
            patch = self.get_patch(p)
            newpatch = new_series.new_patch(
                p,
                message=patch.get_description(),
                can_edit=False,
                unapplied=True,
                bottom=patch.get_bottom(),
                top=patch.get_top(),
                author_name=patch.get_authname(),
                author_email=patch.get_authemail(),
                author_date=patch.get_authdate(),
            )
            if patch.get_log():
                out.info('Setting log to %s' % patch.get_log())
                newpatch.set_log(patch.get_log())
            else:
                out.info('No log for %s' % p)

        # fast forward the cloned series to self's top
        new_series.forward_patches(applied)

        # Clone parent informations
        value = config.get('branch.%s.remote' % self.name)
        if value:
            config.set('branch.%s.remote' % target_series, value)

        value = config.get('branch.%s.merge' % self.name)
        if value:
            config.set('branch.%s.merge' % target_series, value)

        value = config.get('branch.%s.stgit.parentbranch' % self.name)
        if value:
            config.set('branch.%s.stgit.parentbranch' % target_series, value)

    def delete(self, force=False, cleanup=False):
        """Deletes an stgit series
        """
        if self.is_initialised():
            patches = (
                self.get_unapplied()
                + self.get_applied()
                + self.get_hidden()
            )
            if not force and patches:
                raise StackException(
                    'Cannot %s: the series still contains patches' %
                    ('delete', 'clean up')[cleanup])
            for p in patches:
                self.get_patch(p).delete()

            # remove the trash directory if any
            if os.path.exists(self._trash_dir):
                for fname in os.listdir(self._trash_dir):
                    os.remove(os.path.join(self._trash_dir, fname))
                os.rmdir(self._trash_dir)

            # FIXME: find a way to get rid of those manual removals
            # (move functionality to StgitObject ?)
            if os.path.exists(self._applied_file):
                os.remove(self._applied_file)
            if os.path.exists(self._unapplied_file):
                os.remove(self._unapplied_file)
            if os.path.exists(self._hidden_file):
                os.remove(self._hidden_file)
            if os.path.exists(self._dir + '/orig-base'):
                os.remove(self._dir + '/orig-base')

            if not os.listdir(self._patch_dir):
                os.rmdir(self._patch_dir)
            else:
                out.warn('Patch directory %s is not empty' % self._patch_dir)

            try:
                os.removedirs(self._dir)
            except OSError:
                raise StackException('Series directory %s is not empty'
                                     % self._dir)

        if not cleanup:
            try:
                git.delete_branch(self.name)
            except git.GitException:
                out.warn('Could not delete branch "%s"' % self.name)
            config.remove_section('branch.%s' % self.name)

        config.remove_section('branch.%s.stgit' % self.name)

    def new_patch(
        self,
        name,
        message=None,
        can_edit=True,
        unapplied=False,
        show_patch=False,
        top=None,
        bottom=None,
        commit=True,
        author_name=None,
        author_email=None,
        author_date=None,
        committer_name=None,
        committer_email=None,
        sign_str=None,
    ):
        """Creates a new patch, either pointing to an existing commit object,
        or by creating a new commit object.
        """

        assert commit or (top and bottom)
        assert (top and bottom) or (not top and not bottom)
        assert commit or (
            not top
            or bottom == git.get_commit(top).get_parent()
        )

        if name is not None:
            self._patch_name_valid(name)
            if self.patch_exists(name):
                raise StackException('Patch "%s" already exists' % name)

        # TODO: move this out of the stgit.stack module, it is really
        # for higher level commands to handle the user interaction
        def sign(msg):
            return add_sign_line(msg, sign_str,
                                 committer_name or git.committer().name,
                                 committer_email or git.committer().email)
        if not message and can_edit:
            descr = edit_file(
                self, sign(''),
                'Please enter the description for the patch above.',
                show_patch)
        else:
            descr = sign(message)

        head = git.get_head()

        if name is None:
            name = make_patch_name(descr, self.patch_exists)

        patch = self.get_patch(name)
        patch.create()

        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

        if unapplied:
            patches = [patch.name] + self.get_unapplied()
            write_strings(self._unapplied_file, patches)
            set_head = False
        else:
            append_string(self._applied_file, patch.name)
            set_head = True

        if commit:
            if top:
                top_commit = git.get_commit(top)
            else:
                bottom = head
                top_commit = git.get_commit(head)

            # create a commit for the patch (may be empty if top == bottom);
            # only commit on top of the current branch
            assert(unapplied or bottom == head)
            commit_id = git.commit(
                message=descr,
                parents=[bottom],
                cache_update=False,
                tree_id=top_commit.get_tree(),
                allowempty=True,
                set_head=set_head,
                author_name=author_name,
                author_email=author_email,
                author_date=author_date,
                committer_name=committer_name,
                committer_email=committer_email,
            )
            # set the patch top to the new commit
            patch.set_top(commit_id)
        else:
            patch.set_top(top)

        self.log_patch(patch, 'new')

        return patch

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

            patch = self.get_patch(name)

            head = top
            bottom = patch.get_bottom()
            top = patch.get_top()

            # top != bottom always since we have a commit for each patch
            if head == bottom:
                # reset the backup information. No logging since the
                # patch hasn't changed
                patch.set_top(top, backup=True)

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

                    top = git.commit(
                        message=descr,
                        parents=[head],
                        cache_update=False,
                        tree_id=top_tree,
                        allowempty=True,
                        author_name=author_name,
                        author_email=author_email,
                        author_date=author_date,
                        committer_name=committer_name,
                        committer_email=committer_email,
                    )

                    patch.set_top(top, backup=True)

                    self.log_patch(patch, 'push(f)')
                else:
                    top = head
                    # stop the fast-forwarding, must do a real merge
                    break

            forwarded += 1
            unapplied.remove(name)

        if forwarded == 0:
            return 0

        git.switch(top)

        append_strings(self._applied_file, names[0:forwarded])
        write_strings(self._unapplied_file, unapplied)

        return forwarded

    def log_patch(self, patch, message):
        """Generate a log commit for a patch
        """
        top = git.get_commit(patch.get_top())
        old_log = patch.get_log()

        # generate a new log entry
        msg = '%s\t%s' % (message, top.get_id_hash())
        if old_log:
            parents = [old_log]
        else:
            parents = []

        log = git.commit(
            message=msg,
            parents=parents,
            cache_update=False,
            tree_id=top.get_tree(),
            allowempty=True,
        )
        patch.set_log(log)
