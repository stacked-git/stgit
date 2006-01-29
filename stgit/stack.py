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

import sys, os

from stgit.utils import *
from stgit import git
from stgit.config import config


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
    fname = '.stgit.msg'
    tmpl = os.path.join(git.get_base_dir(), 'patchdescr.tmpl')

    f = file(fname, 'w+')
    if line:
        print >> f, line
    elif os.path.isfile(tmpl):
        print >> f, file(tmpl).read().rstrip()
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

    # the editor
    if config.has_option('stgit', 'editor'):
        editor = config.get('stgit', 'editor')
    elif 'EDITOR' in os.environ:
        editor = os.environ['EDITOR']
    else:
        editor = 'vi'
    editor += ' %s' % fname

    print 'Invoking the editor: "%s"...' % editor,
    sys.stdout.flush()
    print 'done (exit code: %d)' % os.system(editor)

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

class Patch:
    """Basic patch implementation
    """
    def __init__(self, name, series_dir):
        self.__series_dir = series_dir
        self.__name = name
        self.__dir = os.path.join(self.__series_dir, self.__name)

    def create(self):
        os.mkdir(self.__dir)
        create_empty_file(os.path.join(self.__dir, 'bottom'))
        create_empty_file(os.path.join(self.__dir, 'top'))

    def delete(self):
        for f in os.listdir(self.__dir):
            os.remove(os.path.join(self.__dir, f))
        os.rmdir(self.__dir)

    def get_name(self):
        return self.__name

    def rename(self, newname):
        olddir = self.__dir
        self.__name = newname
        self.__dir = os.path.join(self.__series_dir, self.__name)

        os.rename(olddir, self.__dir)

    def __get_field(self, name, multiline = False):
        id_file = os.path.join(self.__dir, name)
        if os.path.isfile(id_file):
            line = read_string(id_file, multiline)
            if line == '':
                return None
            else:
                return line
        else:
            return None

    def __set_field(self, name, value, multiline = False):
        fname = os.path.join(self.__dir, name)
        if value and value != '':
            write_string(fname, value, multiline)
        elif os.path.isfile(fname):
            os.remove(fname)

    def get_old_bottom(self):
        return self.__get_field('bottom.old')

    def get_bottom(self):
        return self.__get_field('bottom')

    def set_bottom(self, value, backup = False):
        if backup:
            curr = self.__get_field('bottom')
            if curr != value:
                self.__set_field('bottom.old', curr)
            else:
                self.__set_field('bottom.old', None)
        self.__set_field('bottom', value)

    def get_old_top(self):
        return self.__get_field('top.old')

    def get_top(self):
        return self.__get_field('top')

    def set_top(self, value, backup = False):
        if backup:
            curr = self.__get_field('top')
            if curr != value:
                self.__set_field('top.old', curr)
            else:
                self.__set_field('top.old', None)
        self.__set_field('top', value)

    def restore_old_boundaries(self):
        bottom = self.__get_field('bottom.old')
        top = self.__get_field('top.old')

        if top and bottom:
            self.__set_field('bottom', bottom)
            self.__set_field('top', top)
            return True
        else:
            return False

    def get_description(self):
        return self.__get_field('description', True)

    def set_description(self, line):
        self.__set_field('description', line, True)

    def get_authname(self):
        return self.__get_field('authname')

    def set_authname(self, name):
        if not name and config.has_option('stgit', 'authname'):
            name = config.get('stgit', 'authname')
        self.__set_field('authname', name)

    def get_authemail(self):
        return self.__get_field('authemail')

    def set_authemail(self, address):
        if not address and config.has_option('stgit', 'authemail'):
            address = config.get('stgit', 'authemail')
        self.__set_field('authemail', address)

    def get_authdate(self):
        return self.__get_field('authdate')

    def set_authdate(self, authdate):
        self.__set_field('authdate', authdate)

    def get_commname(self):
        return self.__get_field('commname')

    def set_commname(self, name):
        if not name and config.has_option('stgit', 'commname'):
            name = config.get('stgit', 'commname')
        self.__set_field('commname', name)

    def get_commemail(self):
        return self.__get_field('commemail')

    def set_commemail(self, address):
        if not address and config.has_option('stgit', 'commemail'):
            address = config.get('stgit', 'commemail')
        self.__set_field('commemail', address)


class Series:
    """Class including the operations on series
    """
    def __init__(self, name = None):
        """Takes a series name as the parameter.
        """
        try:
            if name:
                self.__name = name
            else:
                self.__name = git.get_head_file()
            base_dir = git.get_base_dir()
        except git.GitException, ex:
            raise StackException, 'GIT tree not initialised: %s' % ex

        self.__series_dir = os.path.join(base_dir, 'patches',
                                         self.__name)
        self.__base_file = os.path.join(base_dir, 'refs', 'bases',
                                        self.__name)

        self.__applied_file = os.path.join(self.__series_dir, 'applied')
        self.__unapplied_file = os.path.join(self.__series_dir, 'unapplied')
        self.__current_file = os.path.join(self.__series_dir, 'current')
        self.__descr_file = os.path.join(self.__series_dir, 'description')

        # where this series keeps its patches
        self.__patch_dir = os.path.join(self.__series_dir, 'patches')
        if not os.path.isdir(self.__patch_dir):
            self.__patch_dir = self.__series_dir

    def get_branch(self):
        """Return the branch name for the Series object
        """
        return self.__name

    def __set_current(self, name):
        """Sets the topmost patch
        """
        if name:
            write_string(self.__current_file, name)
        else:
            create_empty_file(self.__current_file)

    def get_patch(self, name):
        """Return a Patch object for the given name
        """
        return Patch(name, self.__patch_dir)

    def get_current(self):
        """Return a Patch object representing the topmost patch
        """
        if os.path.isfile(self.__current_file):
            name = read_string(self.__current_file)
        else:
            return None
        if name == '':
            return None
        else:
            return name

    def get_applied(self):
        if not os.path.isfile(self.__applied_file):
            raise StackException, 'Branch "%s" not initialised' % self.__name
        f = file(self.__applied_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names

    def get_unapplied(self):
        if not os.path.isfile(self.__unapplied_file):
            raise StackException, 'Branch "%s" not initialised' % self.__name
        f = file(self.__unapplied_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names

    def get_base_file(self):
        return self.__base_file

    def get_protected(self):
        return os.path.isfile(os.path.join(self.__series_dir, 'protected'))

    def protect(self):
        protect_file = os.path.join(self.__series_dir, 'protected')
        if not os.path.isfile(protect_file):
            create_empty_file(protect_file)

    def unprotect(self):
        protect_file = os.path.join(self.__series_dir, 'protected')
        if os.path.isfile(protect_file):
            os.remove(protect_file)

    def get_description(self):
        if os.path.isfile(self.__descr_file):
            return read_string(self.__descr_file)
        else:
            return ''

    def __patch_is_current(self, patch):
        return patch.get_name() == read_string(self.__current_file)

    def __patch_applied(self, name):
        """Return true if the patch exists in the applied list
        """
        return name in self.get_applied()

    def __patch_unapplied(self, name):
        """Return true if the patch exists in the unapplied list
        """
        return name in self.get_unapplied()

    def __begin_stack_check(self):
        """Save the current HEAD into .git/refs/heads/base if the stack
        is empty
        """
        if len(self.get_applied()) == 0:
            head = git.get_head()
            write_string(self.__base_file, head)

    def __end_stack_check(self):
        """Remove .git/refs/heads/base if the stack is empty.
        This warning should never happen
        """
        if len(self.get_applied()) == 0 \
           and read_string(self.__base_file) != git.get_head():
            print 'Warning: stack empty but the HEAD and base are different'

    def head_top_equal(self):
        """Return true if the head and the top are the same
        """
        crt = self.get_current()
        if not crt:
            # we don't care, no patches applied
            return True
        return git.get_head() == Patch(crt, self.__patch_dir).get_top()

    def is_initialised(self):
        """Checks if series is already initialised
        """
        return os.path.isdir(self.__patch_dir)

    def init(self):
        """Initialises the stgit series
        """
        bases_dir = os.path.join(git.get_base_dir(), 'refs', 'bases')

        if self.is_initialised():
            raise StackException, self.__patch_dir + ' already exists'
        os.makedirs(self.__patch_dir)

        if not os.path.isdir(bases_dir):
            os.makedirs(bases_dir)

        create_empty_file(self.__applied_file)
        create_empty_file(self.__unapplied_file)
        create_empty_file(self.__descr_file)
        os.makedirs(os.path.join(self.__series_dir, 'patches'))
        self.__begin_stack_check()

    def rename(self, to_name):
        """Renames a series
        """
        to_stack = Series(to_name)

        if to_stack.is_initialised():
            raise StackException, '"%s" already exists' % to_stack.get_branch()
        if os.path.exists(to_stack.__base_file):
            os.remove(to_stack.__base_file)

        git.rename_branch(self.__name, to_name)

        if os.path.isdir(self.__series_dir):
            os.rename(self.__series_dir, to_stack.__series_dir)
        if os.path.exists(self.__base_file):
            os.rename(self.__base_file, to_stack.__base_file)

        self.__init__(to_name)

    def clone(self, target_series):
        """Clones a series
        """
        base = read_string(self.get_base_file())
        git.create_branch(target_series, tree_id = base)
        Series(target_series).init()
        new_series = Series(target_series)

        # generate an artificial description file
        write_string(new_series.__descr_file, 'clone of "%s"' % self.__name)

        # clone self's entire series as unapplied patches
        patches = self.get_applied() + self.get_unapplied()
        patches.reverse()
        for p in patches:
            patch = self.get_patch(p)
            new_series.new_patch(p, message = patch.get_description(),
                                 can_edit = False, unapplied = True,
                                 bottom = patch.get_bottom(),
                                 top = patch.get_top(),
                                 author_name = patch.get_authname(),
                                 author_email = patch.get_authemail(),
                                 author_date = patch.get_authdate())

        # fast forward the cloned series to self's top
        new_series.forward_patches(self.get_applied())

    def delete(self, force = False):
        """Deletes an stgit series
        """
        if self.is_initialised():
            patches = self.get_unapplied() + self.get_applied()
            if not force and patches:
                raise StackException, \
                      'Cannot delete: the series still contains patches'
            for p in patches:
                Patch(p, self.__patch_dir).delete()

            if os.path.exists(self.__applied_file):
                os.remove(self.__applied_file)
            if os.path.exists(self.__unapplied_file):
                os.remove(self.__unapplied_file)
            if os.path.exists(self.__current_file):
                os.remove(self.__current_file)
            if os.path.exists(self.__descr_file):
                os.remove(self.__descr_file)
            if not os.listdir(self.__patch_dir):
                os.rmdir(self.__patch_dir)
            else:
                print 'Patch directory %s is not empty.' % self.__name
            if not os.listdir(self.__series_dir):
                os.rmdir(self.__series_dir)
            else:
                print 'Series directory %s is not empty.' % self.__name

        if os.path.exists(self.__base_file):
            os.remove(self.__base_file)

    def refresh_patch(self, files = None, message = None, edit = False,
                      show_patch = False,
                      cache_update = True,
                      author_name = None, author_email = None,
                      author_date = None,
                      committer_name = None, committer_email = None):
        """Generates a new commit for the given patch
        """
        name = self.get_current()
        if not name:
            raise StackException, 'No patches applied'

        patch = Patch(name, self.__patch_dir)

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

        commit_id = git.commit(files = files,
                               message = descr, parents = [patch.get_bottom()],
                               cache_update = cache_update,
                               allowempty = True,
                               author_name = author_name,
                               author_email = author_email,
                               author_date = author_date,
                               committer_name = committer_name,
                               committer_email = committer_email)

        patch.set_top(commit_id)
        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

        return commit_id

    def new_patch(self, name, message = None, can_edit = True,
                  unapplied = False, show_patch = False,
                  top = None, bottom = None,
                  author_name = None, author_email = None, author_date = None,
                  committer_name = None, committer_email = None):
        """Creates a new patch
        """
        if self.__patch_applied(name) or self.__patch_unapplied(name):
            raise StackException, 'Patch "%s" already exists' % name

        if not message and can_edit:
            descr = edit_file(self, None, \
                              'Please enter the description for patch "%s" ' \
                              'above.' % name, show_patch)
        else:
            descr = message

        head = git.get_head()

        self.__begin_stack_check()

        patch = Patch(name, self.__patch_dir)
        patch.create()

        if bottom:
            patch.set_bottom(bottom)
        else:
            patch.set_bottom(head)
        if top:
            patch.set_top(top)
        else:
            patch.set_top(head)

        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

        if unapplied:
            patches = [patch.get_name()] + self.get_unapplied()

            f = file(self.__unapplied_file, 'w+')
            f.writelines([line + '\n' for line in patches])
            f.close()
        else:
            append_string(self.__applied_file, patch.get_name())
            self.__set_current(name)

    def delete_patch(self, name):
        """Deletes a patch
        """
        patch = Patch(name, self.__patch_dir)

        if self.__patch_is_current(patch):
            self.pop_patch(name)
        elif self.__patch_applied(name):
            raise StackException, 'Cannot remove an applied patch, "%s", ' \
                  'which is not current' % name
        elif not name in self.get_unapplied():
            raise StackException, 'Unknown patch "%s"' % name

        patch.delete()

        unapplied = self.get_unapplied()
        unapplied.remove(name)
        f = file(self.__unapplied_file, 'w+')
        f.writelines([line + '\n' for line in unapplied])
        f.close()

    def forward_patches(self, names):
        """Try to fast-forward an array of patches.

        On return, patches in names[0:returned_value] have been pushed on the
        stack. Apply the rest with push_patch
        """
        unapplied = self.get_unapplied()
        self.__begin_stack_check()

        forwarded = 0
        top = git.get_head()

        for name in names:
            assert(name in unapplied)

            patch = Patch(name, self.__patch_dir)

            head = top
            bottom = patch.get_bottom()
            top = patch.get_top()

            # top != bottom always since we have a commit for each patch
            if head == bottom:
                # reset the backup information
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

        f = file(self.__unapplied_file, 'w+')
        f.writelines([line + '\n' for line in unapplied])
        f.close()

        self.__set_current(name)

        return forwarded

    def push_patch(self, name):
        """Pushes a patch on the stack
        """
        unapplied = self.get_unapplied()
        assert(name in unapplied)

        self.__begin_stack_check()

        patch = Patch(name, self.__patch_dir)

        head = git.get_head()
        bottom = patch.get_bottom()
        top = patch.get_top()

        ex = None
        modified = False

        # top != bottom always since we have a commit for each patch
        if head == bottom:
            # reset the backup information
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
                    git.merge(bottom, head, top)
                except git.GitException, ex:
                    print >> sys.stderr, \
                          'The merge failed during "push". ' \
                          'Use "refresh" after fixing the conflicts'

        append_string(self.__applied_file, name)

        unapplied.remove(name)
        f = file(self.__unapplied_file, 'w+')
        f.writelines([line + '\n' for line in unapplied])
        f.close()

        self.__set_current(name)

        # head == bottom case doesn't need to refresh the patch
        if head != bottom:
            if not ex:
                # if the merge was OK and no conflicts, just refresh the patch
                # The GIT cache was already updated by the merge operation
                self.refresh_patch(cache_update = False)
            else:
                raise StackException, str(ex)

        return modified

    def undo_push(self):
        name = self.get_current()
        assert(name)

        patch = Patch(name, self.__patch_dir)
        git.reset()
        self.pop_patch(name)
        return patch.restore_old_boundaries()

    def pop_patch(self, name):
        """Pops the top patch from the stack
        """
        applied = self.get_applied()
        applied.reverse()
        assert(name in applied)

        patch = Patch(name, self.__patch_dir)

        git.switch(patch.get_bottom())

        # save the new applied list
        idx = applied.index(name) + 1

        popped = applied[:idx]
        popped.reverse()
        unapplied = popped + self.get_unapplied()

        f = file(self.__unapplied_file, 'w+')
        f.writelines([line + '\n' for line in unapplied])
        f.close()

        del applied[:idx]
        applied.reverse()

        f = file(self.__applied_file, 'w+')
        f.writelines([line + '\n' for line in applied])
        f.close()

        if applied == []:
            self.__set_current(None)
        else:
            self.__set_current(applied[-1])

        self.__end_stack_check()

    def empty_patch(self, name):
        """Returns True if the patch is empty
        """
        patch = Patch(name, self.__patch_dir)
        bottom = patch.get_bottom()
        top = patch.get_top()

        if bottom == top:
            return True
        elif git.get_commit(top).get_tree() \
                 == git.get_commit(bottom).get_tree():
            return True

        return False

    def rename_patch(self, oldname, newname):
        applied = self.get_applied()
        unapplied = self.get_unapplied()

        if oldname == newname:
            raise StackException, '"To" name and "from" name are the same'

        if newname in applied or newname in unapplied:
            raise StackException, 'Patch "%s" already exists' % newname

        if oldname in unapplied:
            Patch(oldname, self.__patch_dir).rename(newname)
            unapplied[unapplied.index(oldname)] = newname

            f = file(self.__unapplied_file, 'w+')
            f.writelines([line + '\n' for line in unapplied])
            f.close()
        elif oldname in applied:
            Patch(oldname, self.__patch_dir).rename(newname)
            if oldname == self.get_current():
                self.__set_current(newname)

            applied[applied.index(oldname)] = newname

            f = file(self.__applied_file, 'w+')
            f.writelines([line + '\n' for line in applied])
            f.close()
        else:
            raise StackException, 'Unknown patch "%s"' % oldname
