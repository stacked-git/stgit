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

def edit_file(series, string, comment, show_patch = True):
    fname = '.stgit.msg'
    tmpl = os.path.join(git.base_dir, 'patchdescr.tmpl')

    f = file(fname, 'w+')
    if string:
        print >> f, string
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
    print >> f, __comment_prefix, 'vi: set textwidth=75 filetype=diff:'
    f.close()

    # the editor
    if 'EDITOR' in os.environ:
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
    string = f.read()

    f.close()
    os.remove(fname)

    return string

#
# Classes
#

class Patch:
    """Basic patch implementation
    """
    def __init__(self, name, patch_dir):
        self.__patch_dir = patch_dir
        self.__name = name
        self.__dir = os.path.join(self.__patch_dir, self.__name)

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
        self.__dir = os.path.join(self.__patch_dir, self.__name)

        os.rename(olddir, self.__dir)

    def __get_field(self, name, multiline = False):
        id_file = os.path.join(self.__dir, name)
        if os.path.isfile(id_file):
            string = read_string(id_file, multiline)
            if string == '':
                return None
            else:
                return string
        else:
            return None

    def __set_field(self, name, string, multiline = False):
        fname = os.path.join(self.__dir, name)
        if string and string != '':
            write_string(fname, string, multiline)
        elif os.path.isfile(fname):
            os.remove(fname)

    def get_bottom(self):
        return self.__get_field('bottom')

    def set_bottom(self, string, backup = False):
        if backup:
            self.__set_field('bottom.old', self.__get_field('bottom'))
        self.__set_field('bottom', string)

    def get_top(self):
        return self.__get_field('top')

    def set_top(self, string, backup = False):
        if backup:
            self.__set_field('top.old', self.__get_field('top'))
        self.__set_field('top', string)

    def restore_old_boundaries(self):
        bottom = self.__get_field('bottom.old')
        top = self.__get_field('top.old')

        if top and bottom:
            self.__set_field('bottom', bottom)
            self.__set_field('top', top)
        else:
            raise StackException, 'No patch undo information'

    def get_description(self):
        return self.__get_field('description', True)

    def set_description(self, string):
        self.__set_field('description', string, True)

    def get_authname(self):
        return self.__get_field('authname')

    def set_authname(self, string):
        if not string and config.has_option('stgit', 'authname'):
            string = config.get('stgit', 'authname')
        self.__set_field('authname', string)

    def get_authemail(self):
        return self.__get_field('authemail')

    def set_authemail(self, string):
        if not string and config.has_option('stgit', 'authemail'):
            string = config.get('stgit', 'authemail')
        self.__set_field('authemail', string)

    def get_authdate(self):
        return self.__get_field('authdate')

    def set_authdate(self, string):
        self.__set_field('authdate', string)

    def get_commname(self):
        return self.__get_field('commname')

    def set_commname(self, string):
        if not string and config.has_option('stgit', 'commname'):
            string = config.get('stgit', 'commname')
        self.__set_field('commname', string)

    def get_commemail(self):
        return self.__get_field('commemail')

    def set_commemail(self, string):
        if not string and config.has_option('stgit', 'commemail'):
            string = config.get('stgit', 'commemail')
        self.__set_field('commemail', string)


class Series:
    """Class including the operations on series
    """
    def __init__(self, name = None):
        """Takes a series name as the parameter. A valid .git/patches/name
        directory should exist
        """
        if name:
            self.__name = name
        else:
            self.__name = git.get_head_file()

        if self.__name:
            self.__patch_dir = os.path.join(git.base_dir, 'patches',
                                            self.__name)
            self.__base_file = os.path.join(git.base_dir, 'refs', 'bases',
                                            self.__name)
            self.__applied_file = os.path.join(self.__patch_dir, 'applied')
            self.__unapplied_file = os.path.join(self.__patch_dir, 'unapplied')
            self.__current_file = os.path.join(self.__patch_dir, 'current')

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
        f = file(self.__applied_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names

    def get_unapplied(self):
        f = file(self.__unapplied_file)
        names = [line.strip() for line in f.readlines()]
        f.close()
        return names

    def get_base_file(self):
        return self.__base_file

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

    def init(self):
        """Initialises the stgit series
        """
        bases_dir = os.path.join(git.base_dir, 'refs', 'bases')

        if os.path.isdir(self.__patch_dir):
            raise StackException, self.__patch_dir + ' already exists'
        os.makedirs(self.__patch_dir)

        if not os.path.isdir(bases_dir):
            os.makedirs(bases_dir)

        create_empty_file(self.__applied_file)
        create_empty_file(self.__unapplied_file)
        self.__begin_stack_check()

    def refresh_patch(self, message = None, edit = False, show_patch = False,
                      cache_update = True,
                      author_name = None, author_email = None,
                      author_date = None,
                      committer_name = None, committer_email = None,
                      commit_only = False):
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

        commit_id = git.commit(message = descr, parents = [patch.get_bottom()],
                               cache_update = cache_update,
                               allowempty = True,
                               author_name = author_name,
                               author_email = author_email,
                               author_date = author_date,
                               committer_name = committer_name,
                               committer_email = committer_email)

        if not commit_only:
            patch.set_top(commit_id)
            patch.set_description(descr)
            patch.set_authname(author_name)
            patch.set_authemail(author_email)
            patch.set_authdate(author_date)
            patch.set_commname(committer_name)
            patch.set_commemail(committer_email)

        return commit_id

    def new_patch(self, name, message = None, can_edit = True, show_patch = False,
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
        patch.set_bottom(head)
        patch.set_top(head)
        patch.set_description(descr)
        patch.set_authname(author_name)
        patch.set_authemail(author_email)
        patch.set_authdate(author_date)
        patch.set_commname(committer_name)
        patch.set_commemail(committer_email)

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
                patch.set_bottom(bottom, backup = True)
                patch.set_top(top, backup = True)

            else:
                top = head
                # stop the fast-forwarding, must do a real merge
                break

            forwarded+=1
            unapplied.remove(name)

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
            # merge/refresh can fail but the patch needs to be pushed
            try:
                git.merge(bottom, head, top)
            except git.GitException, ex:
                print >> sys.stderr, \
                      'The merge failed during "push". ' \
                      'Use "refresh" after fixing the conflicts'
                pass

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

    def undo_push(self):
        name = self.get_current()
        assert(name)

        patch = Patch(name, self.__patch_dir)
        git.reset()
        self.pop_patch(name)
        patch.restore_old_boundaries()

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
