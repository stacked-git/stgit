"""Common utility functions
"""

import errno, os, os.path, re, sys
from stgit.exception import *
from stgit.config import config
from stgit.out import *

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

def mkdir_file(filename, mode):
    """Opens filename with the given mode, creating the directory it's
    in if it doesn't already exist."""
    create_dirs(os.path.dirname(filename))
    return file(filename, mode)

def read_strings(filename):
    """Reads the lines from a file
    """
    f = file(filename, 'r')
    lines = [line.strip() for line in f.readlines()]
    f.close()
    return lines

def read_string(filename, multiline = False):
    """Reads the first line from a file
    """
    f = file(filename, 'r')
    if multiline:
        result = f.read()
    else:
        result = f.readline().strip()
    f.close()
    return result

def write_strings(filename, lines):
    """Write 'lines' sequence to file
    """
    f = file(filename, 'w+')
    f.writelines([line + '\n' for line in lines])
    f.close()

def write_string(filename, line, multiline = False):
    """Writes 'line' to file and truncates it
    """
    f = mkdir_file(filename, 'w+')
    if multiline:
        f.write(line)
    else:
        print >> f, line
    f.close()

def append_strings(filename, lines):
    """Appends 'lines' sequence to file
    """
    f = mkdir_file(filename, 'a+')
    for line in lines:
        print >> f, line
    f.close()

def append_string(filename, line):
    """Appends 'line' to file
    """
    f = mkdir_file(filename, 'a+')
    print >> f, line
    f.close()

def insert_string(filename, line):
    """Inserts 'line' at the beginning of the file
    """
    f = mkdir_file(filename, 'r+')
    lines = f.readlines()
    f.seek(0); f.truncate()
    print >> f, line
    f.writelines(lines)
    f.close()

def create_empty_file(name):
    """Creates an empty file
    """
    mkdir_file(name, 'w+').close()

def list_files_and_dirs(path):
    """Return the sets of filenames and directory names in a
    directory."""
    files, dirs = [], []
    for fd in os.listdir(path):
        full_fd = os.path.join(path, fd)
        if os.path.isfile(full_fd):
            files.append(fd)
        elif os.path.isdir(full_fd):
            dirs.append(fd)
    return files, dirs

def walk_tree(basedir):
    """Starting in the given directory, iterate through all its
    subdirectories. For each subdirectory, yield the name of the
    subdirectory (relative to the base directory), the list of
    filenames in the subdirectory, and the list of directory names in
    the subdirectory."""
    subdirs = ['']
    while subdirs:
        subdir = subdirs.pop()
        files, dirs = list_files_and_dirs(os.path.join(basedir, subdir))
        for d in dirs:
            subdirs.append(os.path.join(subdir, d))
        yield subdir, files, dirs

def strip_prefix(prefix, string):
    """Return string, without the prefix. Blow up if string doesn't
    start with prefix."""
    assert string.startswith(prefix)
    return string[len(prefix):]

def strip_suffix(suffix, string):
    """Return string, without the suffix. Blow up if string doesn't
    end with suffix."""
    assert string.endswith(suffix)
    return string[:-len(suffix)]

def remove_file_and_dirs(basedir, file):
    """Remove join(basedir, file), and then remove the directory it
    was in if empty, and try the same with its parent, until we find a
    nonempty directory or reach basedir."""
    os.remove(os.path.join(basedir, file))
    try:
        os.removedirs(os.path.join(basedir, os.path.dirname(file)))
    except OSError:
        # file's parent dir may not be empty after removal
        pass

def create_dirs(directory):
    """Create the given directory, if the path doesn't already exist."""
    if directory and not os.path.isdir(directory):
        create_dirs(os.path.dirname(directory))
        try:
            os.mkdir(directory)
        except OSError, e:
            if e.errno != errno.EEXIST:
                raise e

def rename(basedir, file1, file2):
    """Rename join(basedir, file1) to join(basedir, file2), not
    leaving any empty directories behind and creating any directories
    necessary."""
    full_file2 = os.path.join(basedir, file2)
    create_dirs(os.path.dirname(full_file2))
    os.rename(os.path.join(basedir, file1), full_file2)
    try:
        os.removedirs(os.path.join(basedir, os.path.dirname(file1)))
    except OSError:
        # file1's parent dir may not be empty after move
        pass

class EditorException(StgException):
    pass

def get_editor():
    for editor in [os.environ.get('GIT_EDITOR'),
                   config.get('stgit.editor'), # legacy
                   config.get('core.editor'),
                   os.environ.get('VISUAL'),
                   os.environ.get('EDITOR'),
                   'vi']:
        if editor:
            return editor

def call_editor(filename):
    """Run the editor on the specified filename."""
    cmd = '%s %s' % (get_editor(), filename)
    out.start('Invoking the editor: "%s"' % cmd)
    err = os.system(cmd)
    if err:
        raise EditorException, 'editor failed, exit code: %d' % err
    out.done()

def edit_string(s, filename):
    f = file(filename, 'w')
    f.write(s)
    f.close()
    call_editor(filename)
    f = file(filename)
    s = f.read()
    f.close()
    os.remove(filename)
    return s

def append_comment(s, comment, separator = '---'):
    return ('%s\n\n%s\nEverything following the line with "%s" will be'
            ' ignored\n\n%s' % (s, separator, separator, comment))

def strip_comment(s, separator = '---'):
    try:
        return s[:s.index('\n%s\n' % separator)]
    except ValueError:
        return s

def find_patch_name(patchname, unacceptable):
    """Find a patch name which is acceptable."""
    if unacceptable(patchname):
        suffix = 0
        while unacceptable('%s-%d' % (patchname, suffix)):
            suffix += 1
        patchname = '%s-%d' % (patchname, suffix)
    return patchname

def patch_name_from_msg(msg):
    """Return a string to be used as a patch name. This is generated
    from the top line of the string passed as argument."""
    if not msg:
        return None

    name_len = config.getint('stgit.namelength')
    if not name_len:
        name_len = 30

    subject_line = msg.split('\n', 1)[0].lstrip().lower()
    words = re.sub('[\W]+', ' ', subject_line).split()

    # use loop to avoid truncating the last name
    name = words and words[0] or 'unknown'
    for word in words[1:]:
        new = name + '-' + word
        if len(new) > name_len:
            break
        name = new

    return name

def make_patch_name(msg, unacceptable, default_name = 'patch'):
    """Return a patch name generated from the given commit message,
    guaranteed to make unacceptable(name) be false. If the commit
    message is empty, base the name on default_name instead."""
    patchname = patch_name_from_msg(msg)
    if not patchname:
        patchname = default_name
    return find_patch_name(patchname, unacceptable)

# any and all functions are builtin in Python 2.5 and higher, but not
# in 2.4.
if not 'any' in dir(__builtins__):
    def any(bools):
        for b in bools:
            if b:
                return True
        return False
if not 'all' in dir(__builtins__):
    def all(bools):
        for b in bools:
            if not b:
                return False
        return True

def add_sign_line(desc, sign_str, name, email):
    if not sign_str:
        return desc
    sign_str = '%s: %s <%s>' % (sign_str, name, email)
    if sign_str in desc:
        return desc
    desc = desc.rstrip()
    if not any(s in desc for s in ['\nSigned-off-by:', '\nAcked-by:']):
        desc = desc + '\n'
    return '%s\n%s\n' % (desc, sign_str)

def parse_name_email(address):
    """Return a tuple consisting of the name and email parsed from a
    standard 'name <email>' or 'email (name)' string."""
    address = re.sub(r'[\\"]', r'\\\g<0>', address)
    str_list = re.findall(r'^(.*)\s*<(.*)>\s*$', address)
    if not str_list:
        str_list = re.findall(r'^(.*)\s*\((.*)\)\s*$', address)
        if not str_list:
            return None
        return (str_list[0][1], str_list[0][0])
    return str_list[0]

def parse_name_email_date(address):
    """Return a tuple consisting of the name, email and date parsed
    from a 'name <email> date' string."""
    address = re.sub(r'[\\"]', r'\\\g<0>', address)
    str_list = re.findall('^(.*)\s*<(.*)>\s*(.*)\s*$', address)
    if not str_list:
        return None
    return str_list[0]

# Exit codes.
STGIT_SUCCESS = 0        # everything's OK
STGIT_GENERAL_ERROR = 1  # seems to be non-command-specific error
STGIT_COMMAND_ERROR = 2  # seems to be a command that failed
STGIT_CONFLICT = 3       # merge conflict, otherwise OK
STGIT_BUG_ERROR = 4      # a bug in StGit

def add_dict(d1, d2):
    """Return a new dict with the contents of both d1 and d2. In case of
    conflicting mappings, d2 takes precedence."""
    d = dict(d1)
    d.update(d2)
    return d
