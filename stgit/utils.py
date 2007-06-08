"""Common utility functions
"""

import errno, os, os.path, re, sys
from stgit.config import config

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

class MessagePrinter(object):
    def __init__(self):
        class Output(object):
            def __init__(self, write, flush):
                self.write = write
                self.flush = flush
                self.at_start_of_line = True
                self.level = 0
            def new_line(self):
                """Ensure that we're at the beginning of a line."""
                if not self.at_start_of_line:
                    self.write('\n')
                    self.at_start_of_line = True
            def single_line(self, msg, print_newline = True,
                            need_newline = True):
                """Write a single line. Newline before and after are
                separately configurable."""
                if need_newline:
                    self.new_line()
                if self.at_start_of_line:
                    self.write('  '*self.level)
                self.write(msg)
                if print_newline:
                    self.write('\n')
                    self.at_start_of_line = True
                else:
                    self.flush()
                    self.at_start_of_line = False
            def tagged_lines(self, tag, lines):
                tag += ': '
                for line in lines:
                    self.single_line(tag + line)
                    tag = ' '*len(tag)
            def write_line(self, line):
                """Write one line of text on a lines of its own, not
                indented."""
                self.new_line()
                self.write('%s\n' % line)
                self.at_start_of_line = True
            def write_raw(self, string):
                """Write an arbitrary string, possibly containing
                newlines."""
                self.new_line()
                self.write(string)
                self.at_start_of_line = string.endswith('\n')
        self.__stdout = Output(sys.stdout.write, sys.stdout.flush)
        if sys.stdout.isatty():
            self.__out = self.__stdout
        else:
            self.__out = Output(lambda msg: None, lambda: None)
    def stdout(self, line):
        """Write a line to stdout."""
        self.__stdout.write_line(line)
    def stdout_raw(self, string):
        """Write a string possibly containing newlines to stdout."""
        self.__stdout.write_raw(string)
    def info(self, *msgs):
        for msg in msgs:
            self.__out.single_line(msg)
    def note(self, *msgs):
        self.__out.tagged_lines('Notice', msgs)
    def warn(self, *msgs):
        self.__out.tagged_lines('Warning', msgs)
    def error(self, *msgs):
        self.__out.tagged_lines('Error', msgs)
    def start(self, msg):
        """Start a long-running operation."""
        self.__out.single_line('%s ... ' % msg, print_newline = False)
        self.__out.level += 1
    def done(self, extramsg = None):
        """Finish long-running operation."""
        self.__out.level -= 1
        if extramsg:
            msg = 'done (%s)' % extramsg
        else:
            msg = 'done'
        self.__out.single_line(msg, need_newline = False)

out = MessagePrinter()

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

class EditorException(Exception):
    pass

def call_editor(filename):
    """Run the editor on the specified filename."""

    # the editor
    editor = config.get('stgit.editor')
    if editor:
        pass
    elif 'EDITOR' in os.environ:
        editor = os.environ['EDITOR']
    else:
        editor = 'vi'
    editor += ' %s' % filename

    out.start('Invoking the editor: "%s"' % editor)
    err = os.system(editor)
    if err:
        raise EditorException, 'editor failed, exit code: %d' % err
    out.done()

def patch_name_from_msg(msg):
    """Return a string to be used as a patch name. This is generated
    from the top line of the string passed as argument, and is at most
    30 characters long."""
    if not msg:
        return None

    subject_line = msg.split('\n', 1)[0].lstrip().lower()
    return re.sub('[\W]+', '-', subject_line).strip('-')[:30]

def make_patch_name(msg, unacceptable, default_name = 'patch'):
    """Return a patch name generated from the given commit message,
    guaranteed to make unacceptable(name) be false. If the commit
    message is empty, base the name on default_name instead."""
    patchname = patch_name_from_msg(msg)
    if not patchname:
        patchname = default_name
    if unacceptable(patchname):
        suffix = 0
        while unacceptable('%s-%d' % (patchname, suffix)):
            suffix += 1
        patchname = '%s-%d' % (patchname, suffix)
    return patchname
