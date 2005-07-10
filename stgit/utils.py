"""Common utility functions
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

def read_string(filename, multiline = False):
    """Reads the first line from a file
    """
    f = file(filename, 'r')
    if multiline:
        string = f.read()
    else:
        string = f.readline().strip()
    f.close()
    return string

def write_string(filename, string, multiline = False):
    """Writes string to file and truncates it
    """
    f = file(filename, 'w+')
    if multiline:
        f.write(string)
    else:
        print >> f, string
    f.close()

def append_string(filename, string):
    """Appends string to file
    """
    f = file(filename, 'a+')
    print >> f, string
    f.close()

def insert_string(filename, string):
    """Inserts a string at the beginning of the file
    """
    f = file(filename, 'r+')
    lines = f.readlines()
    f.seek(0); f.truncate()
    print >> f, string
    f.writelines(lines)
    f.close()

def create_empty_file(name):
    """Creates an empty file
    """
    file(name, 'w+').close()
