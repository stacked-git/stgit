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
        result = f.read()
    else:
        result = f.readline().strip()
    f.close()
    return result

def write_string(filename, line, multiline = False):
    """Writes 'line' to file and truncates it
    """
    f = file(filename, 'w+')
    if multiline:
        f.write(line)
    else:
        print >> f, line
    f.close()

def append_strings(filename, lines):
    """Appends 'lines' sequence to file
    """
    f = file(filename, 'a+')
    for line in lines:
        print >> f, line
    f.close()

def append_string(filename, line):
    """Appends 'line' to file
    """
    f = file(filename, 'a+')
    print >> f, line
    f.close()

def insert_string(filename, line):
    """Inserts 'line' at the beginning of the file
    """
    f = file(filename, 'r+')
    lines = f.readlines()
    f.seek(0); f.truncate()
    print >> f, line
    f.writelines(lines)
    f.close()

def create_empty_file(name):
    """Creates an empty file
    """
    file(name, 'w+').close()
