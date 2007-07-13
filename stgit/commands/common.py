"""Function/variables common to all the commands
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

import sys, os, os.path, re
from optparse import OptionParser, make_option

from stgit.utils import *
from stgit import stack, git, basedir
from stgit.config import config, file_extensions

crt_series = None


# Command exception class
class CmdException(Exception):
    pass


# Utility functions
class RevParseException(Exception):
    """Revision spec parse error."""
    pass

def parse_rev(rev):
    """Parse a revision specification into its
    patchname@branchname//patch_id parts. If no branch name has a slash
    in it, also accept / instead of //."""
    files, dirs = list_files_and_dirs(os.path.join(basedir.get(),
                                                   'refs', 'heads'))
    if len(dirs) != 0:
        # We have branch names with / in them.
        branch_chars = r'[^@]'
        patch_id_mark = r'//'
    else:
        # No / in branch names.
        branch_chars = r'[^@/]'
        patch_id_mark = r'(/|//)'
    patch_re = r'(?P<patch>[^@/]+)'
    branch_re = r'@(?P<branch>%s+)' % branch_chars
    patch_id_re = r'%s(?P<patch_id>[a-z.]*)' % patch_id_mark

    # Try //patch_id.
    m = re.match(r'^%s$' % patch_id_re, rev)
    if m:
        return None, None, m.group('patch_id')

    # Try path[@branch]//patch_id.
    m = re.match(r'^%s(%s)?%s$' % (patch_re, branch_re, patch_id_re), rev)
    if m:
        return m.group('patch'), m.group('branch'), m.group('patch_id')

    # Try patch[@branch].
    m = re.match(r'^%s(%s)?$' % (patch_re, branch_re), rev)
    if m:
        return m.group('patch'), m.group('branch'), None

    # No, we can't parse that.
    raise RevParseException

def git_id(rev):
    """Return the GIT id
    """
    if not rev:
        return None
    try:
        patch, branch, patch_id = parse_rev(rev)
        if branch == None:
            series = crt_series
        else:
            series = stack.Series(branch)
        if patch == None:
            patch = series.get_current()
            if not patch:
                raise CmdException, 'No patches applied'
        if patch in series.get_applied() or patch in series.get_unapplied() or \
               patch in series.get_hidden():
            if patch_id in ['top', '', None]:
                return series.get_patch(patch).get_top()
            elif patch_id == 'bottom':
                return series.get_patch(patch).get_bottom()
            elif patch_id == 'top.old':
                return series.get_patch(patch).get_old_top()
            elif patch_id == 'bottom.old':
                return series.get_patch(patch).get_old_bottom()
            elif patch_id == 'log':
                return series.get_patch(patch).get_log()
        if patch == 'base' and patch_id == None:
            return series.get_base()
    except RevParseException:
        pass
    return git.rev_parse(rev + '^{commit}')

def check_local_changes():
    if git.local_changes():
        raise CmdException, \
              'local changes in the tree. Use "refresh" or "status --reset"'

def check_head_top_equal():
    if not crt_series.head_top_equal():
        raise CmdException(
            'HEAD and top are not the same. You probably committed\n'
            '  changes to the tree outside of StGIT. To bring them\n'
            '  into StGIT, use the "assimilate" command')

def check_conflicts():
    if os.path.exists(os.path.join(basedir.get(), 'conflicts')):
        raise CmdException, \
              'Unsolved conflicts. Please resolve them first or\n' \
              '  revert the changes with "status --reset"'

def print_crt_patch(branch = None):
    if not branch:
        patch = crt_series.get_current()
    else:
        patch = stack.Series(branch).get_current()

    if patch:
        out.info('Now at patch "%s"' % patch)
    else:
        out.info('No patches applied')

def resolved(filename, reset = None):
    if reset:
        reset_file = filename + file_extensions()[reset]
        if os.path.isfile(reset_file):
            if os.path.isfile(filename):
                os.remove(filename)
            os.rename(reset_file, filename)

    git.update_cache([filename], force = True)

    for ext in file_extensions().values():
        fn = filename + ext
        if os.path.isfile(fn):
            os.remove(fn)

def resolved_all(reset = None):
    conflicts = git.get_conflicts()
    if conflicts:
        for filename in conflicts:
            resolved(filename, reset)
        os.remove(os.path.join(basedir.get(), 'conflicts'))

def push_patches(patches, check_merged = False):
    """Push multiple patches onto the stack. This function is shared
    between the push and pull commands
    """
    forwarded = crt_series.forward_patches(patches)
    if forwarded > 1:
        out.info('Fast-forwarded patches "%s" - "%s"'
                 % (patches[0], patches[forwarded - 1]))
    elif forwarded == 1:
        out.info('Fast-forwarded patch "%s"' % patches[0])

    names = patches[forwarded:]

    # check for patches merged upstream
    if names and check_merged:
        out.start('Checking for patches merged upstream')

        merged = crt_series.merged_patches(names)

        out.done('%d found' % len(merged))
    else:
        merged = []

    for p in names:
        out.start('Pushing patch "%s"' % p)

        if p in merged:
            crt_series.push_patch(p, empty = True)
            out.done('merged upstream')
        else:
            modified = crt_series.push_patch(p)

            if crt_series.empty_patch(p):
                out.done('empty patch')
            elif modified:
                out.done('modified')
            else:
                out.done()

def pop_patches(patches, keep = False):
    """Pop the patches in the list from the stack. It is assumed that
    the patches are listed in the stack reverse order.
    """
    if len(patches) == 0:
        out.info('Nothing to push/pop')
    else:
        p = patches[-1]
        if len(patches) == 1:
            out.start('Popping patch "%s"' % p)
        else:
            out.start('Popping patches "%s" - "%s"' % (patches[0], p))
        crt_series.pop_patch(p, keep)
        out.done()

def parse_patches(patch_args, patch_list, boundary = 0, ordered = False):
    """Parse patch_args list for patch names in patch_list and return
    a list. The names can be individual patches and/or in the
    patch1..patch2 format.
    """
    patches = []

    for name in patch_args:
        pair = name.split('..')
        for p in pair:
            if p and not p in patch_list:
                raise CmdException, 'Unknown patch name: %s' % p

        if len(pair) == 1:
            # single patch name
            pl = pair
        elif len(pair) == 2:
            # patch range [p1]..[p2]
            # inclusive boundary
            if pair[0]:
                first = patch_list.index(pair[0])
            else:
                first = -1
            # exclusive boundary
            if pair[1]:
                last = patch_list.index(pair[1]) + 1
            else:
                last = -1

            # only cross the boundary if explicitly asked
            if not boundary:
                boundary = len(patch_list)
            if first < 0:
                if last <= boundary:
                    first = 0
                else:
                    first = boundary
            if last < 0:
                if first < boundary:
                    last = boundary
                else:
                    last = len(patch_list)

            if last > first:
                pl = patch_list[first:last]
            else:
                pl = patch_list[(last - 1):(first + 1)]
                pl.reverse()
        else:
            raise CmdException, 'Malformed patch name: %s' % name

        for p in pl:
            if p in patches:
                raise CmdException, 'Duplicate patch name: %s' % p

        patches += pl

    if ordered:
        patches = [p for p in patch_list if p in patches]

    return patches

def name_email(address):
    """Return a tuple consisting of the name and email parsed from a
    standard 'name <email>' or 'email (name)' string
    """
    address = re.sub('[\\\\"]', '\\\\\g<0>', address)
    str_list = re.findall('^(.*)\s*<(.*)>\s*$', address)
    if not str_list:
        str_list = re.findall('^(.*)\s*\((.*)\)\s*$', address)
        if not str_list:
            raise CmdException, 'Incorrect "name <email>"/"email (name)" string: %s' % address
        return ( str_list[0][1], str_list[0][0] )

    return str_list[0]

def name_email_date(address):
    """Return a tuple consisting of the name, email and date parsed
    from a 'name <email> date' string
    """
    address = re.sub('[\\\\"]', '\\\\\g<0>', address)
    str_list = re.findall('^(.*)\s*<(.*)>\s*(.*)\s*$', address)
    if not str_list:
        raise CmdException, 'Incorrect "name <email> date" string: %s' % address

    return str_list[0]

def address_or_alias(addr_str):
    """Return the address if it contains an e-mail address or look up
    the aliases in the config files.
    """
    def __address_or_alias(addr):
        if not addr:
            return None
        if addr.find('@') >= 0:
            # it's an e-mail address
            return addr
        alias = config.get('mail.alias.'+addr)
        if alias:
            # it's an alias
            return alias
        raise CmdException, 'unknown e-mail alias: %s' % addr

    addr_list = [__address_or_alias(addr.strip())
                 for addr in addr_str.split(',')]
    return ', '.join([addr for addr in addr_list if addr])

def prepare_rebase(force=None):
    if not force:
        # Be sure we won't loose results of stg-(un)commit by error.
        # Do not require an existing orig-base for compatibility with 0.12 and earlier.
        origbase = crt_series._get_field('orig-base')
        if origbase and crt_series.get_base() != origbase:
            raise CmdException, 'Rebasing would possibly lose data'

    # pop all patches
    applied = crt_series.get_applied()
    if len(applied) > 0:
        out.start('Popping all applied patches')
        crt_series.pop_patch(applied[0])
        out.done()
    return applied

def rebase(target):
    if target == git.get_head():
        out.info('Already at "%s", no need for rebasing.' % target)
        return
    out.start('Rebasing to "%s"' % target)
    git.reset(tree_id = git_id(target))
    out.done()

def post_rebase(applied, nopush, merged):
    # memorize that we rebased to here
    crt_series._set_field('orig-base', git.get_head())
    # push the patches back
    if not nopush:
        push_patches(applied, merged)
