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

from stgit.exception import *
from stgit.utils import *
from stgit.out import *
from stgit.run import *
from stgit import stack, git, basedir
from stgit.config import config, file_extensions
from stgit.lib import stack as libstack

# Command exception class
class CmdException(StgException):
    pass

# Utility functions
class RevParseException(StgException):
    """Revision spec parse error."""
    pass

def parse_rev(rev):
    """Parse a revision specification into its
    patchname@branchname//patch_id parts. If no branch name has a slash
    in it, also accept / instead of //."""
    if '/' in ''.join(git.get_heads()):
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

def git_id(crt_series, rev):
    """Return the GIT id
    """
    if not rev:
        return None

    # try a GIT revision first
    try:
        return git.rev_parse(rev + '^{commit}')
    except git.GitException:
        pass

    # try an StGIT patch name
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
    except stack.StackException:
        pass

    raise CmdException, 'Unknown patch or revision: %s' % rev

def check_local_changes():
    if git.local_changes():
        raise CmdException, \
              'local changes in the tree. Use "refresh" or "status --reset"'

def check_head_top_equal(crt_series):
    if not crt_series.head_top_equal():
        raise CmdException(
"""HEAD and top are not the same. This can happen if you
   modify a branch with git. "stg repair --help" explains
   more about what to do next.""")

def check_conflicts():
    if git.get_conflicts():
        raise CmdException, \
              'Unsolved conflicts. Please resolve them first or\n' \
              '  revert the changes with "status --reset"'

def print_crt_patch(crt_series, branch = None):
    if not branch:
        patch = crt_series.get_current()
    else:
        patch = stack.Series(branch).get_current()

    if patch:
        out.info('Now at patch "%s"' % patch)
    else:
        out.info('No patches applied')

def resolved_all(reset = None):
    conflicts = git.get_conflicts()
    git.resolved(conflicts, reset)

def push_patches(crt_series, patches, check_merged = False):
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
            crt_series.push_empty_patch(p)
            out.done('merged upstream')
        else:
            modified = crt_series.push_patch(p)

            if crt_series.empty_patch(p):
                out.done('empty patch')
            elif modified:
                out.done('modified')
            else:
                out.done()

def pop_patches(crt_series, patches, keep = False):
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
    p = parse_name_email(address)
    if p:
        return p
    else:
        raise CmdException('Incorrect "name <email>"/"email (name)" string: %s'
                           % address)

def name_email_date(address):
    p = parse_name_email_date(address)
    if p:
        return p
    else:
        raise CmdException('Incorrect "name <email> date" string: %s' % address)

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

def prepare_rebase(crt_series):
    # pop all patches
    applied = crt_series.get_applied()
    if len(applied) > 0:
        out.start('Popping all applied patches')
        crt_series.pop_patch(applied[0])
        out.done()
    return applied

def rebase(crt_series, target):
    try:
        tree_id = git_id(crt_series, target)
    except:
        # it might be that we use a custom rebase command with its own
        # target type
        tree_id = target
    if tree_id == git.get_head():
        out.info('Already at "%s", no need for rebasing.' % target)
        return
    if target:
        out.start('Rebasing to "%s"' % target)
    else:
        out.start('Rebasing to the default target')
    git.rebase(tree_id = tree_id)
    out.done()

def post_rebase(crt_series, applied, nopush, merged):
    # memorize that we rebased to here
    crt_series._set_field('orig-base', git.get_head())
    # push the patches back
    if not nopush:
        push_patches(crt_series, applied, merged)

#
# Patch description/e-mail/diff parsing
#
def __end_descr(line):
    return re.match('---\s*$', line) or re.match('diff -', line) or \
            re.match('Index: ', line)

def __split_descr_diff(string):
    """Return the description and the diff from the given string
    """
    descr = diff = ''
    top = True

    for line in string.split('\n'):
        if top:
            if not __end_descr(line):
                descr += line + '\n'
                continue
            else:
                top = False
        diff += line + '\n'

    return (descr.rstrip(), diff)

def __parse_description(descr):
    """Parse the patch description and return the new description and
    author information (if any).
    """
    subject = body = ''
    authname = authemail = authdate = None

    descr_lines = [line.rstrip() for line in  descr.split('\n')]
    if not descr_lines:
        raise CmdException, "Empty patch description"

    lasthdr = 0
    end = len(descr_lines)

    # Parse the patch header
    for pos in range(0, end):
        if not descr_lines[pos]:
           continue
        # check for a "From|Author:" line
        if re.match('\s*(?:from|author):\s+', descr_lines[pos], re.I):
            auth = re.findall('^.*?:\s+(.*)$', descr_lines[pos])[0]
            authname, authemail = name_email(auth)
            lasthdr = pos + 1
            continue
        # check for a "Date:" line
        if re.match('\s*date:\s+', descr_lines[pos], re.I):
            authdate = re.findall('^.*?:\s+(.*)$', descr_lines[pos])[0]
            lasthdr = pos + 1
            continue
        if subject:
            break
        # get the subject
        subject = descr_lines[pos]
        lasthdr = pos + 1

    # get the body
    if lasthdr < end:
        body = reduce(lambda x, y: x + '\n' + y, descr_lines[lasthdr:], '')

    return (subject + body, authname, authemail, authdate)

def parse_mail(msg):
    """Parse the message object and return (description, authname,
    authemail, authdate, diff)
    """
    from email.Header import decode_header, make_header

    def __decode_header(header):
        """Decode a qp-encoded e-mail header as per rfc2047"""
        try:
            words_enc = decode_header(header)
            hobj = make_header(words_enc)
        except Exception, ex:
            raise CmdException, 'header decoding error: %s' % str(ex)
        return unicode(hobj).encode('utf-8')

    # parse the headers
    if msg.has_key('from'):
        authname, authemail = name_email(__decode_header(msg['from']))
    else:
        authname = authemail = None

    # '\n\t' can be found on multi-line headers
    descr = __decode_header(msg['subject']).replace('\n\t', ' ')
    authdate = msg['date']

    # remove the '[*PATCH*]' expression in the subject
    if descr:
        descr = re.findall('^(\[.*?[Pp][Aa][Tt][Cc][Hh].*?\])?\s*(.*)$',
                           descr)[0][1]
    else:
        raise CmdException, 'Subject: line not found'

    # the rest of the message
    msg_text = ''
    for part in msg.walk():
        if part.get_content_type() == 'text/plain':
            msg_text += part.get_payload(decode = True)

    rem_descr, diff = __split_descr_diff(msg_text)
    if rem_descr:
        descr += '\n\n' + rem_descr

    # parse the description for author information
    descr, descr_authname, descr_authemail, descr_authdate = \
           __parse_description(descr)
    if descr_authname:
        authname = descr_authname
    if descr_authemail:
        authemail = descr_authemail
    if descr_authdate:
       authdate = descr_authdate

    return (descr, authname, authemail, authdate, diff)

def parse_patch(text):
    """Parse the input text and return (description, authname,
    authemail, authdate, diff)
    """
    descr, diff = __split_descr_diff(text)
    descr, authname, authemail, authdate = __parse_description(descr)

    # we don't yet have an agreed place for the creation date.
    # Just return None
    return (descr, authname, authemail, authdate, diff)

def readonly_constant_property(f):
    """Decorator that converts a function that computes a value to an
    attribute that returns the value. The value is computed only once,
    the first time it is accessed."""
    def new_f(self):
        n = '__' + f.__name__
        if not hasattr(self, n):
            setattr(self, n, f(self))
        return getattr(self, n)
    return property(new_f)

class DirectoryException(StgException):
    pass

class _Directory(object):
    def __init__(self, needs_current_series = True):
        self.needs_current_series =  needs_current_series
    @readonly_constant_property
    def git_dir(self):
        try:
            return Run('git', 'rev-parse', '--git-dir'
                       ).discard_stderr().output_one_line()
        except RunException:
            raise DirectoryException('No git repository found')
    @readonly_constant_property
    def __topdir_path(self):
        try:
            lines = Run('git', 'rev-parse', '--show-cdup'
                        ).discard_stderr().output_lines()
            if len(lines) == 0:
                return '.'
            elif len(lines) == 1:
                return lines[0]
            else:
                raise RunException('Too much output')
        except RunException:
            raise DirectoryException('No git repository found')
    @readonly_constant_property
    def is_inside_git_dir(self):
        return { 'true': True, 'false': False
                 }[Run('git', 'rev-parse', '--is-inside-git-dir'
                       ).output_one_line()]
    @readonly_constant_property
    def is_inside_worktree(self):
        return { 'true': True, 'false': False
                 }[Run('git', 'rev-parse', '--is-inside-work-tree'
                       ).output_one_line()]
    def cd_to_topdir(self):
        os.chdir(self.__topdir_path)

class DirectoryAnywhere(_Directory):
    def setup(self):
        pass

class DirectoryHasRepository(_Directory):
    def setup(self):
        self.git_dir # might throw an exception

class DirectoryInWorktree(DirectoryHasRepository):
    def setup(self):
        DirectoryHasRepository.setup(self)
        if not self.is_inside_worktree:
            raise DirectoryException('Not inside a git worktree')

class DirectoryGotoToplevel(DirectoryInWorktree):
    def setup(self):
        DirectoryInWorktree.setup(self)
        self.cd_to_topdir()

class DirectoryHasRepositoryLib(_Directory):
    """For commands that use the new infrastructure in stgit.lib.*."""
    def __init__(self):
        self.needs_current_series = False
    def setup(self):
        # This will throw an exception if we don't have a repository.
        self.repository = libstack.Repository.default()
