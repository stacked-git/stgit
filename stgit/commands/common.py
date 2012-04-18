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

import sys, os, os.path, re, email.Utils
from stgit.exception import *
from stgit.utils import *
from stgit.out import *
from stgit.run import *
from stgit import stack, git, basedir
from stgit.config import config, file_extensions
from stgit.lib import stack as libstack
from stgit.lib import git as libgit
from stgit.lib import log

# Command exception class
class CmdException(StgException):
    pass

# Utility functions
def parse_rev(rev):
    """Parse a revision specification into its branch:patch parts.
    """
    try:
        branch, patch = rev.split(':', 1)
    except ValueError:
        branch = None
        patch = rev

    return (branch, patch)

def git_id(crt_series, rev):
    """Return the GIT id
    """
    # TODO: remove this function once all the occurrences were converted
    # to git_commit()
    repository = libstack.Repository.default()
    return git_commit(rev, repository, crt_series.get_name()).sha1

def get_public_ref(branch_name):
    """Return the public ref of the branch."""
    public_ref = config.get('branch.%s.public' % branch_name)
    if not public_ref:
        public_ref = 'refs/heads/%s.public' % branch_name
    return public_ref

def git_commit(name, repository, branch_name = None):
    """Return the a Commit object if 'name' is a patch name or Git commit.
    The patch names allowed are in the form '<branch>:<patch>' and can
    be followed by standard symbols used by git rev-parse. If <patch>
    is '{base}', it represents the bottom of the stack. If <patch> is
    {public}, it represents the public branch corresponding to the stack as
    described in the 'publish' command.
    """
    # Try a [branch:]patch name first
    branch, patch = parse_rev(name)
    if not branch:
        branch = branch_name or repository.current_branch_name

    # The stack base
    if patch.startswith('{base}'):
        base_id = repository.get_stack(branch).base.sha1
        return repository.rev_parse(base_id +
                                    strip_prefix('{base}', patch))
    elif patch.startswith('{public}'):
        public_ref = get_public_ref(branch)
        return repository.rev_parse(public_ref +
                                    strip_prefix('{public}', patch),
                                    discard_stderr = True)

    # Other combination of branch and patch
    try:
        return repository.rev_parse('patches/%s/%s' % (branch, patch),
                                    discard_stderr = True)
    except libgit.RepositoryException:
        pass

    # Try a Git commit
    try:
        return repository.rev_parse(name, discard_stderr = True)
    except libgit.RepositoryException:
        raise CmdException('%s: Unknown patch or revision name' % name)

def color_diff_flags():
    """Return the git flags for coloured diff output if the configuration and
    stdout allows."""
    stdout_is_tty = (sys.stdout.isatty() and 'true') or 'false'
    if config.get_colorbool('color.diff', stdout_is_tty) == 'true':
        return ['--color']
    else:
        return []

def check_local_changes():
    if git.local_changes():
        raise CmdException('local changes in the tree. Use "refresh" or'
                           ' "reset --hard"')

def check_head_top_equal(crt_series):
    if not crt_series.head_top_equal():
        raise CmdException('HEAD and top are not the same. This can happen'
                           ' if you modify a branch with git. "stg repair'
                           ' --help" explains more about what to do next.')

def check_conflicts():
    if git.get_conflicts():
        raise CmdException('Unsolved conflicts. Please fix the conflicts'
                           ' then use "git add --update <files>" or revert the'
                           ' changes with "reset --hard".')

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

def get_patch_from_list(part_name, patch_list):
    candidates = [full for full in patch_list if str.find(full, part_name) != -1]
    if len(candidates) >= 2:
        out.info('Possible patches:\n  %s' % '\n  '.join(candidates))
        raise CmdException, 'Ambiguous patch name "%s"' % part_name
    elif len(candidates) == 1:
        return candidates[0]
    else:
        return None

def parse_patches(patch_args, patch_list, boundary = 0, ordered = False):
    """Parse patch_args list for patch names in patch_list and return
    a list. The names can be individual patches and/or in the
    patch1..patch2 format.
    """
    # in case it receives a tuple
    patch_list = list(patch_list)
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
    p = email.Utils.parseaddr(address)
    if p[1]:
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

def address_or_alias(addr_pair):
    """Return a name-email tuple the e-mail address is valid or look up
    the aliases in the config files.
    """
    addr = addr_pair[1]
    if '@' in addr:
        # it's an e-mail address
        return addr_pair
    alias = config.get('mail.alias.' + addr)
    if alias:
        # it's an alias
        return name_email(alias)
    raise CmdException, 'unknown e-mail alias: %s' % addr

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
            re.match('Index: ', line) or re.match('--- \w', line)

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
    descr_strip = 0

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
        subject = descr_lines[pos][descr_strip:]
        if re.match('commit [\da-f]{40}$', subject):
            # 'git show' output, look for the real subject
            subject = ''
            descr_strip = 4
        lasthdr = pos + 1

    # get the body
    if lasthdr < end:
        body = '\n' + '\n'.join(l[descr_strip:] for l in descr_lines[lasthdr:])

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
    descr = __decode_header(msg['subject'])
    descr = re.sub('\n[ \t]*', ' ', descr)
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
        if part.get_content_type() in ['text/plain',
                                       'application/octet-stream']:
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

def parse_patch(text, contains_diff):
    """Parse the input text and return (description, authname,
    authemail, authdate, diff)
    """
    if contains_diff:
        (text, diff) = __split_descr_diff(text)
    else:
        diff = None
    (descr, authname, authemail, authdate) = __parse_description(text)

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

def update_commit_data(cd, options):
    """Return a new CommitData object updated according to the command line
    options."""
    # Set the commit message from commandline.
    if options.message is not None:
        cd = cd.set_message(options.message)

    # Modify author data.
    cd = cd.set_author(options.author(cd.author))

    # Add Signed-off-by: or similar.
    if options.sign_str != None:
        sign_str = options.sign_str
    else:
        sign_str = config.get("stgit.autosign")
    if sign_str != None:
        cd = cd.set_message(
            add_sign_line(cd.message, sign_str,
                          cd.committer.name, cd.committer.email))

    # Let user edit the commit message manually, unless
    # --save-template or --message was specified.
    if not getattr(options, 'save_template', None) and options.message is None:
        cd = cd.set_message(edit_string(cd.message, '.stgit-new.txt'))

    return cd

class DirectoryException(StgException):
    pass

class _Directory(object):
    def __init__(self, needs_current_series = True, log = True):
        self.needs_current_series =  needs_current_series
        self.log = log
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
    def write_log(self, msg):
        if self.log:
            log.compat_log_entry(msg)

class DirectoryAnywhere(_Directory):
    def setup(self):
        pass

class DirectoryHasRepository(_Directory):
    def setup(self):
        self.git_dir # might throw an exception
        log.compat_log_external_mods()

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
        self.log = False # stgit.lib.transaction handles logging
    def setup(self):
        # This will throw an exception if we don't have a repository.
        self.repository = libstack.Repository.default()
