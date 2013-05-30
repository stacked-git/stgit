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

import sys, os, re, time, datetime, socket, smtplib, getpass
import email, email.Utils, email.Header
from stgit.argparse import opt
from stgit.commands.common import *
from stgit.utils import *
from stgit.out import *
from stgit import argparse, stack, git, version, templates
from stgit.config import config
from stgit.run import Run
from stgit.lib import git as gitlib

help = 'Send a patch or series of patches by e-mail'
kind = 'patch'
usage = [' [options] [--] [<patch1>] [<patch2>] [<patch3>..<patch4>]']
description = r"""
Send a patch or a range of patches by e-mail using the SMTP server
specified by the 'stgit.smtpserver' configuration option, or the
'--smtp-server' command line option. This option can also be an
absolute path to 'sendmail' followed by command line arguments.

The From address and the e-mail format are generated from the template
file passed as argument to '--template' (defaulting to
'.git/patchmail.tmpl' or '~/.stgit/templates/patchmail.tmpl' or
'/usr/share/stgit/templates/patchmail.tmpl'). A patch can be sent as
attachment using the --attach option in which case the
'mailattch.tmpl' template will be used instead of 'patchmail.tmpl'.

The To/Cc/Bcc addresses can either be added to the template file or
passed via the corresponding command line options. They can be e-mail
addresses or aliases which are automatically expanded to the values
stored in the [mail "alias"] section of GIT configuration files.

A preamble e-mail can be sent using the '--cover' and/or
'--edit-cover' options. The first allows the user to specify a file to
be used as a template. The latter option will invoke the editor on the
specified file (defaulting to '.git/covermail.tmpl' or
'~/.stgit/templates/covermail.tmpl' or
'/usr/share/stgit/templates/covermail.tmpl').

All the subsequent e-mails appear as replies to the first e-mail sent
(either the preamble or the first patch). E-mails can be seen as
replies to a different e-mail by using the '--in-reply-to' option.

SMTP authentication is also possible with '--smtp-user' and
'--smtp-password' options, also available as configuration settings:
'smtpuser' and 'smtppassword'. TLS encryption can be enabled by
'--smtp-tls' option and 'smtptls' setting.

The following variables are accepted by both the preamble and the
patch e-mail templates:

  %(diffstat)s     - diff statistics
  %(number)s       - empty if only one patch is sent or 'patchnr/totalnr'
  %(snumber)s      - stripped version of '%(number)s'
  %(nspace)s       - ' ' if %(number)s is non-empty, otherwise empty string
  %(patchnr)s      - patch number
  %(sender)s       - 'sender'  or 'authname <authemail>' as per the config file
  %(totalnr)s      - total number of patches to be sent
  %(version)s      - 'version' string passed on the command line (or empty)
  %(vspace)s       - ' ' if %(version)s is non-empty, otherwise empty string

In addition to the common variables, the preamble e-mail template
accepts the following:

  %(shortlog)s     - first line of each patch description, listed by author

In addition to the common variables, the patch e-mail template accepts
the following:

  %(authdate)s     - patch creation date
  %(authemail)s    - author's email
  %(authname)s     - author's name
  %(commemail)s    - committer's e-mail
  %(commname)s     - committer's name
  %(diff)s         - unified diff of the patch
  %(fromauth)s     - 'From: author\n\n' if different from sender
  %(longdescr)s    - the rest of the patch description, after the first line
  %(patch)s        - patch name
  %(prefix)s       - 'prefix' string passed on the command line
  %(pspace)s       - ' ' if %(prefix)s is non-empty, otherwise empty string
  %(shortdescr)s   - the first line of the patch description"""

args = [argparse.patch_range(argparse.applied_patches,
                             argparse.unapplied_patches,
                             argparse.hidden_patches)]
options = [
    opt('-a', '--all', action = 'store_true',
        short = 'E-mail all the applied patches'),
    opt('--to', action = 'append',
        short = 'Add TO to the To: list'),
    opt('--cc', action = 'append',
        short = 'Add CC to the Cc: list'),
    opt('--bcc', action = 'append',
        short = 'Add BCC to the Bcc: list'),
    opt('--auto', action = 'store_true',
        short = 'Automatically cc the patch signers'),
    opt('--no-thread', action = 'store_true',
        short = 'Do not send subsequent messages as replies'),
    opt('--unrelated', action = 'store_true',
        short = 'Send patches without sequence numbering'),
    opt('--attach', action = 'store_true',
        short = 'Send a patch as attachment'),
    opt('--attach-inline', action = 'store_true',
        short = 'Send a patch inline and as an attachment'),
    opt('-v', '--version', metavar = 'VERSION',
        short = 'Add VERSION to the [PATCH ...] prefix'),
    opt('--prefix', metavar = 'PREFIX',
        short = 'Add PREFIX to the [... PATCH ...] prefix'),
    opt('-t', '--template', metavar = 'FILE',
        short = 'Use FILE as the message template'),
    opt('-c', '--cover', metavar = 'FILE',
        short = 'Send FILE as the cover message'),
    opt('-e', '--edit-cover', action = 'store_true',
        short = 'Edit the cover message before sending'),
    opt('-E', '--edit-patches', action = 'store_true',
        short = 'Edit each patch before sending'),
    opt('-s', '--sleep', type = 'int', metavar = 'SECONDS',
        short = 'Sleep for SECONDS between e-mails sending'),
    opt('--in-reply-to', metavar = 'REFID',
        short = 'Use REFID as the reference id'),
    opt('--smtp-server', metavar = 'HOST[:PORT] or "/path/to/sendmail -t -i"',
        short = 'SMTP server or command to use for sending mail'),
    opt('-u', '--smtp-user', metavar = 'USER',
        short = 'Username for SMTP authentication'),
    opt('-p', '--smtp-password', metavar = 'PASSWORD',
        short = 'Password for SMTP authentication'),
    opt('-T', '--smtp-tls', action = 'store_true',
        short = 'Use SMTP with TLS encryption'),
    opt('-b', '--branch', args = [argparse.stg_branches],
        short = 'Use BRANCH instead of the default branch'),
    opt('-m', '--mbox', action = 'store_true',
        short = 'Generate an mbox file instead of sending'),
    opt('--git', action = 'store_true',
        short = 'Use git send-email (EXPERIMENTAL)')
    ] + argparse.diff_opts_option()

directory = DirectoryHasRepository(log = False)

def __get_sender():
    """Return the 'authname <authemail>' string as read from the
    configuration file
    """
    sender=config.get('stgit.sender')
    if not sender:
        try:
            sender = str(git.user())
        except git.GitException:
            try:
                sender = str(git.author())
            except git.GitException:
                pass
    if not sender:
        raise CmdException, ('Unknown sender name and e-mail; you should'
                             ' for example set git config user.name and'
                             ' user.email')
    sender = email.Utils.parseaddr(sender)

    return email.Utils.formataddr(address_or_alias(sender))

def __addr_list(msg, header):
    return [addr for name, addr in
            email.Utils.getaddresses(msg.get_all(header, []))]

def __parse_addresses(msg):
    """Return a two elements tuple: (from, [to])
    """
    from_addr_list = __addr_list(msg, 'From')
    if len(from_addr_list) == 0:
        raise CmdException, 'No "From" address'

    to_addr_list = __addr_list(msg, 'To') + __addr_list(msg, 'Cc') \
                   + __addr_list(msg, 'Bcc')
    if len(to_addr_list) == 0:
        raise CmdException, 'No "To/Cc/Bcc" addresses'

    return (from_addr_list[0], set(to_addr_list))

def __send_message_sendmail(sendmail, msg):
    """Send the message using the sendmail command.
    """
    cmd = sendmail.split()
    Run(*cmd).raw_input(msg).discard_output()

__smtp_credentials = None

def __set_smtp_credentials(options):
    """Set the (smtpuser, smtppassword, smtpusetls) credentials if the method
    of sending is SMTP.
    """
    global __smtp_credentials

    smtpserver = options.smtp_server or config.get('stgit.smtpserver')
    if options.mbox or options.git or smtpserver.startswith('/'):
        return

    smtppassword = options.smtp_password or config.get('stgit.smtppassword')
    smtpuser = options.smtp_user or config.get('stgit.smtpuser')
    smtpusetls = options.smtp_tls or config.get('stgit.smtptls') == 'yes'

    if (smtppassword and not smtpuser):
        raise CmdException('SMTP password supplied, username needed')
    if (smtpusetls and not smtpuser):
        raise CmdException('SMTP over TLS requested, username needed')
    if (smtpuser and not smtppassword):
        smtppassword = getpass.getpass("Please enter SMTP password: ")

    __smtp_credentials = (smtpuser, smtppassword, smtpusetls)

def __send_message_smtp(smtpserver, from_addr, to_addr_list, msg, options):
    """Send the message using the given SMTP server
    """
    smtpuser, smtppassword, smtpusetls = __smtp_credentials

    try:
        s = smtplib.SMTP(smtpserver)
    except Exception, err:
        raise CmdException, str(err)

    s.set_debuglevel(0)
    try:
        if smtpuser and smtppassword:
            s.ehlo()
            if smtpusetls:
                if not hasattr(socket, 'ssl'):
                    raise CmdException,  "cannot use TLS - no SSL support in Python"
                s.starttls()
                s.ehlo()
            s.login(smtpuser, smtppassword)

        result = s.sendmail(from_addr, to_addr_list, msg)
        if len(result):
            print "mail server refused delivery for the following recipients: %s" % result
    except Exception, err:
        raise CmdException, str(err)

    s.quit()

def __send_message_git(msg, options):
    """Send the message using git send-email
    """
    from subprocess import call
    from tempfile import mkstemp

    cmd = ["git", "send-email", "--from=%s" % msg['From']]
    cmd.append("--quiet")
    cmd.append("--suppress-cc=self")
    if not options.auto:
        cmd.append("--suppress-cc=body")
    if options.in_reply_to:
        cmd.extend(["--in-reply-to", options.in_reply_to])
    if options.no_thread:
        cmd.append("--no-thread")

    # We only support To/Cc/Bcc in git send-email for now.
    for x in ['to', 'cc', 'bcc']:
        if getattr(options, x):
            cmd.extend('--%s=%s' % (x, a) for a in getattr(options, x))

    (fd, path) = mkstemp()
    os.write(fd, msg.as_string(options.mbox))
    os.close(fd)

    try:
        try:
            cmd.append(path)
            call(cmd)
        except Exception, err:
            raise CmdException, str(err)
    finally:
        os.unlink(path)

def __send_message(type, tmpl, options, *args):
    """Message sending dispatcher.
    """
    (build, outstr) = {'cover': (__build_cover, 'the cover message'),
                       'patch': (__build_message, 'patch "%s"' % args[0])}[type]
    if type == 'patch':
        (patch_nr, total_nr) = (args[1], args[2])

    msg_id = email.Utils.make_msgid('stgit')
    msg = build(tmpl, msg_id, options, *args)

    msg_str = msg.as_string(options.mbox)
    if options.mbox:
        out.stdout_raw(msg_str + '\n')
        return msg_id

    if not options.git:
        from_addr, to_addrs = __parse_addresses(msg)
        out.start('Sending ' + outstr)

    smtpserver = options.smtp_server or config.get('stgit.smtpserver')
    if options.git:
        __send_message_git(msg, options)
    elif smtpserver.startswith('/'):
        # Use the sendmail tool
        __send_message_sendmail(smtpserver, msg_str)
    else:
        # Use the SMTP server (we have host and port information)
        __send_message_smtp(smtpserver, from_addr, to_addrs, msg_str, options)

    # give recipients a chance of receiving related patches in correct order
    if type == 'cover' or (type == 'patch' and patch_nr < total_nr):
        sleep = options.sleep or config.getint('stgit.smtpdelay')
        time.sleep(sleep)
    if not options.git:
        out.done()
    return msg_id

def __update_header(msg, header, addr = '', ignore = ()):
    def __addr_pairs(msg, header, extra):
        pairs = email.Utils.getaddresses(msg.get_all(header, []) + extra)
        # remove pairs without an address and resolve the aliases
        return [address_or_alias(p) for p in pairs if p[1]]

    addr_pairs = __addr_pairs(msg, header, [addr])
    del msg[header]
    # remove the duplicates and filter the addresses
    addr_dict = dict((addr, email.Utils.formataddr((name, addr)))
                     for name, addr in addr_pairs if addr not in ignore)
    if addr_dict:
        msg[header] = ', '.join(addr_dict.itervalues())
    return set(addr_dict.iterkeys())

def __build_address_headers(msg, options, extra_cc = []):
    """Build the address headers and check existing headers in the
    template.
    """
    to_addr = ''
    cc_addr = ''
    extra_cc_addr = ''
    bcc_addr = ''

    autobcc = config.get('stgit.autobcc') or ''

    if options.to:
        to_addr = ', '.join(options.to)
    if options.cc:
        cc_addr = ', '.join(options.cc)
    if extra_cc:
        extra_cc_addr = ', '.join(extra_cc)
    if options.bcc:
        bcc_addr = ', '.join(options.bcc + [autobcc])
    elif autobcc:
        bcc_addr = autobcc

    # if an address is on a header, ignore it from the rest
    from_set = __update_header(msg, 'From')
    to_set = __update_header(msg, 'To', to_addr)
    # --auto generated addresses, don't include the sender
    __update_header(msg, 'Cc', extra_cc_addr, from_set)
    cc_set = __update_header(msg, 'Cc', cc_addr, to_set)
    bcc_set = __update_header(msg, 'Bcc', bcc_addr, to_set.union(cc_set))

def __get_signers_list(msg):
    """Return the address list generated from signed-off-by and
    acked-by lines in the message.
    """
    addr_list = []
    tags = '%s|%s|%s|%s|%s|%s|%s' % (
            'signed-off-by',
            'acked-by',
            'cc',
            'reviewed-by',
            'reported-by',
            'tested-by',
            'reported-and-tested-by')
    regex = '^(%s):\s+(.+)$' % tags

    r = re.compile(regex, re.I)
    for line in msg.split('\n'):
        m = r.match(line)
        if m:
            addr_list.append(m.expand('\g<2>'))

    return addr_list

def __build_extra_headers(msg, msg_id, ref_id = None):
    """Build extra email headers and encoding
    """
    del msg['Date']
    msg['Date'] = email.Utils.formatdate(localtime = True)
    msg['Message-ID'] = msg_id
    if ref_id:
        # make sure the ref id has the angle brackets
        ref_id = '<%s>' % ref_id.strip(' \t\n<>')
        msg['In-Reply-To'] = ref_id
        msg['References'] = ref_id
    msg['User-Agent'] = 'StGit/%s' % version.version

    # update other address headers
    __update_header(msg, 'Reply-To')
    __update_header(msg, 'Mail-Reply-To')
    __update_header(msg, 'Mail-Followup-To')


def __encode_message(msg):
    # 7 or 8 bit encoding
    charset = email.Charset.Charset('utf-8')
    charset.body_encoding = None

    # encode headers
    for header, value in msg.items():
        words = []
        for word in value.split(' '):
            try:
                uword = unicode(word, 'utf-8')
            except UnicodeDecodeError:
                # maybe we should try a different encoding or report
                # the error. At the moment, we just ignore it
                pass
            words.append(email.Header.Header(uword).encode())
        new_val = ' '.join(words)
        msg.replace_header(header, new_val)

    # replace the Subject string with a Header() object otherwise the long
    # line folding is done using "\n\t" rather than "\n ", causing issues with
    # some e-mail clients
    subject = msg.get('subject', '')
    msg.replace_header('subject',
                       email.Header.Header(subject, header_name = 'subject'))

    # encode the body and set the MIME and encoding headers
    if msg.is_multipart():
        for p in msg.get_payload():
            p.set_charset(charset)
    else:
        msg.set_charset(charset)

def __edit_message(msg):
    fname = '.stgitmail.txt'

    # create the initial file
    f = file(fname, 'w')
    f.write(msg)
    f.close()

    call_editor(fname)

    # read the message back
    f = file(fname)
    msg = f.read()
    f.close()

    return msg

def __build_cover(tmpl, msg_id, options, patches):
    """Build the cover message (series description) to be sent via SMTP
    """
    sender = __get_sender()

    if options.version:
        version_str = '%s' % options.version
        version_space = ' '
    else:
        version_str = ''
        version_space = ''

    if options.prefix:
        prefix_str = options.prefix
    else:
        prefix_str = config.get('stgit.mail.prefix')
    if prefix_str:
        prefix_space = ' '
    else:
        prefix_str = ''
        prefix_space = ''

    total_nr_str = str(len(patches))
    patch_nr_str = '0'.zfill(len(total_nr_str))
    if len(patches) > 1:
        number_str = '%s/%s' % (patch_nr_str, total_nr_str)
        number_space = ' '
    else:
        number_str = ''
        number_space = ''

    tmpl_dict = {'sender':       sender,
                 # for backward template compatibility
                 'maintainer':   sender,
                 # for backward template compatibility
                 'endofheaders': '',
                 # for backward template compatibility
                 'date':         '',
                 'version':      version_str,
                 'vspace':       version_space,
                 'prefix':       prefix_str,
                 'pspace':       prefix_space,
                 'patchnr':      patch_nr_str,
                 'totalnr':      total_nr_str,
                 'number':       number_str,
                 'nspace':       number_space,
                 'snumber':      number_str.strip(),
                 'shortlog':     stack.shortlog(crt_series.get_patch(p)
                                                for p in reversed(patches)),
                 'diffstat':     gitlib.diffstat(git.diff(
                     rev1 = git_id(crt_series, '%s^' % patches[0]),
                     rev2 = git_id(crt_series, '%s' % patches[-1]),
                     diff_flags = options.diff_flags))}

    try:
        msg_string = tmpl % tmpl_dict
    except KeyError, err:
        raise CmdException, 'Unknown patch template variable: %s' \
              % err
    except TypeError:
        raise CmdException, 'Only "%(name)s" variables are ' \
              'supported in the patch template'

    if options.edit_cover:
        msg_string = __edit_message(msg_string)

    # The Python email message
    try:
        msg = email.message_from_string(msg_string)
    except Exception, ex:
        raise CmdException, 'template parsing error: %s' % str(ex)

    if not options.git:
        __build_address_headers(msg, options)
    __build_extra_headers(msg, msg_id, options.in_reply_to)
    __encode_message(msg)

    return msg

def __build_message(tmpl, msg_id, options, patch, patch_nr, total_nr, ref_id):
    """Build the message to be sent via SMTP
    """
    p = crt_series.get_patch(patch)

    if p.get_description():
        descr = p.get_description().strip()
    else:
        # provide a place holder and force the edit message option on
        descr = '<empty message>'
        options.edit_patches = True

    descr_lines = descr.split('\n')
    short_descr = descr_lines[0].strip()
    long_descr = '\n'.join(l.rstrip() for l in descr_lines[1:]).lstrip('\n')

    authname = p.get_authname();
    authemail = p.get_authemail();
    commname = p.get_commname();
    commemail = p.get_commemail();

    sender = __get_sender()

    fromauth = '%s <%s>' % (authname, authemail)
    if fromauth != sender:
        fromauth = 'From: %s\n\n' % fromauth
    else:
        fromauth = ''

    if options.version:
        version_str = '%s' % options.version
        version_space = ' '
    else:
        version_str = ''
        version_space = ''

    if options.prefix:
        prefix_str = options.prefix
    else:
        prefix_str = config.get('stgit.mail.prefix')
    if prefix_str:
        prefix_space = ' '
    else:
        prefix_str = ''
        prefix_space = ''

    total_nr_str = str(total_nr)
    patch_nr_str = str(patch_nr).zfill(len(total_nr_str))
    if not options.unrelated and total_nr > 1:
        number_str = '%s/%s' % (patch_nr_str, total_nr_str)
        number_space = ' '
    else:
        number_str = ''
        number_space = ''

    diff = git.diff(rev1 = git_id(crt_series, '%s^' % patch),
                    rev2 = git_id(crt_series, '%s' % patch),
                    diff_flags = options.diff_flags)
    tmpl_dict = {'patch':        patch,
                 'sender':       sender,
                 # for backward template compatibility
                 'maintainer':   sender,
                 'shortdescr':   short_descr,
                 'longdescr':    long_descr,
                 # for backward template compatibility
                 'endofheaders': '',
                 'diff':         diff,
                 'diffstat':     gitlib.diffstat(diff),
                 # for backward template compatibility
                 'date':         '',
                 'version':      version_str,
                 'vspace':       version_space,
                 'prefix':       prefix_str,
                 'pspace':       prefix_space,
                 'patchnr':      patch_nr_str,
                 'totalnr':      total_nr_str,
                 'number':       number_str,
                 'nspace':       number_space,
                 'snumber':      number_str.strip(),
                 'fromauth':     fromauth,
                 'authname':     authname,
                 'authemail':    authemail,
                 'authdate':     p.get_authdate(),
                 'commname':     commname,
                 'commemail':    commemail}
    # change None to ''
    for key in tmpl_dict:
        if not tmpl_dict[key]:
            tmpl_dict[key] = ''

    try:
        msg_string = tmpl % tmpl_dict
    except KeyError, err:
        raise CmdException, 'Unknown patch template variable: %s' \
              % err
    except TypeError:
        raise CmdException, 'Only "%(name)s" variables are ' \
              'supported in the patch template'

    if options.edit_patches:
        msg_string = __edit_message(msg_string)

    # The Python email message
    try:
        msg = email.message_from_string(msg_string)
    except Exception, ex:
        raise CmdException, 'template parsing error: %s' % str(ex)

    if options.auto:
        extra_cc = __get_signers_list(descr)
    else:
        extra_cc = []

    if not options.git:
        __build_address_headers(msg, options, extra_cc)
    __build_extra_headers(msg, msg_id, ref_id)
    __encode_message(msg)

    return msg

def func(parser, options, args):
    """Send the patches by e-mail using the patchmail.tmpl file as
    a template
    """
    applied = crt_series.get_applied()

    if options.all:
        patches = applied
    elif len(args) >= 1:
        unapplied = crt_series.get_unapplied()
        patches = parse_patches(args, applied + unapplied, len(applied))
    else:
        raise CmdException, 'Incorrect options. Unknown patches to send'

    # early test for sender identity
    __get_sender()

    out.start('Checking the validity of the patches')
    for p in patches:
        if crt_series.empty_patch(p):
            raise CmdException, 'Cannot send empty patch "%s"' % p
    out.done()

    total_nr = len(patches)
    if total_nr == 0:
        raise CmdException, 'No patches to send'

    if options.in_reply_to:
        if options.no_thread or options.unrelated:
            raise CmdException, \
                  '--in-reply-to option not allowed with --no-thread or --unrelated'
        ref_id = options.in_reply_to
    else:
        ref_id = None

    # get username/password if sending by SMTP
    __set_smtp_credentials(options)

    # send the cover message (if any)
    if options.cover or options.edit_cover:
        if options.unrelated:
            raise CmdException, 'cover sending not allowed with --unrelated'

        # find the template file
        if options.cover:
            tmpl = file(options.cover).read()
        else:
            tmpl = templates.get_template('covermail.tmpl')
            if not tmpl:
                raise CmdException, 'No cover message template file found'

        msg_id = __send_message('cover', tmpl, options, patches)

        # subsequent e-mails are seen as replies to the first one
        if not options.no_thread:
            ref_id = msg_id

    # send the patches
    if options.template:
        tmpl = file(options.template).read()
    else:
        if options.attach:
            tmpl = templates.get_template('mailattch.tmpl')
        elif options.attach_inline:
            tmpl = templates.get_template('patchandattch.tmpl')
        else:
            tmpl = templates.get_template('patchmail.tmpl')
        if not tmpl:
            raise CmdException, 'No e-mail template file found'

    for (p, n) in zip(patches, range(1, total_nr + 1)):
        msg_id = __send_message('patch', tmpl, options, p, n, total_nr, ref_id)

        # subsequent e-mails are seen as replies to the first one
        if not options.no_thread and not options.unrelated and not ref_id:
            ref_id = msg_id
