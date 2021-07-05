import email
import email.charset
import email.header
import email.utils
import getpass
import io
import os
import re
import smtplib
import time

from stgit import templates, version
from stgit.argparse import diff_opts_option, opt, patch_range
from stgit.commands.common import (
    CmdException,
    DirectoryHasRepository,
    address_or_alias,
    parse_patches,
)
from stgit.config import config
from stgit.lib.git import Person
from stgit.out import out
from stgit.run import Run
from stgit.utils import edit_bytes

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
along with this program; if not, see http://www.gnu.org/licenses/.
"""

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
stored in the [mail "alias"] section of Git configuration files.

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

args = [patch_range('applied_patches', 'unapplied_patches', 'hidden_patches')]
options = [
    opt(
        '-a',
        '--all',
        action='store_true',
        short='E-mail all the applied patches',
    ),
    opt(
        '--to',
        action='append',
        args=['mail_aliases'],
        short='Add TO to the To: list',
    ),
    opt(
        '--cc',
        action='append',
        args=['mail_aliases'],
        short='Add CC to the Cc: list',
    ),
    opt(
        '--bcc',
        action='append',
        args=['mail_aliases'],
        short='Add BCC to the Bcc: list',
    ),
    opt(
        '--auto',
        action='store_true',
        short='Automatically cc the patch signers',
    ),
    opt(
        '--no-thread',
        action='store_true',
        short='Do not send subsequent messages as replies',
    ),
    opt(
        '--unrelated',
        action='store_true',
        short='Send patches without sequence numbering',
    ),
    opt(
        '--attach',
        action='store_true',
        short='Send a patch as attachment',
    ),
    opt(
        '--attach-inline',
        action='store_true',
        short='Send a patch inline and as an attachment',
    ),
    opt(
        '-v',
        '--version',
        metavar='VERSION',
        short='Add VERSION to the [PATCH ...] prefix',
    ),
    opt(
        '--prefix',
        metavar='PREFIX',
        short='Add PREFIX to the [... PATCH ...] prefix',
    ),
    opt(
        '-t',
        '--template',
        metavar='FILE',
        short='Use FILE as the message template',
    ),
    opt(
        '-c',
        '--cover',
        metavar='FILE',
        short='Send FILE as the cover message',
    ),
    opt(
        '-e',
        '--edit-cover',
        action='store_true',
        short='Edit the cover message before sending',
    ),
    opt(
        '-E',
        '--edit-patches',
        action='store_true',
        short='Edit each patch before sending',
    ),
    opt(
        '-s',
        '--sleep',
        type='int',
        metavar='SECONDS',
        short='Sleep for SECONDS between e-mails sending',
    ),
    opt(
        '--in-reply-to',
        metavar='REFID',
        short='Use REFID as the reference id',
    ),
    opt(
        '--smtp-server',
        metavar='HOST[:PORT] or "/path/to/sendmail -t -i"',
        short='SMTP server or command to use for sending mail',
    ),
    opt(
        '-u',
        '--smtp-user',
        metavar='USER',
        short='Username for SMTP authentication',
    ),
    opt(
        '-p',
        '--smtp-password',
        metavar='PASSWORD',
        short='Password for SMTP authentication',
    ),
    opt(
        '-T',
        '--smtp-tls',
        action='store_true',
        short='Use SMTP with TLS encryption',
    ),
    opt(
        '-b',
        '--branch',
        args=['stg_branches'],
        short='Use BRANCH instead of the default branch',
    ),
    opt(
        '-m',
        '--mbox',
        action='store_true',
        short='Generate an mbox file instead of sending',
    ),
    opt(
        '--domain',
        metavar='DOMAIN',
        short='Use DOMAIN when generating message IDs '
        '(instead of the system hostname)',
    ),
    opt(
        '--git',
        action='store_true',
        short='Use git send-email (EXPERIMENTAL)',
    ),
] + diff_opts_option()

directory = DirectoryHasRepository()


def __get_sender():
    """Return the 'authname <authemail>' string as read from the
    configuration file
    """
    sender = config.get('stgit.sender')
    if not sender:
        user = Person.user()
        if user.email:
            sender = user.name_email
        else:
            author = Person.author()
            if author.email:
                sender = author.name_email
            else:
                raise CmdException(
                    'Unknown sender name and e-mail; you should for '
                    'example set git config user.name and user.email'
                )

    sender = email.utils.parseaddr(sender)

    return email.utils.formataddr(address_or_alias(sender))


def __addr_list(msg, header):
    return [addr for name, addr in email.utils.getaddresses(msg.get_all(header, []))]


def __parse_addresses(msg):
    """Return a two elements tuple: (from, [to])"""
    from_addr_list = __addr_list(msg, 'From')
    if len(from_addr_list) == 0:
        raise CmdException('No "From" address')

    to_addr_list = (
        __addr_list(msg, 'To') + __addr_list(msg, 'Cc') + __addr_list(msg, 'Bcc')
    )
    if len(to_addr_list) == 0:
        raise CmdException('No "To/Cc/Bcc" addresses')

    return (from_addr_list[0], set(to_addr_list))


def __send_message_sendmail(sendmail, msg_bytes):
    """Send the message using the sendmail command."""
    cmd = sendmail.split()
    Run(*cmd).encoding(None).raw_input(msg_bytes).discard_output()


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

    if smtppassword and not smtpuser:
        raise CmdException('SMTP password supplied, username needed')
    if smtpusetls and not smtpuser:
        raise CmdException('SMTP over TLS requested, username needed')
    if smtpuser and not smtppassword:
        smtppassword = getpass.getpass("Please enter SMTP password: ")

    __smtp_credentials = (smtpuser, smtppassword, smtpusetls)


def __send_message_smtp(smtpserver, from_addr, to_addr_list, msg, options):
    """Send the message using the given SMTP server"""
    smtpuser, smtppassword, smtpusetls = __smtp_credentials

    try:
        s = smtplib.SMTP(smtpserver)
    except Exception as err:
        raise CmdException(str(err))

    s.set_debuglevel(0)
    try:
        if smtpuser and smtppassword:
            s.ehlo()
            if smtpusetls:
                try:
                    s.starttls()
                except (smtplib.SMTPException, RuntimeError) as e:
                    # RuntimeError indicates that Python lacks SSL support.
                    # SMTPException indicates that server does not do STARTTLS.
                    raise CmdException("cannot use TLS: %s" % e)
                s.ehlo()
            s.login(smtpuser, smtppassword)

        result = s.sendmail(from_addr, to_addr_list, msg)
        if len(result):
            print(
                "mail server refused delivery for the following recipients:",
                result,
            )
    except Exception as err:
        raise CmdException(str(err))

    s.quit()


def __send_message_git(msg_bytes, from_, options):
    """Send the message using git send-email"""
    from subprocess import call
    from tempfile import mkstemp

    cmd = ["git", "send-email", "--from=%s" % from_]
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
    try:
        os.write(fd, msg_bytes)
        os.close(fd)
        try:
            cmd.append(path)
            call(cmd)
        except Exception as err:
            raise CmdException(str(err))
    finally:
        os.unlink(path)


def __send_message(msg_type, tmpl, options, *args):
    """Message sending dispatcher."""
    msg_id = email.utils.make_msgid(
        'stgit', domain=options.domain or config.get('stgit.domain')
    )

    if msg_type == 'cover':
        assert len(args) == 1, 'too many args for msg_type == "cover"'
        patches = args[0]
        msg = __build_cover(tmpl, msg_id, options, patches)
        outstr = 'the cover message'
    elif msg_type == 'patch':
        patch, patch_nr, total_nr, ref_id = args
        msg = __build_message(tmpl, msg_id, options, patch, patch_nr, total_nr, ref_id)
        outstr = 'patch "%s"' % patch
    else:
        raise AssertionError('invalid msg_type: %s' % msg_type)  # pragma: no cover

    msg_bytes = msg.as_bytes(options.mbox)

    if options.mbox:
        out.stdout_bytes(msg_bytes + b'\n')
        return msg_id

    if not options.git:
        from_addr, to_addrs = __parse_addresses(msg)
        out.start('Sending ' + outstr)

    smtpserver = options.smtp_server or config.get('stgit.smtpserver')
    if options.git:
        __send_message_git(msg_bytes, msg['From'], options)
    elif smtpserver.startswith('/'):
        # Use the sendmail tool
        __send_message_sendmail(smtpserver, msg_bytes)
    else:
        # Use the SMTP server (we have host and port information)
        __send_message_smtp(smtpserver, from_addr, to_addrs, msg_bytes, options)

    # give recipients a chance of receiving related patches in correct order
    if msg_type == 'cover' or (msg_type == 'patch' and patch_nr < total_nr):
        sleep = options.sleep or config.getint('stgit.smtpdelay')
        time.sleep(sleep)
    if not options.git:
        out.done()
    return msg_id


def __update_header(msg, header, addr='', ignore=()):
    addr_pairs = email.utils.getaddresses(msg.get_all(header, []) + [addr])
    del msg[header]
    # remove pairs without an address and resolve the aliases
    addr_pairs = [
        address_or_alias(name_addr) for name_addr in addr_pairs if name_addr[1]
    ]
    # remove the duplicates and filter the addresses
    addr_pairs = [name_addr for name_addr in addr_pairs if name_addr[1] not in ignore]
    if addr_pairs:
        msg[header] = ', '.join(map(email.utils.formataddr, addr_pairs))
    return set(addr for _, addr in addr_pairs)


def __build_address_headers(msg, options, extra_cc):
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
    __update_header(msg, 'Bcc', bcc_addr, to_set.union(cc_set))


def __get_signers_list(msg):
    """Return the address list generated from signed-off-by and
    acked-by lines in the message.
    """
    addr_list = []
    tags = '%s|%s|%s|%s|%s|%s|%s|%s' % (
        'signed-off-by',
        'acked-by',
        'cc',
        'reviewed-by',
        'reported-by',
        'tested-by',
        'suggested-by',
        'reported-and-tested-by',
    )

    # N.B. This regex treats '#' as the start of a comment and thus discards everything
    # after the '#'. This does not allow for valid email addresses that contain '#' in
    # the local-part (i.e. the part before the '@') as is allowed by RFC2822. Such email
    # addresses containing '#' in their local-part are thus not supported by this
    # function.
    regex = r'^(%s):\s+([^#]+).*$' % tags

    r = re.compile(regex, re.I)
    for line in msg.split('\n'):
        m = r.match(line)
        if m:
            addr_list.append(m.group(2))

    return addr_list


def __build_extra_headers(msg, msg_id, ref_id=None):
    """Build extra email headers and encoding"""
    del msg['Date']
    msg['Date'] = email.utils.formatdate(localtime=True)
    msg['Message-ID'] = msg_id
    if ref_id:
        # make sure the ref id has the angle brackets
        ref_id = '<%s>' % ref_id.strip(' \t\n<>')
        msg['In-Reply-To'] = ref_id
        msg['References'] = ref_id
    msg['User-Agent'] = 'StGit/%s' % version.get_version()

    # update other address headers
    __update_header(msg, 'Reply-To')
    __update_header(msg, 'Mail-Reply-To')
    __update_header(msg, 'Mail-Followup-To')


def __encode_message(msg):
    # 7 or 8 bit encoding
    charset = email.charset.Charset('utf-8')
    charset.body_encoding = None

    # encode headers
    for header, value in msg.items():
        words = []
        for word in value.split(' '):
            words.append(email.header.Header(word).encode())
        new_val = ' '.join(words)
        msg.replace_header(header, new_val)

    # replace the Subject string with a Header() object otherwise the long
    # line folding is done using "\n\t" rather than "\n ", causing issues with
    # some e-mail clients
    subject = msg.get('subject', '')
    msg.replace_header('subject', email.header.Header(subject, header_name='subject'))

    # encode the body and set the MIME and encoding headers
    if msg.is_multipart():
        for p in msg.get_payload():
            # TODO test if payload can be encoded with charset. Perhaps
            # iterate email.charset.CHARSETS to find an encodable one?
            p.set_charset(charset)
    else:
        msg.set_charset(charset)


def __shortlog(stack, patches):
    cmd = ['git', 'show', '--pretty=short']
    for pn in reversed(patches):
        cmd.append(stack.patches[pn].sha1)
    log = stack.repository.run(cmd).raw_output()
    return stack.repository.run(['git', 'shortlog']).raw_input(log).raw_output()


def __diffstat(stack, patches):
    tree1 = stack.patches[patches[0]].data.parent.data.tree
    tree2 = stack.patches[patches[-1]].data.tree
    return stack.repository.diff_tree(
        tree1,
        tree2,
        diff_opts=['--stat-width=72'],
        stat=True,
    )


def __build_cover(tmpl, msg_id, options, patches):
    """Build the cover message (series description) to be sent via SMTP"""
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

    repository = directory.repository
    stack = repository.current_stack

    tmpl_dict = {
        'sender': sender,  # for backward template compatibility
        'maintainer': sender,  # for backward template compatibility
        'endofheaders': '',  # for backward template compatibility
        'date': '',
        'version': version_str,
        'vspace': version_space,
        'prefix': prefix_str,
        'pspace': prefix_space,
        'patchnr': patch_nr_str,
        'totalnr': total_nr_str,
        'number': number_str,
        'nspace': number_space,
        'snumber': number_str.strip(),
        'shortlog': __shortlog(stack, patches),
        'diffstat': __diffstat(stack, patches),
    }

    try:
        msg_bytes = templates.specialize_template(tmpl, tmpl_dict)
    except KeyError as err:
        raise CmdException('Unknown patch template variable: %s' % err)
    except TypeError:
        raise CmdException(
            'Only "%(name)s" variables are ' 'supported in the patch template'
        )

    if options.edit_cover:
        msg_bytes = edit_bytes(msg_bytes, '.stgitmail.txt')

    # The Python email message
    try:
        msg = email.message_from_bytes(msg_bytes)
    except Exception as ex:
        raise CmdException('template parsing error: %s' % str(ex))

    extra_cc = []
    if options.auto:
        for pn in patches:
            message_str = stack.patchs[pn].data.message_str
            if message_str:
                descr = message_str.strip()
                extra_cc.extend(__get_signers_list(descr))
        extra_cc = list(set(extra_cc))

    if not options.git:
        __build_address_headers(msg, options, extra_cc)
    __build_extra_headers(msg, msg_id, options.in_reply_to)
    __encode_message(msg)

    return msg


def __build_message(tmpl, msg_id, options, patch, patch_nr, total_nr, ref_id):
    """Build the message to be sent via SMTP"""
    repository = directory.repository
    stack = repository.current_stack

    cd = stack.patches[patch].data

    if cd.message_str:
        descr = cd.message_str.strip()
    else:
        # provide a place holder and force the edit message option on
        descr = '<empty message>'
        options.edit_patches = True

    descr_lines = descr.split('\n')
    short_descr = descr_lines[0].strip()
    long_descr = '\n'.join(line.rstrip() for line in descr_lines[1:]).lstrip('\n')

    author = cd.author
    committer = cd.committer

    sender = __get_sender()

    if author.name_email != sender:
        fromauth = 'From: %s\n\n' % author.name_email
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

    diff = repository.diff_tree(
        cd.parent.data.tree,
        cd.tree,
        diff_opts=options.diff_flags,
    )
    tmpl_dict = {
        'patch': patch,
        'sender': sender,
        'maintainer': sender,  # for backward template compatibility
        'shortdescr': short_descr,
        'longdescr': long_descr,
        'endofheaders': '',  # for backward template compatibility
        'diff': diff,
        'diffstat': repository.default_iw.diffstat(diff),
        'date': '',  # for backward template compatibility
        'version': version_str,
        'vspace': version_space,
        'prefix': prefix_str,
        'pspace': prefix_space,
        'patchnr': patch_nr_str,
        'totalnr': total_nr_str,
        'number': number_str,
        'nspace': number_space,
        'snumber': number_str.strip(),
        'fromauth': fromauth,
        'authname': author.name,
        'authemail': author.email,
        'authdate': author.date.rfc2822_format(),
        'commname': committer.name,
        'commemail': committer.email,
    }

    try:
        msg_bytes = templates.specialize_template(tmpl, tmpl_dict)
    except KeyError as err:
        raise CmdException('Unknown patch template variable: %s' % err)
    except TypeError:
        raise CmdException(
            'Only "%(name)s" variables are ' 'supported in the patch template'
        )

    if options.edit_patches:
        msg_bytes = edit_bytes(msg_bytes, '.stgitmail.txt')

    # The Python email message
    try:
        msg = email.message_from_bytes(msg_bytes)
    except Exception as ex:
        raise CmdException('template parsing error: %s' % str(ex))

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
    stack = directory.repository.current_stack
    applied = stack.patchorder.applied

    if options.all:
        patches = applied
    elif len(args) >= 1:
        unapplied = stack.patchorder.unapplied
        patches = parse_patches(args, applied + unapplied, len(applied))
    else:
        raise CmdException('Incorrect options. Unknown patches to send')

    # early test for sender identity
    __get_sender()

    out.start('Checking the validity of the patches')
    for pn in patches:
        if stack.patches[pn].data.is_nochange():
            raise CmdException('Cannot send empty patch "%s"' % pn)
    out.done()

    total_nr = len(patches)
    if total_nr == 0:
        raise CmdException('No patches to send')

    if options.in_reply_to:
        if options.no_thread or options.unrelated:
            raise CmdException(
                '--in-reply-to option not allowed with ' '--no-thread or --unrelated'
            )
        ref_id = options.in_reply_to
    else:
        ref_id = None

    # get username/password if sending by SMTP
    __set_smtp_credentials(options)

    # send the cover message (if any)
    if options.cover or options.edit_cover:
        if options.unrelated:
            raise CmdException('cover sending not allowed with --unrelated')

        # find the template file
        if options.cover:
            with io.open(options.cover, 'r') as f:
                tmpl = f.read()
        else:
            tmpl = templates.get_template('covermail.tmpl')
            if not tmpl:
                raise CmdException('No cover message template file found')

        msg_id = __send_message('cover', tmpl, options, patches)

        # subsequent e-mails are seen as replies to the first one
        if not options.no_thread:
            ref_id = msg_id

    # send the patches
    if options.template:
        with io.open(options.template, 'r') as f:
            tmpl = f.read()
    else:
        if options.attach:
            tmpl = templates.get_template('mailattch.tmpl')
        elif options.attach_inline:
            tmpl = templates.get_template('patchandattch.tmpl')
        else:
            tmpl = templates.get_template('patchmail.tmpl')
        if not tmpl:
            raise CmdException('No e-mail template file found')

    for (p, n) in zip(patches, range(1, total_nr + 1)):
        msg_id = __send_message('patch', tmpl, options, p, n, total_nr, ref_id)

        # subsequent e-mails are seen as replies to the first one
        if not options.no_thread and not options.unrelated and not ref_id:
            ref_id = msg_id
