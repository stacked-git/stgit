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

import sys, os, re, time, datetime, smtplib
import email, email.Utils, email.Header
from optparse import OptionParser, make_option

from stgit.commands.common import *
from stgit.utils import *
from stgit import stack, git, version, templates
from stgit.config import config


help = 'send a patch or series of patches by e-mail'
usage = """%prog [options] [<patch1>] [<patch2>] [<patch3>..<patch4>]

Send a patch or a range of patches by e-mail using the 'smtpserver'
configuration option. The From address and the e-mail format are
generated from the template file passed as argument to '--template'
(defaulting to '.git/patchmail.tmpl' or
'~/.stgit/templates/patchmail.tmpl' or
'/usr/share/stgit/templates/patchmail.tmpl').

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
replies to a different e-mail by using the '--refid' option.

SMTP authentication is also possible with '--smtp-user' and
'--smtp-password' options, also available as configuration settings:
'smtpuser' and 'smtppassword'.

The patch e-mail template accepts the following variables:

  %(patch)s        - patch name
  %(sender)s       - 'sender'  or 'authname <authemail>' as per the config file
  %(shortdescr)s   - the first line of the patch description
  %(longdescr)s    - the rest of the patch description, after the first line
  %(diff)s         - unified diff of the patch
  %(diffstat)s     - diff statistics
  %(version)s      - ' version' string passed on the command line (or empty)
  %(prefix)s       - 'prefix ' string passed on the command line
  %(patchnr)s      - patch number
  %(totalnr)s      - total number of patches to be sent
  %(number)s       - empty if only one patch is sent or ' patchnr/totalnr'
  %(fromauth)s     - 'From: author\\n\\n' if different from sender
  %(authname)s     - author's name
  %(authemail)s    - author's email
  %(authdate)s     - patch creation date
  %(commname)s     - committer's name
  %(commemail)s    - committer's e-mail

For the preamble e-mail template, only the %(sender)s, %(version)s,
%(patchnr)s, %(totalnr)s and %(number)s variables are supported."""

options = [make_option('-a', '--all',
                       help = 'e-mail all the applied patches',
                       action = 'store_true'),
           make_option('--to',
                       help = 'add TO to the To: list',
                       action = 'append'),
           make_option('--cc',
                       help = 'add CC to the Cc: list',
                       action = 'append'),
           make_option('--bcc',
                       help = 'add BCC to the Bcc: list',
                       action = 'append'),
           make_option('--auto',
                       help = 'automatically cc the patch signers',
                       action = 'store_true'),
           make_option('--noreply',
                       help = 'do not send subsequent messages as replies',
                       action = 'store_true'),
           make_option('--unrelated',
                       help = 'send patches without sequence numbering',
                       action = 'store_true'),
           make_option('-v', '--version', metavar = 'VERSION',
                       help = 'add VERSION to the [PATCH ...] prefix'),
           make_option('--prefix', metavar = 'PREFIX',
                       help = 'add PREFIX to the [... PATCH ...] prefix'),
           make_option('-t', '--template', metavar = 'FILE',
                       help = 'use FILE as the message template'),
           make_option('-c', '--cover', metavar = 'FILE',
                       help = 'send FILE as the cover message'),
           make_option('-e', '--edit-cover',
                       help = 'edit the cover message before sending',
                       action = 'store_true'),
           make_option('-E', '--edit-patches',
                       help = 'edit each patch before sending',
                       action = 'store_true'),
           make_option('-s', '--sleep', type = 'int', metavar = 'SECONDS',
                       help = 'sleep for SECONDS between e-mails sending'),
           make_option('--refid',
                       help = 'use REFID as the reference id'),
           make_option('-u', '--smtp-user', metavar = 'USER',
                       help = 'username for SMTP authentication'),
           make_option('-p', '--smtp-password', metavar = 'PASSWORD',
                       help = 'username for SMTP authentication'),
           make_option('-b', '--branch',
                       help = 'use BRANCH instead of the default one'),
           make_option('-O', '--diff-opts',
                       help = 'options to pass to git-diff'),
           make_option('-m', '--mbox',
                       help = 'generate an mbox file instead of sending',
                       action = 'store_true')]


def __get_sender():
    """Return the 'authname <authemail>' string as read from the
    configuration file
    """
    sender=config.get('stgit.sender')
    if not sender:
        try:
            sender = str(git.user())
        except git.GitException:
            sender = str(git.author())

    if not sender:
        raise CmdException, 'unknown sender details'

    return address_or_alias(sender)

def __parse_addresses(msg):
    """Return a two elements tuple: (from, [to])
    """
    def __addr_list(msg, header):
        return [name_addr[1] for name_addr in
                email.Utils.getaddresses(msg.get_all(header, []))]

    from_addr_list = __addr_list(msg, 'From')
    if len(from_addr_list) == 0:
        raise CmdException, 'No "From" address'

    to_addr_list = __addr_list(msg, 'To') + __addr_list(msg, 'Cc') \
                   + __addr_list(msg, 'Bcc')
    if len(to_addr_list) == 0:
        raise CmdException, 'No "To/Cc/Bcc" addresses'

    return (from_addr_list[0], to_addr_list)

def __send_message(smtpserver, from_addr, to_addr_list, msg, sleep,
                   smtpuser, smtppassword):
    """Send the message using the given SMTP server
    """
    try:
        s = smtplib.SMTP(smtpserver)
    except Exception, err:
        raise CmdException, str(err)

    s.set_debuglevel(0)
    try:
        if smtpuser and smtppassword:
            s.ehlo()
            s.login(smtpuser, smtppassword)

        result = s.sendmail(from_addr, to_addr_list, msg)
        if len(result):
            print "mail server refused delivery for the following recipients: %s" % result
        # give recipients a chance of receiving patches in the correct order
        time.sleep(sleep)
    except Exception, err:
        raise CmdException, str(err)

    s.quit()

def __build_address_headers(msg, options, extra_cc = []):
    """Build the address headers and check existing headers in the
    template.
    """
    def __replace_header(header, addr):
        if addr:
            crt_addr = msg[header]
            del msg[header]

            if crt_addr:
                msg[header] = address_or_alias(', '.join([crt_addr, addr]))
            else:
                msg[header] = address_or_alias(addr)

    to_addr = ''
    cc_addr = ''
    bcc_addr = ''

    autobcc = config.get('stgit.autobcc') or ''

    if options.to:
        to_addr = ', '.join(options.to)
    if options.cc:
        cc_addr = ', '.join(options.cc + extra_cc)
    elif extra_cc:
        cc_addr = ', '.join(extra_cc)
    if options.bcc:
        bcc_addr = ', '.join(options.bcc + [autobcc])
    elif autobcc:
        bcc_addr = autobcc

    __replace_header('To', to_addr)
    __replace_header('Cc', cc_addr)
    __replace_header('Bcc', bcc_addr)

def __get_signers_list(msg):
    """Return the address list generated from signed-off-by and
    acked-by lines in the message.
    """
    addr_list = []

    r = re.compile('^(signed-off-by|acked-by|cc):\s+(.+)$', re.I)
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
        msg['In-Reply-To'] = ref_id
        msg['References'] = ref_id
    msg['User-Agent'] = 'StGIT/%s' % version.version

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

    # encode the body and set the MIME and encoding headers
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

def __build_cover(tmpl, total_nr, msg_id, options):
    """Build the cover message (series description) to be sent via SMTP
    """
    sender = __get_sender()

    if options.version:
        version_str = ' %s' % options.version
    else:
        version_str = ''

    if options.prefix:
        prefix_str = options.prefix + ' '
    else:
        confprefix = config.get('stgit.mail.prefix')
        if confprefix:
            prefix_str = confprefix + ' '
        else:
            prefix_str = ''
        
    total_nr_str = str(total_nr)
    patch_nr_str = '0'.zfill(len(total_nr_str))
    if total_nr > 1:
        number_str = ' %s/%s' % (patch_nr_str, total_nr_str)
    else:
        number_str = ''

    tmpl_dict = {'sender':       sender,
                 # for backward template compatibility
                 'maintainer':   sender,
                 # for backward template compatibility
                 'endofheaders': '',
                 # for backward template compatibility
                 'date':         '',
                 'version':      version_str,
                 'prefix':	 prefix_str,
                 'patchnr':      patch_nr_str,
                 'totalnr':      total_nr_str,
                 'number':       number_str}

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

    __build_address_headers(msg, options)
    __build_extra_headers(msg, msg_id, options.refid)
    __encode_message(msg)

    return msg

def __build_message(tmpl, patch, patch_nr, total_nr, msg_id, ref_id, options):
    """Build the message to be sent via SMTP
    """
    p = crt_series.get_patch(patch)

    descr = p.get_description().strip()
    descr_lines = descr.split('\n')

    short_descr = descr_lines[0].rstrip()
    long_descr = '\n'.join(descr_lines[1:]).lstrip()

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
        version_str = ' %s' % options.version
    else:
        version_str = ''

    if options.prefix:
        prefix_str = options.prefix + ' '
    else:
        confprefix = config.get('stgit.mail.prefix')
        if confprefix:
            prefix_str = confprefix + ' '
        else:
            prefix_str = ''
        
    if options.diff_opts:
        diff_flags = options.diff_opts.split()
    else:
        diff_flags = []

    total_nr_str = str(total_nr)
    patch_nr_str = str(patch_nr).zfill(len(total_nr_str))
    if not options.unrelated and total_nr > 1:
        number_str = ' %s/%s' % (patch_nr_str, total_nr_str)
    else:
        number_str = ''

    tmpl_dict = {'patch':        patch,
                 'sender':       sender,
                 # for backward template compatibility
                 'maintainer':   sender,
                 'shortdescr':   short_descr,
                 'longdescr':    long_descr,
                 # for backward template compatibility
                 'endofheaders': '',
                 'diff':         git.diff(rev1 = git_id('%s//bottom' % patch),
                                          rev2 = git_id('%s//top' % patch),
                                          diff_flags = diff_flags ),
                 'diffstat':     git.diffstat(rev1 = git_id('%s//bottom'%patch),
                                              rev2 = git_id('%s//top' % patch)),
                 # for backward template compatibility
                 'date':         '',
                 'version':      version_str,
                 'prefix':       prefix_str,
                 'patchnr':      patch_nr_str,
                 'totalnr':      total_nr_str,
                 'number':       number_str,
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

    __build_address_headers(msg, options, extra_cc)
    __build_extra_headers(msg, msg_id, ref_id)
    __encode_message(msg)

    return msg

def func(parser, options, args):
    """Send the patches by e-mail using the patchmail.tmpl file as
    a template
    """
    smtpserver = config.get('stgit.smtpserver')

    applied = crt_series.get_applied()

    if options.all:
        patches = applied
    elif len(args) >= 1:
        unapplied = crt_series.get_unapplied()
        patches = parse_patches(args, applied + unapplied, len(applied))
    else:
        raise CmdException, 'Incorrect options. Unknown patches to send'

    smtppassword = options.smtp_password or config.get('stgit.smtppassword')
    smtpuser = options.smtp_user or config.get('stgit.smtpuser')

    if (smtppassword and not smtpuser):
        raise CmdException, 'SMTP password supplied, username needed'
    if (smtpuser and not smtppassword):
        raise CmdException, 'SMTP username supplied, password needed'

    total_nr = len(patches)
    if total_nr == 0:
        raise CmdException, 'No patches to send'

    if options.refid:
        if options.noreply or options.unrelated:
            raise CmdException, \
                  '--refid option not allowed with --noreply or --unrelated'
        ref_id = options.refid
    else:
        ref_id = None

    sleep = options.sleep or config.getint('stgit.smtpdelay')

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

        msg_id = email.Utils.make_msgid('stgit')
        msg = __build_cover(tmpl, total_nr, msg_id, options)
        from_addr, to_addr_list = __parse_addresses(msg)

        msg_string = msg.as_string(options.mbox)

        # subsequent e-mails are seen as replies to the first one
        if not options.noreply:
            ref_id = msg_id

        if options.mbox:
            out.stdout_raw(msg_string + '\n')
        else:
            out.start('Sending the cover message')
            __send_message(smtpserver, from_addr, to_addr_list, msg_string,
                           sleep, smtpuser, smtppassword)
            out.done()

    # send the patches
    if options.template:
        tmpl = file(options.template).read()
    else:
        tmpl = templates.get_template('patchmail.tmpl')
        if not tmpl:
            raise CmdException, 'No e-mail template file found'

    for (p, patch_nr) in zip(patches, range(1, len(patches) + 1)):
        msg_id = email.Utils.make_msgid('stgit')
        msg = __build_message(tmpl, p, patch_nr, total_nr, msg_id, ref_id,
                              options)
        from_addr, to_addr_list = __parse_addresses(msg)

        msg_string = msg.as_string(options.mbox)

        # subsequent e-mails are seen as replies to the first one
        if not options.noreply and not options.unrelated and not ref_id:
            ref_id = msg_id

        if options.mbox:
            out.stdout_raw(msg_string + '\n')
        else:
            out.start('Sending patch "%s"' % p)
            __send_message(smtpserver, from_addr, to_addr_list, msg_string,
                           sleep, smtpuser, smtppassword)
            out.done()
