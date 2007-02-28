MAN1_TXT=$(wildcard stg-*.txt)
MAN7_TXT=stg.txt

DOC_HTML=$(patsubst %.txt,%.html,$(MAN1_TXT) $(MAN7_TXT))

DOC_MAN1=$(patsubst %.txt,%.1,$(MAN1_TXT))
DOC_MAN7=$(patsubst %.txt,%.7,$(MAN7_TXT))

prefix?=$(HOME)
mandir?=$(prefix)/man
man1dir=$(mandir)/man1
man7dir=$(mandir)/man7
# DESTDIR=

ASCIIDOC=asciidoc --unsafe
ASCIIDOC_EXTRA =
INSTALL?=install

#
# Please note that there is a minor bug in asciidoc.
# The version after 6.0.3 _will_ include the patch found here:
#   http://marc.theaimsgroup.com/?l=git&m=111558757202243&w=2
#
# Until that version is released you may have to apply the patch
# yourself - yes, all 6 characters of it!
#

all: html man

html: $(DOC_HTML)

$(DOC_HTML) $(DOC_MAN1) $(DOC_MAN7): asciidoc.conf

man: man1 man7
man1: $(DOC_MAN1)
man7: $(DOC_MAN7)

install: man
	$(INSTALL) -d -m755 $(DESTDIR)$(man1dir) $(DESTDIR)$(man7dir)
	$(INSTALL) -m644 $(DOC_MAN1) $(DESTDIR)$(man1dir)
	$(INSTALL) -m644 $(DOC_MAN7) $(DESTDIR)$(man7dir)
#
# Determine "include::" file references in asciidoc files.
#
doc.dep : $(wildcard *.txt) build-docdep.perl
	rm -f $@+ $@
	perl ./build-docdep.perl >$@+
	mv $@+ $@

-include doc.dep

clean:
	rm -f *.xml *.html *.1 *.7 doc.dep

%.html : %.txt
	$(ASCIIDOC) -b xhtml11 -d manpage -f asciidoc.conf $(ASCIIDOC_EXTRA) $<

%.1 %.7 : %.xml
	xmlto -m callouts.xsl man $<

%.xml : %.txt
	$(ASCIIDOC) -b docbook -d manpage -f asciidoc.conf $<
