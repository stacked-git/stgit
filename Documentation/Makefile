COMMANDS = $(shell $(PYTHON) ../stg-build --commands)
COMMANDS_TXT = $(patsubst %,stg-%.txt,$(COMMANDS))

MAN1_TXT= stg.txt $(COMMANDS_TXT)

DOC_HTML=$(patsubst %.txt,%.html,$(MAN1_TXT))

ARTICLES = tutorial

DOC_HTML += $(patsubst %,%.html,$(ARTICLES))
DOC_PDF += $(patsubst %,%.pdf,$(ARTICLES))

DOC_MAN1=$(patsubst %.txt,%.1,$(MAN1_TXT))

prefix?=$(HOME)
htmldir?=$(prefix)/share/doc/stgit
mandir?=$(prefix)/share/man
man1dir=$(mandir)/man1
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
pdf: $(DOC_PDF)

$(DOC_HTML) $(DOC_MAN1): asciidoc.conf

man: man1
man1: $(DOC_MAN1)
install: man
	$(INSTALL) -d -m755 $(DESTDIR)$(man1dir)
	$(INSTALL) -m644 $(DOC_MAN1) $(DESTDIR)$(man1dir)

install-html: html
	$(INSTALL) -d -m755 $(DESTDIR)$(htmldir)
	$(INSTALL) -m644 $(DOC_HTML) $(DESTDIR)$(htmldir)
#
# Determine "include::" file references in asciidoc files.
#
doc.dep : $(wildcard *.txt) build-docdep.perl
	rm -f $@+ $@
	perl ./build-docdep.perl >$@+
	mv $@+ $@

-include doc.dep

clean:
	rm -f *.xml *.html *.pdf *.1 doc.dep $(COMMANDS_TXT) command-list.txt

ALL_PY = $(shell find ../stgit -name '*.py')

$(COMMANDS_TXT): $(ALL_PY)
	$(PYTHON) ../stg-build --asciidoc $(basename $(subst stg-,,$@)) > $@

command-list.txt: $(ALL_PY)
	$(PYTHON) ../stg-build --cmd-list > $@

%.html : %.txt
	$(ASCIIDOC) -b xhtml11 -d manpage -f asciidoc.conf $(ASCIIDOC_EXTRA) $<

%.1 : %.xml
	xmlto -m callouts.xsl man $<

%.xml : %.txt
	$(ASCIIDOC) -b docbook -d manpage -f asciidoc.conf $<

%.pdf: %.xml
	xmlto pdf $<

# special formatting rules
tutorial.html : %.html : %.txt
	$(ASCIIDOC) -b xhtml11 -d article -a toc -f tutorial.conf \
                $(ASCIIDOC_EXTRA) $<
tutorial.xml : %.xml : %.txt
	$(ASCIIDOC) -b docbook -d article -f tutorial.conf $<
