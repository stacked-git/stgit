# Guard against environment variables
MAN1_TXT =
MAN5_TXT =
MAN7_TXT =
TECH_DOCS =
ARTICLES =
SP_ARTICLES =
OBSOLETE_HTML =

STGIT_ALL_PY = $(shell find ../stgit -name '*.py')
STG_COMMANDS = $(shell $(PYTHON) ../build.py --commands)
STG_COMMANDS_TXT = $(patsubst %,stg-%.txt,$(STG_COMMANDS))
STGIT_VERSION = $(shell $(PYTHON) ../build.py --stgit-version)

MAN1_TXT += stg.txt
MAN1_TXT += $(STG_COMMANDS_TXT)

ifdef MAN_FILTER
MAN_TXT = $(filter $(MAN_FILTER),$(MAN1_TXT) $(MAN5_TXT) $(MAN7_TXT))
else
MAN_TXT = $(MAN1_TXT) $(MAN5_TXT) $(MAN7_TXT)
MAN_FILTER = $(MAN_TXT)
endif

MAN_XML = $(patsubst %.txt,%.xml,$(MAN_TXT))
MAN_HTML = $(patsubst %.txt,%.html,$(MAN_TXT))

DOC_HTML=$(patsubst %.txt,%.html,$(MAN1_TXT))

ARTICLES += tutorial

ARTICLES_HTML += $(patsubst %,%.html,$(ARTICLES) $(SP_ARTICLES))
HTML_FILTER ?= $(ARTICLES_HTML) $(OBSOLETE_HTML)
DOC_HTML = $(MAN_HTML) $(filter $(HTML_FILTER),$(ARTICLES_HTML) $(OBSOLETE_HTML))

DOC_MAN1 = $(patsubst %.txt,%.1,$(filter $(MAN_FILTER),$(MAN1_TXT)))
DOC_MAN5 = $(patsubst %.txt,%.5,$(filter $(MAN_FILTER),$(MAN5_TXT)))
DOC_MAN7 = $(patsubst %.txt,%.7,$(filter $(MAN_FILTER),$(MAN7_TXT)))

prefix ?= $(HOME)/.local
bindir ?= $(prefix)/bin
htmldir ?= $(prefix)/share/doc/stgit
infodir ?= $(prefix)/share/info
pdfdir ?= $(prefix)/share/doc/stgit
mandir ?= $(prefix)/share/man
man1dir = $(mandir)/man1
man5dir = $(mandir)/man5
man7dir = $(mandir)/man7
# DESTDIR =

ASCIIDOC = asciidoc
ASCIIDOC_EXTRA =
ASCIIDOC_HTML ?= xhtml11
ASCIIDOC_DOCBOOK = docbook
ASCIIDOC_CONF = -f asciidoc.conf
ASCIIDOC_COMMON = $(ASCIIDOC) $(ASCIIDOC_EXTRA) $(ASCIIDOC_CONF) \
		-amanversion=$(STGIT_VERSION) \
		-amanmanual='StGit Manual' -amansource='StGit'
TXT_TO_HTML = $(ASCIIDOC_COMMON) -b $(ASCIIDOC_HTML)
TXT_TO_XML = $(ASCIIDOC_COMMON) -b $(ASCIIDOC_DOCBOOK)
MANPAGE_XSL = manpage-normal.xsl
XMLTO = xmlto
XMLTO_EXTRA =
INSTALL ?= install
RM ?= rm -f

MAKEINFO = makeinfo
INSTALL_INFO = install-info
DOCBOOK2X_TEXI = docbook2x-texi
DBLATEX = dblatex
ASCIIDOC_DBLATEX_DIR = /etc/asciidoc/dblatex
DBLATEX_COMMON = -p $(ASCIIDOC_DBLATEX_DIR)/asciidoc-dblatex.xsl -s $(ASCIIDOC_DBLATEX_DIR)/asciidoc-dblatex.sty
ifndef PERL_PATH
	PERL_PATH = /usr/bin/perl
endif

ifndef NO_MAN_BOLD_LITERAL
XMLTO_EXTRA += -m manpage-bold-literal.xsl
endif

# Newer DocBook stylesheet emits warning cruft in the output when
# this is not set, and if set it shows an absolute link.  Older
# stylesheets simply ignore this parameter.
#
# Distros may want to use MAN_BASE_URL=file:///path/to/git/docs/
# or similar.
ifndef MAN_BASE_URL
MAN_BASE_URL = file://$(htmldir)/
endif
XMLTO_EXTRA += -m manpage-base-url.xsl

# If your target system uses GNU groff, it may try to render
# apostrophes as a "pretty" apostrophe using unicode.  This breaks
# cut&paste, so you should set GNU_ROFF to force them to be ASCII
# apostrophes.  Unfortunately does not work with non-GNU roff.
ifdef GNU_ROFF
XMLTO_EXTRA += -m manpage-quote-apos.xsl
endif

ifdef USE_ASCIIDOCTOR
ASCIIDOC = asciidoctor
ASCIIDOC_CONF =
ASCIIDOC_HTML = xhtml5
ASCIIDOC_DOCBOOK = docbook5
ASCIIDOC_EXTRA += -acompat-mode -atabsize=8
ASCIIDOC_EXTRA += -I. -rasciidoctor-extensions
ASCIIDOC_EXTRA += -alitdd='&\#x2d;&\#x2d;'
DBLATEX_COMMON =
XMLTO_EXTRA += --skip-validation
XMLTO_EXTRA += -x manpage.xsl
endif

SHELL_PATH ?= $(SHELL)
# Shell quote;
SHELL_PATH_SQ = $(subst ','\'',$(SHELL_PATH))
# ' <-- help syntax highlighters back into good state

QUIET_SUBDIR0  = +$(MAKE) -C # space to separate -C and subdir
QUIET_SUBDIR1  =

ifneq ($(findstring $(MAKEFLAGS),w),w)
PRINT_DIR = --no-print-directory
else # "make -w"
NO_SUBDIR = :
endif

ifneq ($(findstring $(MAKEFLAGS),s),s)
ifndef V
	QUIET_ASCIIDOC  = @echo '   ' ASCIIDOC $@;
	QUIET_XMLTO     = @echo '   ' XMLTO $@;
	QUIET_DB2TEXI   = @echo '   ' DB2TEXI $@;
	QUIET_MAKEINFO  = @echo '   ' MAKEINFO $@;
	QUIET_DBLATEX   = @echo '   ' DBLATEX $@;
	QUIET_XSLTPROC  = @echo '   ' XSLTPROC $@;
	QUIET_GEN       = @echo '   ' GEN $@;
	QUIET_LINT      = @echo '   ' LINT $@;
	QUIET_STDERR    = 2> /dev/null
	QUIET_SUBDIR0   = +@subdir=
	QUIET_SUBDIR1   = ;$(NO_SUBDIR) echo '   ' SUBDIR $$subdir; \
			  $(MAKE) $(PRINT_DIR) -C $$subdir
	export V
endif
endif

all: html man

html: $(DOC_HTML)

man: man1 man5 man7
man1: $(DOC_MAN1)
man5: $(DOC_MAN5)
man7: $(DOC_MAN7)

info: stgitman.info

pdf: tutorial.pdf

install: install-man

install-man: man
	$(INSTALL) -d -m 755 $(DESTDIR)$(man1dir)
	$(INSTALL) -m 644 $(DOC_MAN1) $(DESTDIR)$(man1dir)

install-info: info
	$(INSTALL) -d -m 755 $(DESTDIR)$(infodir)
	$(INSTALL) -m 644 stgitman.info $(DESTDIR)$(infodir)
	if test -r $(DESTDIR)$(infodir)/dir; then \
	  $(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) stgitman.info ;\
	else \
	  echo "No directory found in $(DESTDIR)$(infodir)" >&2 ; \
	fi

install-pdf: pdf
	$(INSTALL) -d -m 755 $(DESTDIR)$(pdfdir)
	$(INSTALL) -m 644 tutorial.pdf $(DESTDIR)$(pdfdir)

install-html: html
	$(INSTALL) -d -m 755 $(DESTDIR)$(htmldir)
	$(INSTALL) -m 644 $(DOC_HTML) $(DESTDIR)$(htmldir)

$(DOC_HTML) $(DOC_MAN1): asciidoc.conf

#
# Determine "include::" file references in asciidoc files.
#
doc.dep : $(wildcard *.txt) build-docdep.perl
	$(QUIET_GEN)$(RM) $@+ $@ && \
	$(PERL_PATH) ./build-docdep.perl >$@+ $(QUIET_STDERR) && \
	mv $@+ $@

-include doc.dep

HTML-ASCIIDOCFLAGS: FORCE
	@FLAGS='$(subst ','\'',$(TXT_TO_HTML))'; \
		if test x"$$FLAGS" != x"`cat HTML-ASCIIDOCFLAGS 2>/dev/null`"; then \
			echo >&2 "    * new HTML-ASCIIDOCFLAGS"; \
			echo "$$FLAGS" >HTML-ASCIIDOCFLAGS; \
		fi

XML-ASCIIDOCFLAGS: FORCE
	@FLAGS='$(subst ','\'',$(TXT_TO_XML))'; \
		if test x"$$FLAGS" != x"`cat XML-ASCIIDOCFLAGS 2>/dev/null`"; then \
			echo >&2 "    * new XML-ASCIIDOCFLAGS"; \
			echo "$$FLAGS" >XML-ASCIIDOCFLAGS; \
		fi

clean:
	$(RM) *.xml *.xml+ *.html *.html+ *.1 *.5 *.7
	$(RM) *.texi *.texi+ *.texi++ stgitman.info
	$(RM) *.pdf
	$(RM) doc.dep
	$(RM) manpage-base-url.xsl
	$(RM) *-ASCIIDOCFLAGS
	$(RM) $(STG_COMMANDS_TXT) command-list.txt

$(MAN_HTML): %.html : %.txt asciidoc.conf asciidoctor-extensions.rb HTML-ASCIIDOCFLAGS
	$(QUIET_ASCIIDOC)$(RM) $@+ $@ && \
	$(TXT_TO_HTML) -d manpage -o $@+ $< && \
	mv $@+ $@

$(OBSOLETE_HTML): %.html : %.txto asciidoc.conf asciidoctor-extensions.rb HTML-ASCIIDOCFLAGS
	$(QUIET_ASCIIDOC)$(RM) $@+ $@ && \
	$(TXT_TO_HTML) -o $@+ $< && \
	mv $@+ $@

manpage-base-url.xsl: manpage-base-url.xsl.in
	$(QUIET_GEN)sed "s|@@MAN_BASE_URL@@|$(MAN_BASE_URL)|" $< > $@

%.1 %.5 %.7 : %.xml manpage-base-url.xsl $(wildcard manpage*.xsl)
	$(QUIET_XMLTO)$(RM) $@ && \
	$(XMLTO) -m $(MANPAGE_XSL) $(XMLTO_EXTRA) man $<

%.xml : %.txt asciidoc.conf asciidoctor-extensions.rb XML-ASCIIDOCFLAGS
	$(QUIET_ASCIIDOC)$(RM) $@+ $@ && \
	$(TXT_TO_XML) -d manpage -o $@+ $< && \
	mv $@+ $@

tutorial.xml: tutorial.txt tutorial.conf asciidoctor-extensions.rb XML-ASCIIDOCFLAGS
	$(QUIET_ASCIIDOC)$(RM) $@+ $@ && \
	$(TXT_TO_XML) -d article -o $@+ $< && \
	mv $@+ $@

tutorial.pdf: tutorial.xml
	$(QUIET_DBLATEX)$(RM) $@+ $@ && \
	$(DBLATEX) -o $@+ $(DBLATEX_COMMON) $< && \
	mv $@+ $@

stgitman.texi: $(MAN_XML) cat-texi.perl texi.xsl
	$(QUIET_DB2TEXI)$(RM) $@+ $@ && \
	($(foreach xml,$(sort $(MAN_XML)),xsltproc -o $(xml)+ texi.xsl $(xml) && \
		$(DOCBOOK2X_TEXI) --encoding=UTF-8 --to-stdout $(xml)+ && \
		rm $(xml)+ &&) true) > $@++ && \
	$(PERL_PATH) cat-texi.perl $@ <$@++ >$@+ && \
	rm $@++ && \
	mv $@+ $@

stgitman.info: stgitman.texi
	$(QUIET_MAKEINFO)$(MAKEINFO) --no-split --no-validate $*.texi

$(patsubst %.txt,%.texi,$(MAN_TXT)): %.texi : %.xml
	$(QUIET_DB2TEXI)$(RM) $@+ $@ && \
	$(DOCBOOK2X_TEXI) --to-stdout $*.xml >$@+ && \
	mv $@+ $@

$(patsubst %,%.html,$(ARTICLES)) : %.html : %.txt HTML-ASCIIDOCFLAGS
	$(QUIET_ASCIIDOC)$(TXT_TO_HTML) -a toc $*.txt

print-man1:
	@for i in $(MAN1_TXT); do echo $$i; done

.PHONY: FORCE

$(STG_COMMANDS_TXT): $(STGIT_ALL_PY)
	$(QUIET_GEN)$(PYTHON) ../build.py --asciidoc $(basename $(subst stg-,,$@)) > $@

command-list.txt: $(STGIT_ALL_PY)
	$(QUIET_GEN)$(PYTHON) ../build.py --cmd-list > $@

build-txt: command-list.txt FORCE
