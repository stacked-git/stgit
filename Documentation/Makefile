# Import tree-wide shared Makefile behavior and libraries
# include ../shared.mak
QUIET_SUBDIR0  = +$(MAKE) -C # space to separate -C and subdir
QUIET_SUBDIR1  =

ifneq ($(findstring w,$(MAKEFLAGS)),w)
PRINT_DIR = --no-print-directory
else # "make -w"
NO_SUBDIR = :
endif

ifneq ($(findstring s,$(MAKEFLAGS)),s)
ifndef V
## common
	QUIET_SUBDIR0  = +@subdir=
	QUIET_SUBDIR1  = ;$(NO_SUBDIR) echo '   ' SUBDIR $$subdir; \
			 $(MAKE) $(PRINT_DIR) -C $$subdir

	QUIET          = @
	QUIET_GEN      = @echo '   ' GEN $@;

	QUIET_MKDIR_P_PARENT  = @echo '   ' MKDIR -p $(@D);

## Used in "Documentation/Makefile"
	QUIET_ASCIIDOC	= @echo '   ' ASCIIDOC $@;
	QUIET_XMLTO	= @echo '   ' XMLTO $@;
	QUIET_DB2TEXI	= @echo '   ' DB2TEXI $@;
	QUIET_MAKEINFO	= @echo '   ' MAKEINFO $@;
	QUIET_DBLATEX	= @echo '   ' DBLATEX $@;
	QUIET_XSLTPROC	= @echo '   ' XSLTPROC $@;
	QUIET_GEN	= @echo '   ' GEN $@;
	QUIET_STDERR	= 2> /dev/null

	export V
endif
endif

# Guard against environment variables
MAN1_TXT =
MAN5_TXT =
MAN7_TXT =
DOC_DEP_TXT =
TECH_DOCS =
ARTICLES =
SP_ARTICLES =
OBSOLETE_HTML =

STG_PROFILE ?= release
CARGO ?= cargo
CARGO_RUN = $(CARGO) --quiet run --profile=$(STG_PROFILE)
STG_COMMANDS_SRC = $(shell find ../src/cmd -name '*.rs')
STG_COMMANDS = $(shell $(CARGO_RUN) completion list commands)
STG_COMMANDS_TXT = $(patsubst %,stg-%.txt,$(STG_COMMANDS))

# STGIT_VERSION = $(shell $(CARGO) --quiet metadata --format-version=1 --no-deps | \
#                         jq -r '.packages[].version')
# STGIT_VERSION = $(shell $(CARGO) --quiet metadata --format-version=1 --no-deps | \
#                         grep -Eo '"version":"[^"]+"' | \
#                         sed -Ee 's/"version":"([^"]+)"/\1/g')
STGIT_VERSION = $(shell $(CARGO_RUN) version --short | sed -e 's/stg //g')

MAN1_TXT += stg.txt
MAN1_TXT += $(STG_COMMANDS_TXT)
DOC_DEP_TXT += $(wildcard *.txt)

ifdef MAN_FILTER
MAN_TXT = $(filter $(MAN_FILTER),$(MAN1_TXT) $(MAN5_TXT) $(MAN7_TXT))
else
MAN_TXT = $(MAN1_TXT) $(MAN5_TXT) $(MAN7_TXT)
MAN_FILTER = $(MAN_TXT)
endif

MAN_XML = $(patsubst %.txt,%.xml,$(MAN_TXT))
MAN_HTML = $(patsubst %.txt,%.html,$(MAN_TXT))

DOC_HTML=$(patsubst %.txt,%.html,$(MAN1_TXT))

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
ASCIIDOC_DEPS = asciidoc.conf GIT-ASCIIDOCFLAGS
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
ASCIIDOC_DEPS = asciidoctor-extensions.rb GIT-ASCIIDOCFLAGS
DBLATEX_COMMON =
XMLTO_EXTRA += --skip-validation
XMLTO_EXTRA += -x manpage.xsl
endif

SHELL_PATH ?= $(SHELL)
# Shell quote;
SHELL_PATH_SQ = $(subst ','\'',$(SHELL_PATH))
# ' <-- help syntax highlighters back into good state


all: html man

html: $(DOC_HTML)

man: man1 man5 man7
man1: $(DOC_MAN1)
man5: $(DOC_MAN5)
man7: $(DOC_MAN7)

info: stgitman.info

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

install-html: html
	$(INSTALL) -d -m 755 $(DESTDIR)$(htmldir)
	$(INSTALL) -m 644 $(DOC_HTML) $(DESTDIR)$(htmldir)

$(DOC_HTML) $(DOC_MAN1): asciidoc.conf

#
# Determine "include::" file references in asciidoc files.
#
doc.dep : $(DOC_DEP_TXT) build-docdep.perl
	$(QUIET_GEN)$(PERL_PATH) ./build-docdep.perl >$@ $(QUIET_STDERR)

ifneq ($(MAKECMDGOALS),clean)
-include doc.dep
endif

TRACK_ASCIIDOCFLAGS = $(subst ','\'',$(ASCIIDOC_COMMON):$(ASCIIDOC_HTML):$(ASCIIDOC_DOCBOOK))

GIT-ASCIIDOCFLAGS: FORCE
	@FLAGS='$(TRACK_ASCIIDOCFLAGS)'; \
	if test x"$$FLAGS" != x"`cat GIT-ASCIIDOCFLAGS 2>/dev/null`" ; then \
	    echo >&2 "    * new asciidoc flags"; \
	    echo "$$FLAGS" >GIT-ASCIIDOCFLAGS; \
	fi

clean:
	$(RM) *.xml *.xml+ *.html *.html+ *.1 *.5 *.7
	$(RM) *.texi *.texi+ *.texi++ stgitman.info
	$(RM) *.pdf
	$(RM) doc.dep
	$(RM) manpage-base-url.xsl
	$(RM) GIT-ASCIIDOCFLAGS
	$(RM) $(STG_COMMANDS_TXT) command-list.txt
	$(RM) txt-stamp

$(MAN_HTML): %.html : %.txt $(ASCIIDOC_DEPS)
	$(QUIET_ASCIIDOC)$(TXT_TO_HTML) -d manpage -o $@ $<

$(OBSOLETE_HTML): %.html : %.txto $(ASCIIDOC_DEPS)
	$(QUIET_ASCIIDOC)$(TXT_TO_HTML) -o $@ $<

manpage-base-url.xsl: manpage-base-url.xsl.in
	$(QUIET_GEN)sed "s|@@MAN_BASE_URL@@|$(MAN_BASE_URL)|" $< > $@

%.1 %.5 %.7 : %.xml manpage-base-url.xsl $(wildcard manpage*.xsl)
	$(QUIET_XMLTO)$(XMLTO) -m $(MANPAGE_XSL) $(XMLTO_EXTRA) man $<

%.xml : %.txt $(ASCIIDOC_DEPS)
	$(QUIET_ASCIIDOC)$(TXT_TO_XML) -d manpage -o $@ $<

stgitman.texi: $(MAN_XML) cat-texi.perl texi.xsl
	$(QUIET_DB2TEXI) \
	($(foreach xml,$(sort $(MAN_XML)),xsltproc -o $(xml)+ texi.xsl $(xml) && \
		$(DOCBOOK2X_TEXI) --encoding=UTF-8 --to-stdout $(xml)+ && \
		$(RM) $(xml)+ &&) true) > $@+ && \
	$(PERL_PATH) cat-texi.perl $@ <$@+ >$@ && \
	$(RM) $@+

stgitman.info: stgitman.texi
	$(QUIET_MAKEINFO)$(MAKEINFO) --no-split --no-validate $<

$(patsubst %.txt,%.texi,$(MAN_TXT)): %.texi : %.xml
	$(QUIET_DB2TEXI)$(DOCBOOK2X_TEXI) --to-stdout $*.xml >$@

$(patsubst %,%.html,$(ARTICLES)) : %.html : %.txt
	$(QUIET_ASCIIDOC)$(TXT_TO_HTML) -a toc $*.txt

print-man1:
	@for i in $(MAN1_TXT); do echo $$i; done

.PHONY: FORCE

$(STG_COMMANDS_TXT): txt-stamp

stg.xml: stg.txt command-list.txt

command-list.txt: $(STG_COMMANDS_SRC)
	$(QUIET_GEN)$(CARGO_RUN) completion list commands --style=asciidoc > $@

txt-stamp: $(STG_COMMANDS_SRC)
	$(QUIET)$(RM) txt-stamp-temp
	$(QUIET)touch txt-stamp-temp
	$(QUIET_GEN)$(CARGO_RUN) completion man
	$(QUIET)mv txt-stamp-temp $@

