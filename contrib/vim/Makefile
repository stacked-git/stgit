prefix ?= $(HOME)/.local
DESTDIR	?=
INSTALL ?= install

all:

install: install-ftdetect install-syntax

install-ftdetect: ftdetect/stg.vim
	$(INSTALL) -d $(DESTDIR)$(prefix)/share/vim/vimfiles/ftdetect
	$(INSTALL) -D -m 0644 $< $(DESTDIR)$(prefix)/share/vim/vimfiles/ftdetect

install-syntax: syntax/*.vim
	$(INSTALL) -d $(DESTDIR)$(prefix)/share/vim/vimfiles/syntax
	$(INSTALL) -D -m 0644 $? $(DESTDIR)$(prefix)/share/vim/vimfiles/syntax

clean:

.PHONY: all clean install install-ftdetect install-syntax
