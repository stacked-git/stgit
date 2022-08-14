prefix ?= $(HOME)/.local
DESTDIR	?=
INSTALL ?= install

all:
	$(MAKE) -C vim all

install: install-emacs install-vim

install-emacs: stgit.el
	$(INSTALL) -d $(DESTDIR)$(prefix)/share/emacs/site-lisp
	$(INSTALL) -m 0644 $< $(DESTDIR)$(prefix)/share/emacs/site-lisp

install-vim:
	$(MAKE) -C vim install

clean:
	$(MAKE) -C vim clean

.PHONY: all clean install install-emacs install-vim
