INSTALL ?= install

prefix ?= $(HOME)/.local
bashdir ?= $(prefix)/share/bash-completion/completions

install: install-bash

install-bash:
	${INSTALL} -d -m 755 $(DESTDIR)$(bashdir)
	${INSTALL} -T -m 644 stgit.bash $(DESTDIR)$(bashdir)/stg.bash

.PHONY: install install-bash
