INSTALL ?= install

prefix ?= $(HOME)/.local
bashdir ?= $(prefix)/share/bash-completion/completions
fishdir ?= $(prefix)/share/fish/vendor_completions.d
zshdir ?= $(prefix)/share/zsh/site-functions

install: install-bash install-fish install-zsh

install-bash:
	${INSTALL} -d -m 755 $(DESTDIR)$(bashdir)
	${INSTALL} -T -m 644 stgit.bash $(DESTDIR)$(bashdir)/stg

install-fish:
	${INSTALL} -d -m 755 $(DESTDIR)$(fishdir)
	${INSTALL} -T -m 644 stg.fish $(DESTDIR)$(fishdir)/stg.fish

install-zsh:
	${INSTALL} -d -m 755 $(DESTDIR)$(zshdir)
	${INSTALL} -T -m 644 stgit.zsh $(DESTDIR)$(zshdir)/_stg

.PHONY: install install-bash install-fish install-zsh
