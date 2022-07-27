CARGO ?= cargo
CARGO_RUN = $(CARGO) --quiet run
INSTALL ?= install

prefix ?= $(HOME)/.local
bashdir ?= $(prefix)/share/bash-completion/completions
fishdir ?= $(prefix)/share/fish/vendor_completions.d
zshdir ?= $(prefix)/share/zsh/site-functions

all: stgit.bash stg.fish

.PHONY: all

install: install-bash install-fish install-zsh

install-bash: stgit.bash
	$(INSTALL) -d -m 755 $(DESTDIR)$(bashdir)
	$(INSTALL) -T -m 644 stgit.bash $(DESTDIR)$(bashdir)/stg

install-fish: stg.fish
	$(INSTALL) -d -m 755 $(DESTDIR)$(fishdir)
	$(INSTALL) -T -m 644 stg.fish $(DESTDIR)$(fishdir)/stg.fish

install-zsh:
	$(INSTALL) -d -m 755 $(DESTDIR)$(zshdir)
	$(INSTALL) -T -m 644 stgit.zsh $(DESTDIR)$(zshdir)/_stg

.PHONY: install install-bash install-fish install-zsh

stgit.bash:
	$(CARGO_RUN) completion bash > $@

stg.fish:
	$(CARGO_RUN) completion fish > $@

clean:
	rm -f stgit.bash
	rm -f stg.fish

.PHONY: clean
