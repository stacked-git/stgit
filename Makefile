prefix	?= $(HOME)
DESTDIR	?= /
PYTHON	?= python

TEST_PATCHES ?= ..

all: build
	$(PYTHON) setup.py build

build: stgit/commands/cmdlist.py stgit-completion.bash

ALL_PY = $(shell find stgit -name '*.py')

stgit/commands/cmdlist.py: $(ALL_PY)
	$(PYTHON) stg-build --py-cmd-list > $@

stgit-completion.bash: $(ALL_PY)
	$(PYTHON) stg-build --bash-completion > $@

install: build
	$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) --force

doc:
	cd Documentation && $(MAKE) all

install-doc:
	$(MAKE) -C Documentation install

install-html:
	$(MAKE) -C Documentation install-html

test: build
	cd t && $(MAKE) all

test_patches: build
	for patch in $$(stg series --noprefix $(TEST_PATCHES)); do \
		stg goto $$patch && $(MAKE) test || break; \
	done

clean:
	for dir in Documentation t; do \
		(cd $$dir && $(MAKE) clean); \
	done
	rm -rf build
	rm -f stgit/*.pyc
	rm -f stgit/commands/*.pyc
	rm -f TAGS
	rm -f stgit/commands/cmdlist.py

tags:
	ctags -e -R stgit/*

.PHONY: all build install doc install-doc install-html test test_patches clean
