prefix	?= $(HOME)
DESTDIR	?= /
PYTHON	?= python

TEST_PATCHES ?= ..

all:
	$(PYTHON) setup.py build

install:
	$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) --force

doc:
	cd Documentation && $(MAKE) all

test:
	cd t && $(MAKE) all

test_patches:
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

tags:
	ctags -e -R stgit/*

.PHONY: all install doc test test_patches clean
