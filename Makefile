prefix	?= $(HOME)
DESTDIR	?= /
PYTHON	?= python

export DESTDIR PYTHON

TEST_PATCHES ?= ..

all:
	$(PYTHON) setup.py build

install:
	$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) --force

doc:
	$(MAKE) -C Documentation all

install-doc:
	$(MAKE) -C Documentation install

install-html:
	$(MAKE) -C Documentation install-html

test:
	$(PYTHON) setup.py build
	$(MAKE) -C t all

test_patches:
	for patch in $$(stg series --noprefix $(TEST_PATCHES)); do \
		stg goto $$patch && $(MAKE) test || break; \
	done

coverage:
	$(PYTHON) -m coverage run setup.py build
	COVERAGE_PROCESS_START=$(PWD)/.coveragerc $(MAKE) -C t all
	$(PYTHON) -m coverage combine $$(find . -name '.coverage.*')
	$(PYTHON) -m coverage html --title="stgit coverage"
	$(PYTHON) -m coverage report
	@echo "HTML coverage report: file://$(PWD)/htmlcov/index.html"

clean:
	for dir in Documentation t; do \
		$(MAKE) -C $$dir clean; \
	done
	rm -rf build
	rm -f stgit/*.pyc
	rm -f stgit/commands/*.pyc
	rm -f TAGS
	rm -f stgit/commands/cmdlist.py

tags:
	ctags -R stgit/*

TAGS:
	ctags -e -R stgit/*

.PHONY: all install doc install-doc install-html test test_patches \
	coverage clean tags TAGS
