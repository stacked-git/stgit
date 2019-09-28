prefix	?= $(HOME)
DESTDIR	?= /
PYTHON	?= python

export DESTDIR PYTHON

TEST_PATCHES ?= ..

all: build

build:
	$(PYTHON) setup.py build

install:
	$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) --force

.PHONY: all build install

doc:
	$(MAKE) -C Documentation all

install-doc:
	$(MAKE) -C Documentation install

install-html:
	$(MAKE) -C Documentation install-html

.PHONY: doc install-doc install-html

lint:
	$(PYTHON) -m flake8 . stg stg-build stg-dbg stg-prof
	$(PYTHON) -m isort --check-only --quiet --recursive stgit stg stg-build stg-dbg stg-prof t/test.py

test: build
	$(MAKE) -C t all

test-patches:
	for patch in $$(stg series --noprefix $(TEST_PATCHES)); do \
		stg goto $$patch && $(MAKE) test || break; \
	done

.PHONY: lint test test-patches

coverage:
	$(MAKE) coverage-test
	$(MAKE) coverage-report

coverage-test:
	rm -f .coverage
	$(MAKE) .coverage

.coverage:
	rm -rf build
	-mkdir .cov-files
	COVERAGE_FILE=$(PWD)/.cov-files/.coverage \
	$(PYTHON) -m coverage run setup.py build
	COVERAGE_PROCESS_START=$(PWD)/.coveragerc \
	COVERAGE_FILE=$(PWD)/.cov-files/.coverage \
	$(MAKE) -C t all
	COVERAGE_PROCESS_START=$(PWD)/.coveragerc \
	COVERAGE_FILE=$(PWD)/.cov-files/.coverage \
	$(MAKE) -C Documentation build-txt
	$(PYTHON) -m coverage combine .cov-files/.coverage.*
	rm -r .cov-files

coverage-report: .coverage
	$(PYTHON) -m coverage html --title="stgit coverage"
	$(PYTHON) -m coverage report
	@echo "HTML coverage report: file://$(PWD)/htmlcov/index.html"

.PHONY: coverage coverage-test coverage-report

clean:
	for dir in Documentation t; do \
		$(MAKE) -C $$dir clean; \
	done
	rm -rf build
	rm -rf dist
	rm  -f stgit/*.pyc
	rm -rf stgit/__pycache__
	rm  -f stgit/builtin_version.py
	rm  -f stgit/commands/*.pyc
	rm -rf stgit/commands/__pycache__
	rm  -f stgit/commands/cmdlist.py
	rm  -f stgit/completion/*.pyc
	rm -rf stgit/completion/__pycache__
	rm  -f stgit/lib/*.pyc
	rm -rf stgit/lib/__pycache__
	rm  -f stgit/lib/git/*.pyc
	rm -rf stgit/lib/git/__pycache__
	rm  -f TAGS tags
	rm  -f MANIFEST
	rm  -f completion/stg.fish
	rm  -f completion/stgit.bash
	rm  -f .coverage
	rm -rf .cov-files
	rm -rf htmlcov

tags:
	ctags -R stgit/*

TAGS:
	ctags -e -R stgit/*

.PHONY: clean tags TAGS
