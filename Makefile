prefix	?= $(HOME)/.local
DESTDIR	?= /
PYTHON	?= python3
DEFAULT_TEST_TARGET ?= test
STG_PROVE_OPTS ?=

export DESTDIR PYTHON

TEST_PATCHES ?= ..

all: build

build:
	$(PYTHON) setup.py build

dist:
	$(PYTHON) setup.py sdist

install:
	$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) --force

.PHONY: all build dist install

doc:
	$(MAKE) -C Documentation all

install-doc:
	$(MAKE) -C Documentation install

install-html:
	$(MAKE) -C Documentation install-html

.PHONY: doc install-doc install-html

lint: lint-black lint-isort lint-flake8 lint-t

lint-black:
	$(PYTHON) -m black --check --quiet --diff .

lint-isort:
	$(PYTHON) -m isort --check-only --quiet --diff .

lint-flake8:
	$(PYTHON) -m flake8 .

lint-t:
	$(MAKE) -C t test-lint

.PHONY: lint lint-black lint-isort lint-flake8 lint-t

format:
	$(PYTHON) -m black .
	$(PYTHON) -m isort --quiet .

test: build
	$(MAKE) -C t all

test-patches:
	for patch in $$(t/stg series --noprefix $(TEST_PATCHES)); do \
		t/stg goto $$patch && $(MAKE) test || break; \
	done

.PHONY: format test test-patches .install

coverage:
	$(MAKE) coverage-test
	$(MAKE) coverage-report

coverage-test:
	rm -f .coverage
	$(MAKE) .coverage

.coverage:
	rm -rf install
	$(MAKE) build
	-mkdir .cov-files
	COVERAGE_FILE=$(CURDIR)/.cov-files/.coverage \
	$(PYTHON) -m coverage run --context=setup setup.py install
	COVERAGE_PROCESS_START=$(CURDIR)/pyproject.toml \
	COVERAGE_FILE=$(CURDIR)/.cov-files/.coverage \
	$(MAKE) -C t all
	COVERAGE_PROCESS_START=$(CURDIR)/pyproject.toml \
	COVERAGE_FILE=$(CURDIR)/.cov-files/.coverage \
	$(MAKE) -C Documentation build-txt
	$(PYTHON) -m coverage combine .cov-files/.coverage.*
	rm -r .cov-files

coverage-report: .coverage
	$(PYTHON) -m coverage html
	$(PYTHON) -m coverage report
	@echo "HTML coverage report: file://$(CURDIR)/htmlcov/index.html"

.PHONY: coverage coverage-test coverage-report

dev-env:
	$(PYTHON) -m pip install -U pip
	$(PYTHON) -m pip install coverage[toml] black flake8 flake8-bugbear isort
.PHONY: dev-env

clean:
	for dir in Documentation t; do \
		$(MAKE) -C $$dir clean; \
	done
	rm -rf build
	rm -rf inst
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
