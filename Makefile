prefix	?= $(HOME)/.local
DESTDIR	?= /
DEFAULT_TEST_TARGET ?= test
STG_PROVE_OPTS ?=
CARGO ?= cargo

export DESTDIR CARGO

TEST_PATCHES ?= ..

all: build

build:
	$(CARGO) build

install:
	$(CARGO) install --path=.

install-completion: build
	$(MAKE) -C completion install

.PHONY: all build dist install install-completion

doc: build
	$(MAKE) -C Documentation all

install-doc:
	$(MAKE) -C Documentation install

install-html:
	$(MAKE) -C Documentation install-html

.PHONY: doc install-doc install-html

lint: lint-format lint-clippy lint-t

lint-format:
	$(CARGO) fmt --all --check

lint-clippy:
	$(CARGO) clippy -- --deny warnings

lint-t:
	$(MAKE) -C t test-lint

.PHONY: lint lint-format lint-clippy lint-t

format:
	$(CARGO) fmt

test: build
	$(MAKE) -C t all

test-patches:
	for patch in $$($(CARGO) --quiet run series --noprefix $(TEST_PATCHES)); do \
		$(CARGO) --quiet run goto $$patch && $(MAKE) test || break; \
	done

.PHONY: format test test-patches

clean:
	$(MAKE) -C Documentation clean
	$(MAKE) -C t clean
	$(MAKE) -C completion clean
	rm  -r target
