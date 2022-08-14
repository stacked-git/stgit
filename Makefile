prefix	?= $(HOME)/.local
DESTDIR	?=
DEFAULT_TEST_TARGET ?= test
STG_PROVE_OPTS ?=
STG_PROFILE ?= release
CARGO ?= cargo --locked
CARGO_OFFLINE = $(CARGO) --offline
CARGO_RUN = $(CARGO_OFFLINE) --quiet run --profile=$(STG_PROFILE)

export DESTDIR CARGO STG_PROFILE

TEST_PATCHES ?= ..

all: build

build:
	$(CARGO) build --profile=$(STG_PROFILE)

doc: build
	$(MAKE) -C Documentation all

.PHONY: all build doc


install: install-bin

install-all: install-bin install-completion install-man install-html

install-bin: build
	$(CARGO_OFFLINE) install --profile=$(STG_PROFILE) --path=. --root=$(DESTDIR)$(prefix)

install-completion: build
	$(MAKE) -C completion install

install-man:
	$(MAKE) -C Documentation install-man

install-html:
	$(MAKE) -C Documentation install-html

.PHONY: install install-all install-bin install-completion install-man install-html


lint: lint-format lint-clippy lint-t unit-test

lint-format:
	$(CARGO_OFFLINE) --quiet fmt --all --check

lint-clippy:
	$(CARGO_OFFLINE) --quiet clippy -- --deny warnings

lint-t:
	$(MAKE) -C t test-lint

unit-test:
	$(CARGO_OFFLINE) --quiet test

.PHONY: lint lint-format lint-clippy lint-t unit-test


format:
	$(CARGO_OFFLINE) fmt

test: build
	$(MAKE) -C t all

test-patches:
	for patch in $$($(CARGO_RUN) series --noprefix $(TEST_PATCHES)); do \
		$(CARGO_RUN) goto $$patch && $(MAKE) test || break; \
	done

clean:
	$(MAKE) -C Documentation clean
	$(MAKE) -C t clean
	$(MAKE) -C completion clean
	rm  -r target

.PHONY: format test test-patches clean
