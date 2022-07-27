prefix	?= $(HOME)/.local
DESTDIR	?=
DEFAULT_TEST_TARGET ?= test
STG_PROVE_OPTS ?=
STG_PROFILE ?= release
CARGO ?= cargo
CARGO_RUN = $(CARGO) --quiet run --profile=$(STG_PROFILE)

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

install-bin:
	$(CARGO) install --profile=$(STG_PROFILE) --locked --path=. --root=$(DESTDIR)$(prefix)

install-completion: build
	$(MAKE) -C completion install

install-man:
	$(MAKE) -C Documentation install-man

install-html:
	$(MAKE) -C Documentation install-html

.PHONY: install install-all install-bin install-completion install-man install-html


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
	for patch in $$($(CARGO_RUN) series --noprefix $(TEST_PATCHES)); do \
		$(CARGO_RUN) goto $$patch && $(MAKE) test || break; \
	done

clean:
	$(MAKE) -C Documentation clean
	$(MAKE) -C t clean
	$(MAKE) -C completion clean
	rm  -r target

.PHONY: format test test-patches clean
