prefix	?= $(HOME)/.local
DESTDIR	?=
DEFAULT_TEST_TARGET ?= test
STG_PROVE_OPTS ?=
STG_PROFILE ?= release
CARGO ?= cargo --locked
CARGO_OFFLINE = $(CARGO) --offline
CARGO_RUN = $(CARGO_OFFLINE) --quiet run --profile=$(STG_PROFILE)
RM ?= rm -f

ifeq ($(STG_PROFILE),dev)
TARGET_DIR = target/debug
else
TARGET_DIR = target/$(STG_PROFILE)
endif

export DESTDIR CARGO STG_PROFILE

TEST_PATCHES ?= ..

build:
	$(CARGO) build --profile=$(STG_PROFILE)

all: build doc completion contrib

completion: build
	$(MAKE) -C completion all

contrib:
	$(MAKE) -C contrib all

doc: build
	$(MAKE) -C Documentation all

.PHONY: all build completion contrib doc


install: install-bin

install-all: install-bin install-completion install-contrib install-man install-html

install-bin: build
	$(CARGO_OFFLINE) install --profile=$(STG_PROFILE) --path=. --root=$(DESTDIR)$(prefix) --no-track --force

install-completion: build
	$(MAKE) -C completion install

install-man:
	$(MAKE) -C Documentation install-man

install-html:
	$(MAKE) -C Documentation install-html

install-contrib:
	$(MAKE) -C contrib install

.PHONY: install install-all install-bin install-completion install-contrib install-man install-html


lint: lint-format lint-clippy lint-api-doc lint-t unit-test

lint-format:
	$(CARGO_OFFLINE) --quiet fmt --all --check

lint-clippy:
	$(CARGO_OFFLINE) --quiet clippy -- --deny warnings

lint-api-doc:
	$(CARGO_OFFLINE) --quiet doc --no-deps

lint-t:
	$(MAKE) -C t test-lint

unit-test:
	$(CARGO_OFFLINE) --quiet test

.PHONY: lint lint-format lint-clippy lint-api-doc lint-t unit-test


coverage:
	$(MAKE) coverage-test
	$(MAKE) coverage-html
	$(MAKE) coverage-report

coverage-bin:
	RUSTFLAGS="-C instrument-coverage" $(CARGO) build --profile=$(STG_PROFILE)

coverage-html: $(TARGET_DIR)/stg.profdata
	llvm-cov show \
	  --instr-profile=$< \
	  --object=$(TARGET_DIR)/stg \
	  -ignore-filename-regex='/.cargo/registry' \
	  -ignore-filename-regex='rustc/' \
	  -Xdemangler=rustfilt \
	  -format=html \
	  -output-dir=htmlcov

coverage-report: $(TARGET_DIR)/stg.profdata
	llvm-cov report \
	  --instr-profile=$< \
	  --object=$(TARGET_DIR)/stg \
	  -ignore-filename-regex='/.cargo/registry' \
	  -ignore-filename-regex='rustc/'

coverage-test: coverage-bin
	$(RM) $(TARGET_DIR)/stg.profdata
	$(MAKE) $(TARGET_DIR)/stg.profdata

$(TARGET_DIR)/stg.profdata:
	-$(RM) -r $(TARGET_DIR)/.profraw
	-mkdir $(TARGET_DIR)/.profraw
	LLVM_PROFILE_FILE=$(CURDIR)/$(TARGET_DIR)/.profraw/stg-%m-%p.profraw \
	$(MAKE) -C t all
	llvm-profdata merge -sparse -o $@ $(TARGET_DIR)/.profraw/*.profraw
	$(RM) -r $(TARGET_DIR)/.profraw

.PHONY: coverage coverage-bin coverage-html coverage-report coverage-test


format:
	$(CARGO_OFFLINE) fmt

test: build
	$(MAKE) -C t all

prove: build
	$(MAKE) -C t prove

test-patches:
	for patch in $$($(CARGO_RUN) series --noprefix $(TEST_PATCHES)); do \
		$(CARGO_RUN) goto $$patch && $(MAKE) test || break; \
	done

clean:
	$(MAKE) -C Documentation clean
	$(MAKE) -C t clean
	$(MAKE) -C completion clean
	$(MAKE) -C contrib clean
	$(RM) -r target
	$(RM) -r htmlcov

.PHONY: format prove test test-patches clean
