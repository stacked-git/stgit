CARGO ?= cargo --locked
RM ?= rm -f

STGIT_VERSION := $(shell $(CARGO) metadata --format-version=1 --no-deps | \
                         grep -Eo '"version":"[^"]+"' | \
                         sed -Ee 's/"version":"([^"]+)"/\1/g')

msi: stgit.wxs release-build
	wix build -arch x64 -src $< -ext WixToolset.UI.wixext -out stgit-$(STGIT_VERSION).msi -define StGitVersion=$(STGIT_VERSION) -bindpath ..\..

release-build:
	$(MAKE) -C ../.. STG_PROFILE=release build completion

.PHONY: release-build


clean:
	$(RM) stgit-*.msi
	$(RM) stgit-*.wixpdb

.PHONY: clean
