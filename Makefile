PREFIX	?= $(HOME)
DESTDIR	?= /
PYTHON	?= python

all:
	$(PYTHON) setup.py build

install:
	$(PYTHON) setup.py install --prefix=$(PREFIX) --root=$(DESTDIR)

doc:
	cd Documentation && $(MAKE) all

test:
	cd t && $(MAKE) all

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

.PHONY: all install doc test clean
