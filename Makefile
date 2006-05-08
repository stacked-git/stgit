PREFIX = $(HOME)
DESTDIR = /
PYTHON = python

all:
	$(PYTHON) setup.py build

install:
	$(PYTHON) setup.py install --prefix=$(PREFIX) --root=$(DESTDIR)

doc:
	cd doc && $(MAKE) all

test:
	cd t && $(MAKE) all

clean:
	for dir in doc t; do \
		(cd $$dir && $(MAKE) clean); \
	done
	rm -rf build
	rm -f stgit/*.pyc
	rm -f stgit/commands/*.pyc
