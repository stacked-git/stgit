#!/bin/sh
#
# Makes a release

set -e

#UPLOAD=false
UPLOAD=true

VERSION=$(./stg --version | grep "Stacked GIT" | sed -e "s/Stacked GIT //")
TARBALL=dist/stgit-$VERSION.tar.gz
BINRPM=dist/stgit-$VERSION-1.noarch.rpm
SRCRPM=dist/stgit-$VERSION-1.src.rpm
DEBPKG=../stgit_$VERSION-0_all.deb

git-rev-list --pretty HEAD > ChangeLog

rm -f MANIFEST

# source distribution
python setup.py sdist
gpg --detach-sign $TARBALL

# build the binary distributions
python setup.py bdist_rpm
dpkg-buildpackage -rfakeroot -b

# Upload
$UPLOAD && echo Uploading...

$UPLOAD && lftp ftp://your-ftp-site/stgit -u username,password -e " \
set ftp:list-empty-ok yes; \
echo Uploading $TARBALL; \
put $TARBALL; \
echo Uploading $BINRPM; \
put $BINRPM; \
echo Uploading $SRCRPM; \
put $SRCRPM; \
echo Uploading $DEBPKG; \
put $DEBPKG; \
exit"

$UPLOAD && scp $TARBALL $TARBALL.sig username@download.gna.org:/upload/stgit/
