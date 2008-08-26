#!/bin/sh
#
# Builds a daily snapshot

VERSION=`date "+%Y%m%d"`
SNAPSHOT=dist/stgit-$VERSION.tar.gz

git rev-list --pretty HEAD > ChangeLog

mv stgit/version.py stgit/version.py-
echo "version = '$VERSION'" > stgit/version.py

rm -f MANIFEST
python setup.py sdist

rm stgit/version.py
mv stgit/version.py- stgit/version.py

# Upload
lftp ftp://your-ftp-site/stgit/snapshots -u username,password -e " \
set ftp:list-empty-ok yes; \
echo Uploading $SNAPSHOT; \
put $SNAPSHOT; \
exit"
