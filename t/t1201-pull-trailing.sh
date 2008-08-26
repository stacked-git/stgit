#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='test

'

. ./test-lib.sh

# don't need this repo, but better not drop it, see t1100
#rm -rf .git

# Need a repo to clone
test_create_repo foo

test_expect_success \
    'Setup and clone tree, and setup changes' \
    "(cd foo &&
      printf 'a\nb\n' > file && git add file && git commit -m .
     ) &&
     stg clone foo bar &&
     (cd bar && stg new p1 -m p1
      printf 'c\n' >> file && stg refresh
     )
"

test_expect_success \
    'Port those patches to orig tree' \
    '(cd foo &&
      GIT_DIR=../bar/.git git format-patch --stdout \
          $(cd ../bar && stg id master:{base})..HEAD |
      git am -3 -k
     )
    '

test_expect_success \
    'Pull those patches applied upstream, without pushing' \
    "(cd bar && stg pull --nopush
     )
"

test_expect_success \
    'Try to push those patches without merge detection' \
    "(cd bar && stg push --all
     )
"

test_expect_success \
    'Pull those patches applied upstream' \
    "(cd bar && stg push --undo && stg push --all --merged
     )
"

test_expect_success \
    'Check that all went well' \
    "test_cmp foo/file bar/file
"

test_done
