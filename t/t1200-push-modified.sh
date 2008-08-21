#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Exercise pushing patches applied upstream.

Especially, consider the case of a patch that adds a file, while a
subsequent one modifies it, so we have to use --merged for push to
detect the merge.  Reproduce the common workflow where one does not
specify --merged, then rollback and retry with the correct flag.'

. ./test-lib.sh

# don't need this repo, but better not drop it, see t1100
#rm -rf .git

# Need a repo to clone
test_create_repo foo

test_expect_success \
    'Clone tree and setup changes' '
    stg clone foo bar &&
    (
        cd bar && stg new p1 -m p1 &&
        printf "a\nc\n" > file && git add file && stg refresh &&
        stg new p2 -m p2 &&
        printf "a\nb\nc\n" > file && stg refresh &&
        [ "$(echo $(stg series --applied --noprefix))" = "p1 p2" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
    )
'

test_expect_success \
    'Port those patches to orig tree' '
    (
        cd foo &&
        GIT_DIR=../bar/.git git-format-patch --stdout \
          $(cd ../bar && stg id master:{base})..HEAD | git-am -3 -k
    )
'

test_expect_success \
    'Pull to sync with parent, preparing for the problem' \
    "(cd bar && stg pop --all &&
      stg pull
     )
"

test_expect_success \
    'Attempt to push the first of those patches without --merged' \
    "(cd bar && conflict_old stg push
     )
"

test_expect_success \
    'Rollback the push' '
    (
        cd bar && stg push --undo &&
        [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ]
    )
'

test_expect_success \
    'Push those patches while checking they were merged upstream' '
    (
        cd bar && stg push --merged --all
        [ "$(echo $(stg series --applied --noprefix))" = "p1 p2" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
    )
'

test_done
