#!/bin/sh
test_description='Test git/StGit interoperability with "stg repair"'
. ./test-lib.sh

test_expect_success 'Create some git-only history' '
    echo foo > foo.txt &&
    git add foo.txt &&
    git commit -a -m foo &&
    git tag foo-tag &&
    for i in 0 1 2 3 4; do
        echo foo$i >> foo.txt &&
        git commit -a -m foo$i;
    done
'

test_expect_success 'Initialize the StGit repository' '
    stg init
'

test_expect_success 'Create five patches' '
    for i in 0 1 2 3 4; do
        stg new p$i -m p$i;
    done &&
    [ "$(echo $(stg applied))" = "p0 p1 p2 p3 p4" ] &&
    [ "$(echo $(stg unapplied))" = "" ]
'

test_expect_success 'Pop two patches with git-reset' '
    git reset --hard HEAD~2 &&
    ! stg refresh &&
    stg repair &&
    stg refresh &&
    [ "$(echo $(stg applied))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg unapplied))" = "p3 p4" ]
'

test_expect_success 'Create a new patch' '
    stg new q0 -m q0 &&
    [ "$(echo $(stg applied))" = "p0 p1 p2 q0" ] &&
    [ "$(echo $(stg unapplied))" = "p3 p4" ]
'

test_expect_success 'Go to an unapplied patch with with git-reset' '
    git reset --hard $(stg id p3) &&
    ! stg refresh &&
    stg repair &&
    stg refresh &&
    [ "$(echo $(stg applied))" = "p0 p1 p2 p3" ] &&
    [ "$(echo $(stg unapplied))" = "q0 p4" ]
'

test_expect_success 'Go back to below the stack base with git-reset' '
    git reset --hard foo-tag &&
    stg repair &&
    [ "$(echo $(stg applied))" = "" ] &&
    [ "$(echo $(stg unapplied))" = "p0 p1 p2 p3 q0 p4" ]
'

test_done
