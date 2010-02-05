#!/bin/sh

test_description='Run "stg clean"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    stg init &&
    stg new e0 -m e0 &&
    stg new p0 -m p0 &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg new e1 -m e1 &&
    stg new e2 -m e2 &&
    stg pop
'

test_expect_success 'Clean empty patches' '
    [ "$(echo $(stg series --applied --noprefix))" = "e0 p0 e1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "e2" ] &&
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Create a conflict' '
    stg new p1 -m p1 &&
    echo bar > foo.txt &&
    stg refresh &&
    stg pop &&
    stg new p2 -m p2
    echo quux > foo.txt &&
    stg refresh &&
    conflict stg push
'

test_expect_success 'Make sure conflicting patches are preserved' '
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p2 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_done
