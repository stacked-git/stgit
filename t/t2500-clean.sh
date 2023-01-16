#!/bin/sh

test_description='Run "stg clean"'

. ./test-lib.sh

test_expect_success 'Clean uninitialized branch' '
    stg clean >out &&
    test_must_be_empty out
'

test_expect_success 'Initialize StGit stack' '
    stg new e0 -m e0 &&
    stg new p0 -m p0 &&
    echo foo >foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg new e1 -m e1 &&
    stg new e2 -m e2 &&
    stg pop
'

test_expect_success 'Test too many arguments' '
    general_error stg clean p0 2>err &&
    grep -e "unexpected argument .p0." err
'

test_expect_success 'Clean empty patches' '
    [ "$(echo $(stg series --applied --noprefix))" = "e0 p0 e1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "e2" ] &&
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Selectively clean applied and unapplied patches' '
    stg new e3 -m e3 &&
    stg new e4 -m e4 &&
    stg pop &&
    stg clean --applied &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "e4" ] &&
    stg clean --unapplied &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Ensure hidden patches are not cleaned' '
    stg new e5 -m e5 &&
    stg hide e5 &&
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --hidden --noprefix))" = "e5" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    stg unhide e5 &&
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --hidden --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Create a conflict' '
    stg new p1 -m p1 &&
    echo bar >foo.txt &&
    stg refresh &&
    stg pop &&
    stg new p2 -m p2
    echo quux >foo.txt &&
    stg refresh &&
    conflict stg push
'

test_expect_success 'Make sure conflicting patches are preserved' '
    stg clean &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p2 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_done
