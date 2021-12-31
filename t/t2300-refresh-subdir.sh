#!/bin/sh
test_description='Test the refresh command from a subdirectory'
. ./test-lib.sh
stg init

test_expect_success 'Refresh from a subdirectory' '
    stg new p0 -m p0 &&
    echo foo >> foo.txt &&
    mkdir bar &&
    echo bar >> bar/bar.txt &&
    stg add foo.txt bar/bar.txt &&
    cd bar &&
    stg refresh &&
    cd .. &&
    [ "$(stg status)" = "" ]
'

test_expect_success 'Refresh again' '
    echo foo2 >> foo.txt &&
    echo bar2 >> bar/bar.txt &&
    cd bar &&
    stg refresh &&
    cd .. &&
    [ "$(stg status)" = "" ]
'

test_expect_success 'Refresh file in subdirectory' '
    echo foo3 >> foo.txt &&
    echo bar3 >> bar/bar.txt &&
    cd bar &&
    stg refresh bar.txt &&
    cd .. &&
    [ "$(stg status)" = " M foo.txt" ]
'

test_expect_success 'Refresh whole subdirectory' '
    echo bar4 >> bar/bar.txt &&
    stg refresh bar &&
    [ "$(stg status)" = " M foo.txt" ]
'

test_expect_success 'Refresh subdirectories recursively' '
    echo bar5 >> bar/bar.txt &&
    stg refresh . &&
    [ "$(stg status)" = "" ]
'

test_expect_success 'refresh -u' '
    echo baz >> bar/baz.txt &&
    stg new p1 -m p1 &&
    stg add bar/baz.txt &&
    stg refresh --index &&
    echo xyzzy >> foo.txt &&
    echo xyzzy >> bar/bar.txt &&
    echo xyzzy >> bar/baz.txt &&
    stg refresh -u &&
    test "$(echo $(stg status))" = "M bar/bar.txt M foo.txt" &&
    test "$(echo $(stg files p0))" = "A bar/bar.txt A foo.txt" &&
    test "$(echo $(stg files p1))" = "A bar/baz.txt"
'

test_expect_success 'refresh -u -p <subdir>' '
    echo xyzzy >> bar/baz.txt &&
    stg refresh -p p0 -u bar &&
    test "$(echo $(stg status))" = "M bar/baz.txt M foo.txt" &&
    test "$(echo $(stg files p0))" = "A bar/bar.txt A foo.txt" &&
    test "$(echo $(stg files p1))" = "A bar/baz.txt"
'

test_expect_success 'refresh an unapplied patch' '
    stg refresh -u &&
    stg goto --keep p0 &&
    test "$(stg status)" = " M foo.txt" &&
    stg refresh -p p1 &&
    test "$(stg status)" = "" &&
    test "$(echo $(stg files p1))" = "A bar/baz.txt M foo.txt"
'

test_done
