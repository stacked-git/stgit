#!/bin/sh
test_description='Test the refresh command from a subdirectory'
. ./test-lib.sh
stg init

test_expect_success 'Refresh from a subdirectory' '
    stg new foo -m foo &&
    echo foo >> foo.txt &&
    mkdir bar &&
    echo bar >> bar/bar.txt &&
    git add foo.txt bar/bar.txt &&
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
    [ "$(stg status)" = "M foo.txt" ]
'

test_expect_success 'Refresh whole subdirectory' '
    echo bar4 >> bar/bar.txt &&
    stg refresh bar &&
    [ "$(stg status)" = "M foo.txt" ]
'

test_expect_success 'Refresh subdirectories recursively' '
    echo bar5 >> bar/bar.txt &&
    stg refresh . &&
    [ "$(stg status)" = "" ]
'

test_done
