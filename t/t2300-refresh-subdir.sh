#!/bin/sh
test_description='Test the refresh command from a subdirectory'
. ./test-lib.sh
stg init

test_expect_success 'Refresh from a subdirectory' '
    stg new foo -m foo &&
    echo foo >> foo.txt &&
    mkdir bar &&
    echo bar >> bar/bar.txt &&
    stg add foo.txt bar/bar.txt &&
    cd bar &&
    stg refresh &&
    cd .. &&
    [ "$(stg status)" = "" ]
'

test_expect_failure 'Refresh again' '
    echo foo2 >> foo.txt &&
    echo bar2 >> bar/bar.txt &&
    cd bar &&
    stg refresh &&
    cd .. &&
    [ "$(stg status)" = "" ]
'

test_done
