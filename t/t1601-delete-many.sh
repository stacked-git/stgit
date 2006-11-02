#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the delete command (deleting many patches at once).'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    'stg init'

test_expect_success \
    'Create five applied and five unapplied patches' \
    '
    stg new foo0 -m foo0 &&
    echo foo0 > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    for i in 1 2 3 4 5 6 7 8 9; do
        stg new foo$i -m foo$i &&
        echo foo$i >> foo.txt &&
        stg refresh;
    done &&
    stg pop -n 5
    '

test_expect_success \
    'Delete some patches' \
    '
    [ $(stg applied | wc -l) -eq 5 ] &&
    [ $(stg unapplied | wc -l) -eq 5 ] &&
    stg delete foo7 foo6 foo3 foo4 &&
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg unapplied | wc -l) -eq 3 ]
    '

test_expect_success \
    'Delete some more patches, some of which do not exist' \
    '
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg unapplied | wc -l) -eq 3 ] &&
    ! stg delete foo7 foo8 foo2 foo0 &&
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg unapplied | wc -l) -eq 3 ]
    '

test_expect_success \
    'Delete a range of patches' \
    '
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg unapplied | wc -l) -eq 3 ] &&
    stg delete foo1..foo8 &&
    [ $(stg applied | wc -l) -eq 1 ] &&
    [ $(stg unapplied | wc -l) -eq 1 ]
    '

test_done
