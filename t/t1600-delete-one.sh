#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the delete command (deleting one patch at a time).'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    'stg init'

test_expect_success \
    'Create a patch' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh
    '

test_expect_success \
    'Try to delete a non-existing patch' \
    '
    [ $(stg applied | wc -l) -eq 1 ] &&
    ! stg delete bar &&
    [ $(stg applied | wc -l) -eq 1 ]
    '

test_expect_success \
    'Try to delete the topmost patch while dirty' \
    '
    echo dirty >> foo.txt &&
    [ $(stg applied | wc -l) -eq 1 ] &&
    ! stg delete foo &&
    [ $(stg applied | wc -l) -eq 1 ] &&
    git reset --hard
    '

test_expect_success \
    'Delete the topmost patch' \
    '
    [ $(stg applied | wc -l) -eq 1 ] &&
    stg delete foo &&
    [ $(stg applied | wc -l) -eq 0 ]
    '

test_expect_success \
    'Create an unapplied patch' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg pop
    '

test_expect_success \
    'Delete an unapplied patch' \
    '
    [ $(stg unapplied | wc -l) -eq 1 ] &&
    stg delete foo &&
    [ $(stg unapplied | wc -l) -eq 0 ]
    '

test_expect_success \
    'Create two patches' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg new bar -m bar &&
    echo bar > bar.txt &&
    stg add bar.txt &&
    stg refresh
    '

test_expect_success \
    'Try to delete a non-topmost applied patch' \
    '
    [ $(stg applied | wc -l) -eq 2 ] &&
    ! stg delete foo &&
    [ $(stg applied | wc -l) -eq 2 ]
    '

test_expect_success \
    'Create another branch, and put one patch in each branch' \
    '
    stg branch --create br &&
    stg new baz -m baz &&
    echo baz > baz.txt &&
    stg add baz.txt &&
    stg refresh &&
    stg branch master &&
    stg new baz -m baz &&
    echo baz > baz.txt &&
    stg add baz.txt &&
    stg refresh
    '

test_expect_success \
    'Delete a patch in another branch' \
    '
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg applied -b br | wc -l) -eq 1 ] &&
    stg delete -b br baz &&
    [ $(stg applied | wc -l) -eq 3 ] &&
    [ $(stg applied -b br | wc -l) -eq 0 ]
    '

test_done
