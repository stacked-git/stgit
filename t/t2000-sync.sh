#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Test the sync command.'

. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    stg init
    '

test_expect_success \
    'Create some patches' \
    '
    stg new p1 -m p1 &&
    echo foo1 > foo1.txt &&
    stg add foo1.txt &&
    stg refresh &&
    stg new p2 -m p2 &&
    echo foo2 > foo2.txt &&
    stg add foo2.txt &&
    stg refresh &&
    stg new p3 -m p3 &&
    echo foo3 > foo3.txt &&
    stg add foo3.txt &&
    stg refresh &&
    stg export &&
    stg pop
    '

test_expect_success \
    'Create a branch with empty patches' \
    '
    stg branch -c foo base &&
    stg new p1 -m p1 &&
    stg new p2 -m p2 &&
    stg new p3 -m p3
    test $(stg applied -c) -eq 3
    '

test_expect_success \
    'Synchronise second patch with the master branch' \
    '
    stg sync -b master p2 &&
    test $(stg applied -c) -eq 3 &&
    test $(cat foo2.txt) = "foo2"
    '

test_expect_success \
    'Synchronise the first two patches with the master branch' \
    '
    stg sync -b master -a &&
    test $(stg applied -c) -eq 3 &&
    test $(cat foo1.txt) = "foo1" &&
    test $(cat foo2.txt) = "foo2"
    '

test_expect_success \
    'Synchronise all the patches with the exported series' \
    '
    stg sync -s patches-master/series -a &&
    test $(stg applied -c) -eq 3 &&
    test $(cat foo1.txt) = "foo1" &&
    test $(cat foo2.txt) = "foo2" &&
    test $(cat foo3.txt) = "foo3"
    '

test_expect_success \
    'Modify the master patches' \
    '
    stg branch master &&
    stg goto p1 &&
    echo bar1 >> foo1.txt &&
    stg refresh &&
    stg goto p2 &&
    echo bar2 > bar2.txt &&
    stg add bar2.txt &&
    stg refresh &&
    stg goto p3 &&
    echo bar3 >> foo3.txt &&
    stg refresh &&
    stg export &&
    stg branch foo
    '

test_expect_success \
    'Synchronise second patch with the master branch' \
    '
    stg sync -b master p2 &&
    test $(stg applied -c) -eq 3 &&
    test $(cat bar2.txt) = "bar2"
    '

test_expect_failure \
    'Synchronise the first two patches with the master branch (to fail)' \
    '
    stg sync -b master -a
    '

test_expect_success \
    'Restore the stack status after the failed sync' \
    '
    test $(stg applied -c) -eq 1 &&
    stg resolved -a &&
    stg refresh &&
    stg goto p3
    '

test_expect_failure \
    'Synchronise the third patch with the exported series (to fail)' \
    '
    stg sync -s patches-master/series p3
    '

test_expect_success \
    'Restore the stack status after the failed sync' \
    '
    test $(stg applied -c) -eq 3 &&
    stg resolved -a &&
    stg refresh
    '

test_done
