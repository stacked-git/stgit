#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Branch cloning.

Exercises branch cloning options.
'

. ./test-lib.sh

test_expect_success \
    'Create a GIT commit' \
    '
    echo bar > bar.txt &&
    git add bar.txt &&
    git commit -a -m bar
    '

test_expect_success \
    'Try to create a patch in a GIT branch' \
    '
    ! stg new p0 -m "p0"
    '

test_expect_success \
    'Clone the current GIT branch' \
    '
    stg branch --clone foo &&
    stg new p1 -m "p1" &&
    test $(stg applied -c) -eq 1
    '

test_expect_success \
    'Clone the current StGIT branch' \
    '
    stg branch --clone bar &&
    test $(stg applied -c) -eq 1 &&
    stg new p2 -m "p2" &&
    test $(stg applied -c) -eq 2
    '

test_done
