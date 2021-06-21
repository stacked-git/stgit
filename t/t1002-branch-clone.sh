#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Branch cloning.

Exercises branch cloning options.
'

. ./test-lib.sh

test_expect_success \
    'Create a Git commit' \
    '
    echo bar > bar.txt &&
    stg add bar.txt &&
    git commit -a -m bar
    '

test_expect_success \
    'Try to create a patch in a Git branch' \
    '
    command_error stg new p0 -m "p0"
    '

test_expect_success \
    'Clone the current Git branch' \
    '
    stg branch --clone foo &&
    stg new p1 -m "p1" &&
    test "$(stg series --applied -c)" -eq 1
    '

test_expect_success \
    'Clone the current StGit branch' \
    '
    stg branch --clone bar &&
    test "$(stg series --applied -c)" -eq 1 &&
    test "$(git config --get branch.bar.description)" = "clone of \"foo\"" &&
    stg new p2 -m "p2" &&
    test "$(stg series --applied -c)" -eq 2
    '

test_expect_success \
    'Anonymous clone' \
    '
    stg branch --clone &&
    stg branch |
    grep -E "^bar-[0-9]+-[0-9]+"
    '

test_expect_success \
    'Invalid clone args' \
    '
    command_error stg branch --clone bname extra
    '

test_done
