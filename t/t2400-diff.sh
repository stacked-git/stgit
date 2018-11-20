#!/bin/sh

test_description='Run "stg diff"'

. ./test-lib.sh

test_expect_success 'Diff with no StGit data' '
    stg diff
'

test_expect_success 'Make some local changes' '
    echo foo >> foo.txt &&
    stg add foo.txt
'

test_expect_success 'Diff with some local changes' '
    stg diff
'

test_expect_success 'Diff with bad diff-opts' '
    command_error stg diff --diff-opts=--bad-diff-opt
'

test_expect_success 'Initialize StGit stuff' '
    stg init &&
    stg new foo -m foo
'

test_expect_success 'Diff with some local changes' '
    stg diff
'

test_expect_success 'Refresh patch' '
    stg refresh
'

test_expect_success 'Diff with no local changes' '
    stg diff
'

test_done
