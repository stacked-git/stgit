#!/bin/sh

test_description='Test branch protection'

. ./test-lib.sh

test_expect_success 'Initialize branch' '
    stg init &&
    stg branch --create foo &&
    echo "hello" > bar &&
    stg add bar &&
    stg new -m p0 &&
    stg refresh
'

test_expect_success 'Invald num args to protect' '
    command_error stg branch --protect foo bar 2>&1 |
    grep "incorrect number of arguments"
'

test_expect_success 'Protect branch' '
    stg branch --protect
'

test_expect_success 'List protected' '
    stg branch --list |
    grep -E "> sp.*\bfoo"
'

test_expect_success 'Protect idempotency' '
    stg branch --protect foo
'

test_expect_success 'Attempt cleanup protected' '
    command_error stg branch --cleanup 2>&1 |
    grep "This branch is protected"
'

test_expect_success 'Attempt delete protected' '
    stg branch master &&
    command_error stg branch --delete foo 2>&1 |
    grep "This branch is protected"
'

test_expect_success 'Invalid num arts to unprotect' '
    command_error stg branch --unprotect foo bar 2>&1 |
    grep "incorrect number of arguments"
'

test_expect_success 'Unprotect branch' '
    stg branch --unprotect foo
'

test_expect_success 'List unprotected' '
    stg branch --list |
    grep -E "  s .*\bfoo"
'

test_expect_success 'Unprotect idempotency' '
    stg branch foo &&
    stg branch --unprotect &&
    stg branch --list |
    grep -E "> s .*\bfoo"
'

test_expect_success 'Cleanup unprotected' '
    stg commit -a &&
    stg branch --cleanup
'

test_expect_success 'Protect uninitialized branch' '
    command_error stg branch --protect 2>&1 |
    grep -E "is not controlled by StGIT"
'

test_expect_success 'Protect uninitialized branch' '
    command_error stg branch --unprotect 2>&1 |
    grep -E "is not controlled by StGIT"
'

test_expect_success 'Delete unprotected' '
    stg branch master &&
    stg branch --delete foo
'

test_done
