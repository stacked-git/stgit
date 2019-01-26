#!/bin/sh

test_description='Branch cleanup'

. ./test-lib.sh

test_expect_success 'Initialize branch' '
    stg init &&
    stg branch --create foo &&
    echo "hello" > bar &&
    stg add bar &&
    stg new -m p0 &&
    stg refresh
'

test_expect_success 'Cannot cleanup with patches' '
    command_error stg branch --cleanup 2>&1 |
    grep "Cannot clean up: the series still contains patches"
'

test_expect_success 'Cannot cleanup with unapplied patches' '
    stg pop &&
    command_error stg branch --cleanup 2>&1 |
    grep "Cannot clean up: the series still contains patches"
'

test_expect_success 'Commit patches' '
    stg push -a &&
    stg commit -a
'

test_expect_success 'Invalid num args to cleanup' '
    command_error stg branch --cleanup foo extra 2>&1 |
    grep "incorrect number of arguments"
'

test_expect_success 'Cleanup current branch' '
    stg branch --cleanup &&
    [ $(stg branch) = "foo" ] &&
    command_error stg new -m p1 2>&1 |
    grep "branch not initialized"
'

test_expect_success 'Re-initialize branch' '
    stg init
'

test_expect_success 'Cleanup from another branch' '
    stg branch master &&
    stg branch --cleanup foo &&
    [ $(stg branch) = "master" ]
'

test_done
