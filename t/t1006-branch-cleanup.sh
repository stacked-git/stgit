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

if test -z "$STG_RUST"; then
test_expect_success 'Cannot cleanup with patches' '
    command_error stg branch --cleanup 2>err &&
    grep "Cannot clean up: the series still contains patches" err
'
else
test_expect_success 'Cannot cleanup with patches' '
    command_error stg branch --cleanup 2>err &&
      cat err &&
    grep "Clean up not permitted: the series still contains patches" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Cannot cleanup with unapplied patches' '
    stg pop &&
    command_error stg branch --cleanup 2>err &&
    grep "Cannot clean up: the series still contains patches" err
'
else
test_expect_success 'Cannot cleanup with unapplied patches' '
    stg pop &&
    command_error stg branch --cleanup 2>err &&
    grep "Clean up not permitted: the series still contains patches" err
'
fi

test_expect_success 'Clone branch with patches' '
    stg branch --clone foo2 &&
    test "$(stg branch)" = "foo2" &&
    test "$(stg series --noprefix --unapplied)" = "p0"
'

if test -z "$STG_RUST"; then
test_expect_success 'Force cleanup branch with patches' '
    git config --get-regexp branch\\.foo2\\.stgit &&
    stg branch --cleanup --force &&
    test "$(stg series --noprefix --all)" = "" &&
    command_error stg new -m p1 2>err &&
    grep "branch not initialized" err &&
    test_expect_code 1 git config --get-regexp branch\\.foo2\\.stgit
'
else
test_expect_success 'Force cleanup branch with patches' '
    git config --get-regexp branch\\.foo2\\.stgit &&
    stg branch --cleanup --force &&
    test "$(stg series --noprefix --all)" = "" &&
    command_error stg new -m p1 2>err &&
    grep "Branch \`foo2\` not initialized" err &&
    test_expect_code 1 git config --get-regexp branch\\.foo2\\.stgit
'
fi

test_expect_success 'Commit patches' '
    stg branch foo &&
    stg push -a &&
    stg commit -a
'

if test -z "$STG_RUST"; then
test_expect_success 'Invalid num args to cleanup' '
    command_error stg branch --cleanup foo extra 2>err &&
    grep "incorrect number of arguments" err
'
else
test_expect_success 'Invalid num args to cleanup' '
    general_error stg branch --cleanup foo extra 2>err &&
    grep "Found argument .extra. which wasn.t expected" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Cleanup current branch' '
    stg branch --cleanup &&
    test "$(stg branch)" = "foo" &&
    command_error stg new -m p1 2>err &&
    grep "branch not initialized" err
'
else
test_expect_success 'Cleanup current branch' '
    stg branch --cleanup &&
    test "$(stg branch)" = "foo" &&
    command_error stg new -m p1 2>err &&
    grep "Branch \`foo\` not initialized" err
'
fi

test_expect_success 'Re-initialize branch' '
    stg init
'

test_expect_success 'Cleanup from another branch' '
    stg branch master &&
    stg branch --cleanup foo &&
    test "$(stg branch)" = "master"
'

test_done
