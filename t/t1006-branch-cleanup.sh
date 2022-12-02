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
    command_error stg branch --cleanup 2>err &&
    grep "Clean up not permitted: the series still contains patches" err
'

test_expect_success 'Cannot cleanup with unapplied patches' '
    stg pop &&
    command_error stg branch --cleanup 2>err &&
    grep "Clean up not permitted: the series still contains patches" err
'

test_expect_success 'Clone branch with patches' '
    stg branch --clone foo2 &&
    test "$(stg branch)" = "foo2" &&
    test "$(stg series --noprefix --unapplied)" = "p0"
'

test_expect_success 'Force cleanup branch with patches' '
    git config --get-regexp branch\\.foo2\\.stgit &&
    stg branch --cleanup --force &&
    test "$(stg series --noprefix --all)" = "" &&
    command_error stg branch --cleanup 2>err &&
    grep "StGit stack not initialized for branch \`foo2\`" err &&
    test_expect_code 1 git config --get-regexp branch\\.foo2\\.stgit &&
    test_expect_code 128 git config --remove-section branch.foo2.stgit
'

test_expect_success 'Commit patches' '
    stg branch foo &&
    stg push -a &&
    stg commit -a
'

test_expect_success 'Invalid num args to cleanup' '
    general_error stg branch --cleanup foo extra 2>err &&
    grep "Found argument .extra. which wasn.t expected" err
'

test_expect_success 'Cleanup current branch' '
    stg branch --cleanup &&
    test "$(stg branch)" = "foo" &&
    command_error stg branch --cleanup 2>err &&
    grep "StGit stack not initialized for branch \`foo\`" err
'

test_expect_success 'Re-initialize branch' '
    stg init
'

test_expect_success 'Cleanup from another branch' '
    stg branch master &&
    stg branch --cleanup foo &&
    test "$(stg branch)" = "master"
'

test_done
