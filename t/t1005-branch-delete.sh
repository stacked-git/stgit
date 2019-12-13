#!/bin/sh

test_description='Attempt to delete branches'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    stg init &&
    test_commit p0 &&
    test_commit p1 &&
    stg uncommit -n 2
'

test_expect_success 'Create a branch (and switch to it)' '
    stg branch --clone foo
'

test_expect_success 'Attempt to delete branch with patches' '
    command_error stg branch --delete master 2>&1 |
    grep -e "series still contains patches"
'

test_expect_success 'Force delete branch with patches' '
    stg branch --delete --force master
'

test_expect_success 'Make sure the branch ref was deleted' '
    [ -z "$(git show-ref | grep master | tee /dev/stderr)" ]
    '

test_expect_success 'Make sure the branch config was deleted' '
    [ -z "$(git config -l | grep branch\\.master | tee /dev/stderr)" ]
    '

test_expect_success 'Make sure the branch files were deleted' '
    [ -z "$(find .git -type f | grep master | tee /dev/stderr)" ]
    '

test_expect_success 'Attempt to delete current branch' '
    command_error stg branch --delete $(stg branch) 2>&1 |
    grep -e "Cannot delete the current branch"
'

test_expect_success 'Invalid num args to delete' '
    command_error stg branch --delete 2>&1 |
    grep -e "incorrect number of arguments" &&
    command_error stg branch --delete foo extra 2>&1 |
    grep -e "incorrect number of arguments"
'

test_expect_success 'Create a non-StGIT branch and delete it' '
    git branch bar &&
    stg branch --delete bar
'

test_expect_success 'Delete a nonexistent branch' '
   command_error stg branch --delete bar 2>&1 |
   grep -e "no such branch"
'

test_done
