#!/bin/sh

test_description='Attempt to delete branches'

. ./test-lib.sh

stg init

test_expect_success 'Create a branch (and switch to it)' '
    stg branch --create foo
    '

test_expect_success 'Delete a branch' '
    stg branch --delete master
    '

test_expect_success 'Attempt to delete current branch' '
    command_error stg branch --delete $(stg branch)
'

test_expect_success 'Invalid num args to delete' '
    command_error stg branch --delete &&
    command_error stg branch --delete foo extra
'

test_expect_success 'Create a non-StGIT branch and delete it' '
    git branch bar &&
    stg branch --delete bar
    '

test_expect_success 'Delete a nonexistent branch' '
   stg branch --delete bar
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

test_done
