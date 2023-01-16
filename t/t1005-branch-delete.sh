#!/bin/sh

test_description='Attempt to delete branches'

. ./test-lib.sh

test_expect_success 'Initialize master branch' '
    stg init &&
    test_commit p0 &&
    test_commit p1 &&
    stg uncommit -n 2 &&
    git config branch.master.stgit.autostash true
'

test_expect_success 'Create a branch (and switch to it)' '
    stg branch --clone foo
'

test_expect_success 'Attempt to delete branch with patches' '
    command_error stg branch --delete master 2>err &&
    grep -e "delete not permitted: the series still contains patches" err
'

test_expect_success 'Force delete branch with patches' '
    stg branch --delete --force master
'

test_expect_success 'Make sure the branch ref was deleted' '
    [ -z "$(git show-ref | grep master | tee /dev/stderr)" ]
'

test_expect_success 'Make sure the branch config was deleted' '
    [ -z "$(git config -l | grep branch\\.master | tee /dev/stderr)" ] &&
    [ -z "$(git config -l | grep branch\\.master\\.stgit | tee /dev/stderr)" ] &&
    test_expect_code 128 git config --remove-section branch.master.stgit
'

test_expect_success 'Make sure the branch files were deleted' '
    [ -z "$(find .git -type f | grep master | tee /dev/stderr)" ]
'

test_expect_success 'Attempt to delete current branch' '
    command_error stg branch --delete $(stg branch) 2>err &&
    grep -e "cannot delete the current branch" err
'

test_expect_success 'Invalid num args to delete' '
    general_error stg branch --delete 2>err &&
    grep -e "the following required arguments were not provided" err &&
    general_error stg branch --delete foo extra 2>err &&
    grep -e "unexpected argument .extra." err
'

test_expect_success 'Create a non-StGit branch and delete it' '
    git branch bar &&
    stg branch --delete bar
'

test_expect_success 'Delete a nonexistent branch' '
   command_error stg branch --delete bar 2>err &&
   grep -e "branch \`bar\` not found" err
'

test_done
