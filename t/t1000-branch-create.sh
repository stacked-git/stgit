#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Branch operations.

Exercises the "stg branch" commands.
'

. ./test-lib.sh

test_expect_success \
    'Create a branch when the current one is not an StGIT stack' '
    git branch origin &&
    stg branch --create new origin &&
    test "$(stg branch)" = "new"
'

test_expect_success \
    'Too many args to --create' '
    command_error stg branch --create aname acommit anextra
'

test_expect_success \
    'Create a spurious patches/ entry' '
    stg branch master &&
    stg init &&
    mkdir -p .git/patches && touch .git/patches/foo1
'

test_expect_success \
    'Try to create an stgit branch with a spurious patches/ entry' '
    command_error stg branch -c foo1
'

test_expect_success \
    'Check that no part of the branch was created' '
    test "$(find .git -name foo1 | tee /dev/stderr)" = ".git/patches/foo1" &&
    test $(git show-ref | grep foo1 | wc -l) -eq 0 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

test_expect_success \
    'Create a git branch' '
    git update-ref refs/heads/foo2 refs/heads/master
'

test_expect_success \
    'Try to create an stgit branch with an existing git branch by that name' '
    command_error stg branch -c foo2
'

test_expect_success \
    'Attempt switching to current branch' '
    command_error stg branch $(stg branch) 2>&1 |
    grep "is already the current branch"
'

test_expect_success \
    'Attempt no branch command' '
    command_error stg branch foo bar 2>&1 |
    grep "incorrect number of arguments"
'

test_expect_success \
    'Invalid num arguments to branch list' '
    command_error stg branch --list new 2>&1 |
    grep "incorrect number of arguments"
'

test_expect_success \
    'Check that no part of the branch was created' '
    test $(find .git -name foo2 | tee /dev/stderr \
        | grep -v ^\\.git/refs/heads/foo2$ \
        | grep -v ^\\.git/logs/refs/heads/foo2$ | wc -l) -eq 0 &&
    test $(git show-ref | grep foo2 | wc -l) -eq 1 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

test_expect_success \
    'Create an invalid refs/heads/ entry' '
    touch .git/refs/heads/foo3 &&
    command_error stg branch -c foo3
'

test_expect_failure \
    'Check that no part of the branch was created' '
    test $(find .git -name foo3 | tee /dev/stderr \
        | grep -v ^\\.git/refs/heads/foo3$ | wc -l) -eq 0 &&
    test $(git show-ref | grep foo3 | wc -l) -eq 0 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

# Workaround for the test failure to make the rest of the subtests
# succeed. (HEAD was erroneously overwritten with the bad foo3 ref, so
# we need to reset it.)
git symbolic-ref HEAD refs/heads/master

test_expect_success \
    'Setup two commits including removal of generated files' '
    git init &&
    touch file1 file2 &&
    stg add file1 file2 &&
    git commit -m 1 &&
    stg rm file1 file2 &&
    git commit -m 2 &&
    touch file2
'

test_expect_success \
    'Create branch down the stack, behind the conflict caused by the generated file' '
    command_error stg branch --create foo4 master^
'

test_expect_success \
    'Check the branch was not created' '
    test_path_is_missing file1 &&
    test $(find .git -name foo4 | tee /dev/stderr | wc -l) -eq 0 &&
    test $(git show-ref | grep foo4 | wc -l) -eq 0 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

test_done
