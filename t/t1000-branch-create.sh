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
    test $(stg branch) = "new"
'

test_expect_success \
    'Create a spurious patches/ entry' '
    stg branch master &&
    stg init &&
    find .git -name foo | xargs rm -rf &&
    mkdir -p .git/patches && touch .git/patches/foo
'

test_expect_success \
    'Try to create an stgit branch with a spurious patches/ entry' '
    ! stg branch -c foo
'

test_expect_success \
    'Check that no part of the branch was created' '
    test "`find .git -name foo | tee /dev/stderr`" = ".git/patches/foo" &&
    ( grep foo .git/HEAD; test $? = 1 )
'

test_expect_success \
    'Create a git branch' '
    find .git -name foo | xargs rm -rf &&
    cp .git/refs/heads/master .git/refs/heads/foo
'

test_expect_success \
    'Try to create an stgit branch with an existing git branch by that name' '
    ! stg branch -c foo
'

test_expect_success \
    'Check that no part of the branch was created' '
    test "`find .git -name foo | tee /dev/stderr`" = ".git/refs/heads/foo" &&
    ( grep foo .git/HEAD; test $? = 1 )
'

test_expect_success \
    'Create an invalid refs/heads/ entry' '
    find .git -name foo | xargs rm -rf &&
    touch .git/refs/heads/foo &&
    ! stg branch -c foo
'

test_expect_success \
    'Setup two commits including removal of generated files' '
    git init &&
    touch a.c a.o &&
    git add a.c a.o &&
    git commit -m 1 &&
    git rm a.c a.o &&
    git commit -m 2 &&
    touch a.o
'

test_expect_failure \
    'Create branch down the stack, behind the conflict caused by the generated file' '
    stg branch --create bar master^
'

test_expect_success \
    'Check the branch was not created' '
    test ! -r .git/refs/heads/bar &&
    test ! -r a.c
'

test_done
