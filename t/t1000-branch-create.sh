#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Branch operations.

Exercises the "stg branch" commands.
'

. ./test-lib.sh

stg init

test_expect_success \
    'Create a spurious patches/ entry' '
    find .git -name foo | xargs rm -rf &&
    mkdir -p .git/patches && touch .git/patches/foo
'

test_expect_failure \
    'Try to create an stgit branch with a spurious patches/ entry' '
    stg branch -c foo
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

test_expect_failure \
    'Try to create an stgit branch with an existing git branch by that name' '
    stg branch -c foo
'

test_expect_success \
    'Check that no part of the branch was created' '
    test "`find .git -name foo | tee /dev/stderr`" = ".git/refs/heads/foo" &&
    ( grep foo .git/HEAD; test $? = 1 )
'

test_expect_success \
    'Create an invalid refs/heads/ entry' '
    find .git -name foo | xargs rm -rf &&
    touch .git/refs/heads/foo
'

test_done
