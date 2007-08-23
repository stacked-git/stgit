#!/bin/sh
#
# Copyright (c) 2007 Yann Dirson
#

test_description='Excercise pull-policy "rebase".'

. ./test-lib.sh

test_expect_success \
    'Fork stack off parent branch, and add patches to the stack' \
    '
    git branch -m master parent &&
    stg init &&
    stg branch --create stack &&
    git repo-config branch.stack.stgit.pull-policy rebase &&
    git repo-config --list &&
    stg new c1 -m c1 &&
    echo a > file && stg add file && stg refresh
    '

test_expect_success \
    'Add non-rewinding commit in parent and pull the stack' \
    '
    stg branch parent && stg new u1 -m u1 &&
    echo b > file2 && stg add file2 && stg refresh &&
    stg branch stack && stg pull &&
    test -e file2
    '

test_expect_success \
    'Rewind/rewrite commit in parent and pull the stack' \
    '
    stg branch parent && echo b >> file2 && stg refresh &&
    stg branch stack && stg pull &&
    test `wc -l <file2` = 2
    '

# this one exercises the guard against commits
# (use a new file to avoid mistaking a conflict for a success)
test_expect_success \
    'New commit in parent and commit a patch in stack' \
    '
    stg branch parent && stg new u2 -m u2 &&
     echo c > file3 && stg add file3 && stg refresh &&
    stg branch stack && stg commit && stg new c2 -m c2 &&
     echo a >> file && stg refresh
    '
test_expect_failure \
    'Try to pull/rebase now that stack base has moved' \
    'stg pull'

test_expect_success \
    'Force the pull/rebase, but do not push yet' \
    'stg pull --force --nopush'
test_expect_failure \
    '...check we lost the committed patch' \
    'test -e file'
test_expect_failure \
    '...and check we get a conflict while pushing' \
    'stg push'

test_done
