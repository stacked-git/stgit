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
    git config branch.stack.stgit.pull-policy rebase &&
    git config --list &&
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

test_done
