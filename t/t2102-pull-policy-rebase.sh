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

if test -z "$STG_RUST"; then
test_expect_success \
    'Test invalid remote argument' \
    '
    command_error stg pull origin 2>err &&
    grep "specifying a repository is meaningless for policy=\"rebase\"" err
    '
else
test_expect_success \
    'Test invalid remote argument' \
    '
    command_error stg pull origin 2>err &&
    grep "specifying a repository is meaningless for \`rebase\` pull-policy" err
    '
fi

if test -z "$STG_RUST"; then
test_expect_success \
    'Test invalid arguments' \
    '
    command_error stg pull origin master 2>err &&
    grep "incorrect number of arguments" err
    '
else
test_expect_success \
    'Test invalid arguments' \
    '
    general_error stg pull origin master 2>err &&
    grep "Found argument .master. which wasn.t expected" err
    '
fi

test_expect_success \
    'Add non-rewinding commit in parent and pull the stack' \
    '
    stg branch parent && stg new u1 -m u1 &&
    echo b > file2 && stg add file2 && stg refresh &&
    stg branch stack && stg pull &&
    test_path_is_file file2
    '

test_expect_success \
    'Rewind/rewrite commit in parent and pull the stack' \
    '
    stg branch parent && echo b >> file2 && stg refresh &&
    stg branch stack && stg pull &&
    test_line_count = 2 file2
    '

test_done
