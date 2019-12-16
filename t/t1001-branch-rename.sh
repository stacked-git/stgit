#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Branch renames.

Exercises branch renaming commands.
'

. ./test-lib.sh

test_expect_success \
    'Create an stgit branch from scratch' '
    stg init &&
    stg branch -c foo &&
    stg new p1 -m "p1"
'

test_expect_success \
    'Rename a stgit branch' '
    git config --get-regexp branch\\.foo\\. &&
    stg branch -c buz &&
    stg branch -r foo bar &&
    test_expect_code 1 git config --get-regexp branch\\.foo\\. &&
    git config --get-regexp branch\\.bar\\. &&
    test_path_is_file .git/refs/heads/bar &&
    test_path_is_file .git/refs/heads/bar.stgit
'

test_expect_success \
    'Rename the current stgit branch' '
    stg branch bar &&
    git config --get-regexp branch\\.bar\\. &&
    test_expect_code 1 git config --get-regexp branch\\.foo\\. &&
    stg branch -r foo &&
    git config --get-regexp branch\\.foo\\. &&
    test_path_is_file .git/refs/heads/foo &&
    test_path_is_file .git/refs/heads/foo.stgit &&
    test_expect_code 1 git config --get-regexp branch\\.bar\\. &&
    test_path_is_missing .git/refs/heads/bar &&
    test_path_is_missing .git/refs/heads/bar.stgit &&
    test "$(stg series --noprefix --applied)" = "p1" &&
    test "$(git symbolic-ref --short HEAD)" = "foo"
'

test_expect_success \
    'Rename the current stgit branch single arg' '
    stg branch -r xxx &&
    test "$(stg branch)" = "xxx"
'

test_expect_success \
    'Invalid num args to rename' '
    command_error stg branch --rename bar biz bop 2>&1 |
    grep "incorrect number of arguments"
'

test_done
